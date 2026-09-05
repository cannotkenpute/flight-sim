(ns flight-sim.app
  (:require [clojure.string :as string]
            [dao.postgraphics.terminal :as terminal]
            [dao.postgraphics.web :as pg]
            [dao.stream :as ds]
            [dao.stream.ringbuffer]
            [flight-sim.audio :as audio]
            [flight-sim.event-bridge :as input]
            [flight-sim.flight :as flight]
            [flight-sim.geometry :as geo]
            [flight-sim.gltf :as gltf]
            [flight-sim.heightfield :as hf]
            [flight-sim.math :as m]
            [flight-sim.radar :as radar]
            [flight-sim.scene :as scene]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(def fixed-dt (/ 1.0 60.0))
(def aircraft-order [:prop :jet :b747])
(def spd-px 2.6)
(def alt-px 0.55)
(def hdg-px 5.0)
(def ppd 6.5)

(defonce frame-stream
  (ds/open! {:dao.stream/type :ringbuffer
             :capacity 4
             :eviction-policy :evict-oldest}))

(defn- initial-app-state []
  (let [f (flight/initial-state hf/height-at :prop)
        [x _ z] (:pos f)
        [cx cz] (geo/terrain-key x z)]
    {:flight f
     :terrain-key [cx cz]
     :terrain-mesh (geo/build-terrain-mesh cx cz)
     :viewport [1280.0 720.0]
     :camera-mode :chase
     :orbit-angle 0.0
     :paused false
     :hud-hidden false
     :wireframe? false
     :muted? false
     :message "Throttle up with W — rotate with ↑ near takeoff speed — V swaps aircraft, C camera"
     :message-until (+ (js/Date.now) 9000)
     :terrain-threat 0.0
     :webgpu? (pg/gpu-available?)
     :render-error nil
     :aircraft-assets {}
     :asset-status {:prop :loading}
     :sim-time 0.0
     :prop-angle 0.0
     :fan-angle 0.0
     :refueling? false
     :events []}))

(defonce app-state (r/atom (initial-app-state)))
(defonce keys-down* (atom #{}))
(defonce raf-id* (atom nil))
(defonce last-ms* (atom nil))
(defonce accumulator* (atom 0.0))
(defonce threat-clock* (atom 0.0))
(defonce asset-load-started?* (atom false))

(defn- message! [text ms]
  (swap! app-state assoc
         :message text
         :message-until (if (pos? ms) (+ (js/Date.now) ms) ##Inf)))

(defn- next-aircraft [k]
  (let [i (or (first (keep-indexed #(when (= %2 k) %1) aircraft-order)) 0)]
    (nth aircraft-order (mod (inc i) (count aircraft-order)))))

(defn- set-camera! [mode]
  (swap! app-state assoc :camera-mode mode)
  (message! (str (string/upper-case (name mode)) " CAM") 1400))

(defn- cycle-camera! []
  (let [current (:camera-mode @app-state)
        idx (or (first (keep-indexed #(when (= %2 current) %1) scene/camera-modes)) 0)]
    (set-camera! (nth scene/camera-modes (mod (inc idx) (count scene/camera-modes))))))

(defn- reset-flight! []
  (swap! app-state
         (fn [state]
           (-> state
               (update :flight #(flight/reset-state hf/height-at %))
               (assoc :terrain-threat 0.0 :refueling? false :events []))))
  (message! "AIRCRAFT RESET" 1600))

(defn- toggle-aircraft! []
  (let [f (:flight @app-state)
        next-k (next-aircraft (:spec-key f))]
    (swap! app-state update :flight #(flight/switch-aircraft hf/height-at % next-k))
    (let [name (get-in @app-state [:flight :spec :name])
          asset-status (get-in @app-state [:asset-status next-k])]
      (message! (str "AIRFRAME: " name
                     (when (and (= next-k :prop) (= asset-status :loading)) " — LOADING"))
                1800))))

(defn- gear-toggle! []
  (let [before (:flight @app-state)
        old-target (:gear-target before)]
    (swap! app-state update :flight flight/toggle-gear)
    (let [after (:flight @app-state)
          new-target (:gear-target after)]
      (if (= old-target new-target)
        (message! "GEAR LOCKED — WEIGHT ON WHEELS" 1800)
        (let [down? (pos? new-target)]
          (message! (if down? "GEAR DOWN" "GEAR UP") 1500)
          (audio/speak! (if down? "Gear down." "Gear up.") 900))))))

(defn- handle-one-shot! [code]
  (case code
    :key-f (do (swap! app-state update :flight flight/adjust-flaps 1)
               (message! (str "FLAPS " (js/Math.round (* 100 (get-in @app-state [:flight :flaps]))) "%") 1000))
    :key-r (do (swap! app-state update :flight flight/adjust-flaps -1)
               (message! (str "FLAPS " (js/Math.round (* 100 (get-in @app-state [:flight :flaps]))) "%") 1000))
    :key-g (gear-toggle!)
    :key-v (toggle-aircraft!)
    :key-c (cycle-camera!)
    :digit1 (set-camera! :chase)
    :digit2 (set-camera! :cockpit)
    :digit3 (set-camera! :tower)
    :digit4 (set-camera! :orbit)
    :digit5 (swap! app-state update :hud-hidden not)
    :numpad5 (swap! app-state update :hud-hidden not)
    :key-t (do (swap! app-state update :wireframe? not)
               (message! (if (:wireframe? @app-state) "WIREFRAME ON" "WIREFRAME OFF") 1200))
    :key-m (let [muted? (audio/toggle-mute!)]
             (swap! app-state assoc :muted? muted?)
             (message! (if muted? "AUDIO MUTED" "AUDIO ON") 1200))
    :key-p (do (swap! app-state update :paused not)
               (message! (if (:paused @app-state) "PAUSED" "RESUMED") 1200))
    :space (reset-flight!)
    nil))

(defn- keyboard-event! [event]
  ;; This callback is reached synchronously from the native key event after
  ;; dao.gui.event has interpreted the packet, so it still satisfies browser
  ;; autoplay's user-activation requirement.
  (audio/unlock!)
  (let [phase (:event/phase event)
        code (get-in event [:key :code])
        repeat? (:repeat? event)]
    (case phase
      :down (do (swap! keys-down* conj code)
                (when-not repeat? (handle-one-shot! code)))
      :up (swap! keys-down* disj code)
      nil)))

(defn- clear-held-controls! []
  (reset! keys-down* #{}))

(defn- apply-held-controls [f dt]
  (let [k @keys-down*
        throttle (cond
                   (contains? k :key-w) (min 1.0 (+ (:throttle f) (* 0.55 dt)))
                   (contains? k :key-s) (max 0.0 (- (:throttle f) (* 0.55 dt)))
                   :else (:throttle f))
        pitch (- (if (contains? k :arrow-up) 1.0 0.0)
                 (if (contains? k :arrow-down) 1.0 0.0))
        roll (- (if (contains? k :arrow-right) 1.0 0.0)
                (if (contains? k :arrow-left) 1.0 0.0))
        yaw (- (if (contains? k :key-d) 1.0 0.0)
               (if (contains? k :key-a) 1.0 0.0))]
    (assoc f
           :throttle throttle
           :brake (contains? k :key-b)
           :input {:pitch pitch :roll roll :yaw yaw})))

(defn- terrain-threat [f]
  (let [{:keys [forward]} (flight/axes f)
        [fx _ fz] forward
        horiz (m/hypot2 fx fz)]
    (if (or (< horiz 0.15) (:on-ground f) (<= (flight/speed f) 45.0))
      0.0
      (let [dx (/ fx horiz)
            dz (/ fz horiz)
            spd (max 1.0 (flight/speed f))
            [_ vy _] (:vel f)
            slope (/ vy spd)
            look-dist (min (+ (* spd 3.5) 150.0) 4000.0)
            [px py pz] (:pos f)]
        (loop [d 30.0]
          (if (> d look-dist)
            0.0
            (let [h (hf/height-at (+ px (* dx d)) (+ pz (* dz d)))]
              (if (>= h (- (+ py (* slope d)) 15.0))
                d
                (recur (+ d 30.0))))))))))

(defn- maybe-rebuild-terrain [state]
  (let [[x _ z] (get-in state [:flight :pos])
        key (geo/terrain-key x z)]
    (if (= key (:terrain-key state))
      state
      (let [[cx cz] key]
        (assoc state
               :terrain-key key
               :terrain-mesh (geo/build-terrain-mesh cx cz))))))

(defn- refuel-step [f dt]
  (let [[x _ z] (:pos f)
        nr (hf/nearest-runway x z)
        cap (double (get-in f [:spec :fuel-cap]))
        before (double (:fuel f))
        refueling? (and (:on-ground f)
                        (< (:d nr) 3.0)
                        (< (flight/speed f) 5.0)
                        (< before cap))
        after (if refueling? (min cap (+ before (* cap 0.07 dt))) before)]
    [(assoc f :fuel after)
     refueling?
     (and refueling? (< before cap) (>= after cap))]))

(defn- physics-step [state]
  (if (:paused state)
    state
    (let [f0 (apply-held-controls (:flight state) fixed-dt)
          had-fuel? (pos? (:fuel f0))
          f1 (flight/step hf/height-at f0 fixed-dt)
          crashed? (:crashed f1)
          flameout? (and had-fuel? (<= (:fuel f1) 0.0))
          [f2 refueling? fuel-full?]
          (if crashed?
            [(flight/reset-state hf/height-at f1) false false]
            (refuel-step f1 fixed-dt))
          refuel-start? (and refueling? (not (:refueling? state)))
          events (cond-> []
                   flameout? (conj :flameout)
                   crashed? (conj :crash)
                   refuel-start? (conj :refuel-start)
                   fuel-full? (conj :fuel-full))]
      (-> state
          (assoc :flight f2
                 :refueling? refueling?
                 :events (into (:events state) events))
          (update :sim-time + fixed-dt)
          (update :prop-angle + (* (+ 2.0 (* (:throttle f2) 70.0)) fixed-dt))
          (update :fan-angle + (* (+ 18.0 (* (:throttle f2) 55.0)) fixed-dt))))))

(defn- handle-runtime-events! []
  (let [events (:events @app-state)]
    (when (seq events)
      (swap! app-state assoc :events []))
    (doseq [event events]
      (case event
        :flameout (do (message! "ENGINE FLAMEOUT — FUEL EXHAUSTED" 5000)
                      (audio/speak! "Fuel exhausted." 3000))
        :crash (do (message! "CRASHED — RESETTING" 2200)
                   (audio/crash!)
                   (audio/speak! "Aircraft destroyed." 500))
        :refuel-start (audio/speak! "Refueling." 1400)
        :fuel-full (do (message! (str "FUEL FULL — " (get-in @app-state [:flight :spec :fuel-cap]) " GAL") 2600)
                       (audio/speak! "Fuel full." 1400))
        nil))))

(defn- update-threat! [render-dt]
  (swap! threat-clock* + render-dt)
  (when (>= @threat-clock* 0.25)
    (reset! threat-clock* 0.0)
    (let [f (:flight @app-state)
          threat (if (:paused @app-state) 0.0 (terrain-threat f))
          stall? (and (not (:paused @app-state))
                      (not (:on-ground f))
                      (< (flight/speed f) (+ (get-in f [:spec :stall]) 3.0)))]
      (swap! app-state assoc :terrain-threat threat)
      (when (pos? threat)
        (when-not (audio/play-once! :pullup 4500 0.9)
          (audio/speak! "Terrain! Terrain! Pull up!" 4500)))
      (when stall?
        (when-not (audio/play-once! :stall 6000 0.9)
          (audio/speak! "Stall!" 3500))))))

(defn- emit-frame! []
  (swap! app-state assoc :viewport (input/viewport))
  (terminal/put-frame! frame-stream (scene/frame-from-state @app-state)))

(defn- update-message-expiry! []
  (let [until (:message-until @app-state)]
    (when (and (number? until) (< until (js/Date.now)))
      (swap! app-state assoc :message nil :message-until nil))))

(defn- animation-frame! [ms]
  (let [last @last-ms*
        render-dt (if last (m/clamp (/ (- ms last) 1000.0) 0.0 0.1) fixed-dt)]
    (reset! last-ms* ms)
    (swap! accumulator* + render-dt)
    (loop [steps 0]
      (when (and (>= @accumulator* fixed-dt) (< steps 6))
        (swap! accumulator* - fixed-dt)
        (swap! app-state physics-step)
        (recur (inc steps))))
    (handle-runtime-events!)
    (swap! app-state maybe-rebuild-terrain)
    (when (and (not (:paused @app-state)) (= :orbit (:camera-mode @app-state)))
      (swap! app-state update :orbit-angle + (* render-dt 0.35)))
    (update-threat! render-dt)
    (update-message-expiry!)
    (let [f (:flight @app-state)
          [x _ z] (:pos f)
          nr (hf/nearest-runway x z)]
      (audio/update-engine! (:spec-key f) (if (pos? (:fuel f)) (:throttle f) 0.0) (flight/speed f))
      (radar/update! f nr render-dt))
    (emit-frame!)
    (reset! raf-id* (js/requestAnimationFrame animation-frame!))))

(defn- load-piper-asset! []
  (when-not @asset-load-started?*
    (reset! asset-load-started?* true)
    (swap! app-state assoc-in [:asset-status :prop] :loading)
    (gltf/load-glb!
      "/assets/models/piper_hawk.glb"
      (fn [asset]
        (swap! app-state
               (fn [state]
                 (-> state
                     (assoc-in [:aircraft-assets :prop] asset)
                     (assoc-in [:asset-status :prop] :ready))))
        (when (= :prop (get-in @app-state [:flight :spec-key]))
          (message! "PIPER ASSET READY" 1400)))
      (fn [error]
        (js/console.warn "Piper GLB load failed; native procedural fallback remains active" error)
        (swap! app-state assoc-in [:asset-status :prop] :failed)
        (when (= :prop (get-in @app-state [:flight :spec-key]))
          (message! "PIPER ASSET FAILED — PROCEDURAL AIRFRAME ACTIVE" 4200))))))

(defn start! []
  (when-not @raf-id*
    (audio/init!)
    (input/start! keyboard-event! clear-held-controls!)
    (load-piper-asset!)
    (reset! last-ms* nil)
    (reset! accumulator* 0.0)
    (reset! threat-clock* 0.0)
    (emit-frame!)
    (reset! raf-id* (js/requestAnimationFrame animation-frame!)))
  :started)

(defn stop! []
  (when-let [id @raf-id*]
    (js/cancelAnimationFrame id))
  (reset! raf-id* nil)
  (reset! keys-down* #{})
  (audio/update-engine! :prop 0.0 0.0)
  (input/stop!)
  :stopped)

(defn- format-endurance [f]
  (let [seconds (flight/endurance f)]
    (cond
      (<= (:fuel f) 0.0) "FLAMEOUT"
      (= seconds ##Inf) "--"
      (> seconds 3599.0) "--"
      :else (let [mins (long (js/Math.floor (/ seconds 60.0)))
                  secs (long (js/Math.floor (mod seconds 60.0)))]
              (str mins ":" (.padStart (str secs) 2 "0"))))))

(defn- heading-label [deg]
  (case (mod (long deg) 360)
    0 "N"
    90 "E"
    180 "S"
    270 "W"
    (.padStart (str (long (/ (mod (long deg) 360) 10))) 2 "0")))

(defn- speed-tape [speed-kt]
  (let [center (* 10 (long (js/Math.round (/ speed-kt 10.0))))]
    [:div#spd-tape
     [:div#spd-ticks
      (for [kt (range (- center 60) (+ center 70) 10)
            :when (and (>= kt 0) (<= kt 1000))]
        ^{:key kt}
        [:div.vtick
         {:class (when (zero? (mod kt 50)) "major")
          :style {:top (str (+ 140.0 (* (- kt speed-kt) spd-px)) "px")}}
         (when (zero? (mod kt 50)) [:span kt])])]]))

(defn- altitude-tape [alt-ft]
  (let [center (* 100 (long (js/Math.round (/ alt-ft 100.0))))]
    [:div#alt-tape
     [:div#alt-ticks
      (for [ft (range (- center 1000) (+ center 1100) 100)
            :when (and (>= ft -1000) (<= ft 35000))]
        ^{:key ft}
        [:div.vtick
         {:class (when (zero? (mod ft 500)) "major")
          :style {:top (str (+ 140.0 (* (- ft alt-ft) alt-px)) "px")}}
         (when (zero? (mod ft 500)) [:span ft])])]]))

(defn- heading-tape [heading]
  (let [center (* 5 (long (js/Math.round (/ heading 5.0))))]
    [:div#hdg-tape
     [:div#hdg-ticks
      (for [raw (range (- center 50) (+ center 55) 5)
            :let [deg (mod raw 360)
                  offset (- raw heading)]]
        ^{:key raw}
        [:div.htick
         {:class (when (zero? (mod deg 10)) "major")
          :style {:left (str (* offset hdg-px) "px")}}
         (when (zero? (mod deg 10)) [:span (heading-label deg)])])]]))

(defn- attitude-view [f]
  (let [roll-deg (m/deg (:roll f))
        pitch-deg (m/deg (:pitch f))
        speed (flight/speed f)
        [vx vy vz] (:vel f)
        {:keys [forward]} (flight/axes f)
        fpm
        (when (and (not (:on-ground f)) (> speed 15.0))
          (let [d-pitch (m/deg (- (m/asin (m/clamp (/ vy speed) -1.0 1.0))
                                  (m/asin (m/clamp (nth forward 1) -1.0 1.0))))
                v-hdg (mod (+ (m/deg (m/atan2 (- vx) (- vz))) 360.0) 360.0)
                nose-hdg (flight/heading-deg f)
                raw (- v-hdg nose-hdg)
                d-hdg (cond (> raw 180.0) (- raw 360.0)
                            (< raw -180.0) (+ raw 360.0)
                            :else raw)]
            [(* d-hdg ppd) (* (- d-pitch) ppd)]))]
    [:div#adi
     [:div#adi-inner
      {:style {:transform (str "rotate(" (.toFixed (- roll-deg) 1) "deg) translateY("
                               (.toFixed (* pitch-deg ppd) 1) "px)")}}
      [:div.ladder-line.horizon]
      (for [d [10 -10 20 -20 30 -30 -45]]
        ^{:key d}
        [:div.ladder-line
         {:class (when (>= (m/abs d) 30) "short")
          :style {:transform (str "translateY(" (* d -6.5) "px)")}}
         [:span d]])]
     [:div#roll-scale
      (for [a [-60 -45 -30 -20 -10 0 10 20 30 45 60]]
        ^{:key a}
        [:div.rtick
         {:class (when (zero? (mod a 30)) "major")
          :style {:transform (str "rotate(" a "deg) translateY(-120px)")}}])]
     [:div#roll-pointer {:style {:transform (str "rotate(" (.toFixed (- roll-deg) 1) "deg)")}}]
     [:div#adi-marker]
     (when fpm
       [:div#fpm
        {:style {:display "block"
                 :transform (str "translate(" (.toFixed (first fpm) 1) "px,"
                                 (.toFixed (second fpm) 1) "px)")}}])]))

(defn- runway-text [nr]
  (if (< (:d nr) 150.0)
    "ON RUNWAY"
    (str (.toFixed (/ (:d nr) 1000.0) 1) " KM")))

(defn- hud-view []
  (let [{:keys [flight camera-mode paused hud-hidden message terrain-threat webgpu?
                refueling? muted? asset-status]} @app-state
        speed (flight/speed flight)
        speed-kt (* speed 1.94384)
        [_ y _] (:pos flight)
        alt-ft (* y 3.28084)
        agl-ft (* (max 0.0 (flight/agl hf/height-at flight)) 3.28084)
        [_ vy _] (:vel flight)
        vsi (* vy 196.85)
        hdg (flight/heading-deg flight)
        fuel-frac (flight/fuel-fraction flight)
        [x _ z] (:pos flight)
        nr (hf/nearest-runway x z)
        runway (runway-text nr)
        stall? (and (not (:on-ground flight)) (< speed (+ (get-in flight [:spec :stall]) 4.0)))
        rotate? (and (:on-ground flight) (flight/can-rotate? flight))
        low-fuel? (and (< fuel-frac 0.1) (pos? (:fuel flight)))
        gear-locked? (= (:gear flight) (:gear-target flight))
        refuel-message (when refueling?
                         (str "REFUELING — " (.toFixed (:fuel flight) 0) " / "
                              (get-in flight [:spec :fuel-cap]) " GAL"))]
    [:div#hud {:class (when hud-hidden "hud-hidden")}
     [:div#cam-label
      (str (string/upper-case (name camera-mode)) " CAM"
           (when paused " · PAUSED")
           (when muted? " · MUTED"))]

     [:div#warnings
      (when stall? [:div.warning-live "STALL"])
      (when (pos? terrain-threat) [:div.warning-live "PULL UP"])
      (when low-fuel? [:div.warning-live "LOW FUEL"])
      (when rotate? [:div.warning-live.rotate "ROTATE"])
      (when-let [text (or refuel-message message)] [:div#message text])]

     [heading-tape hdg]
     [:div#hdg-box (.padStart (.toFixed hdg 0) 3 "0")]

     [speed-tape speed-kt]
     [:div#spd-box [:span (.toFixed speed-kt 0)] [:span.unit "KT"]]

     [altitude-tape alt-ft]
     [:div#alt-box [:span (.toFixed alt-ft 0)] [:span.unit "FT"]]
     [:div#vsi-box "VS " [:span (str (when (>= vsi 0.0) "+") (.toFixed vsi 0))] " FPM"]
     [:div#agl-box "AGL " [:span (.toFixed agl-ft 0)] " FT"]

     [attitude-view flight]

     [:div#flight-data
      [:div.row.ext [:span.lbl "SPD"] [:span (str (.toFixed speed-kt 0) " KT")]]
      [:div.row.ext [:span.lbl "ALT"] [:span (str (.toFixed alt-ft 0) " FT")]]
      [:div.row.ext [:span.lbl "HDG"] [:span (.padStart (.toFixed hdg 0) 3 "0")]]
      [:div.row.ext [:span.lbl "VS"] [:span (str (when (>= vsi 0.0) "+") (.toFixed vsi 0) " FPM")]]
      [:div.row.ext [:span.lbl "AGL"] [:span (str (.toFixed agl-ft 0) " FT")]]
      [:div.row.ext [:span.lbl "NEAREST RWY"] [:span runway]]
      [:div.sep]
      [:div.row [:span.lbl "AIRFRAME"] [:span (get-in flight [:spec :name])]]
      [:div.row [:span.lbl "MACH"] [:span (.toFixed (/ speed 343.0) 2)]]
      [:div.row [:span.lbl "G"] [:span (.toFixed (:g-load flight) 1)]]
      [:div.row [:span.lbl "THR"] [:span (str (if (pos? (:fuel flight))
                                                (js/Math.round (* 100 (:throttle flight))) 0) "%")]]
      [:div#thr-track [:div#hud-thr-bar {:style {:width (str (* 100 (if (pos? (:fuel flight)) (:throttle flight) 0.0)) "%")}}]]
      [:div.row [:span.lbl "FLAPS"] [:span (str (js/Math.round (* 100 (:flaps flight))) "%")]]
      [:div.row [:span.lbl "GEAR"]
       [:span {:style {:color (when-not (and gear-locked? (pos? (:gear-target flight))) "#ffd97a")}}
        (if gear-locked? (if (pos? (:gear-target flight)) "DOWN" "UP") "IN TRANSIT")]]
      [:div.fuel-head [:span.lbl "FUEL"]
       [:span (str (.toFixed (:fuel flight) 0) " / " (get-in flight [:spec :fuel-cap]) " GAL")]]
      [:div#fuel-track
       [:div#fuel-bar
        {:class (cond (< fuel-frac 0.05) "critical" (< fuel-frac 0.15) "low" :else nil)
         :style {:width (str (* 100 fuel-frac) "%")}}]
       [:div#fuel-LOW]]
      [:div.row.small [:span.lbl "ENDURANCE"] [:span (format-endurance flight)]]
      [:div.row.small.system-line
       [:span.lbl "RENDER"] [:span (if webgpu? "WEBGPU" "SOFTWARE FALLBACK")]]
      (when (= :prop (:spec-key flight))
        [:div.row.small.system-line
         [:span.lbl "PIPER MESH"]
         [:span (string/upper-case (name (get asset-status :prop :unknown)))]] )]

     [:canvas#terrain-map {:width 192 :height 192 :ref radar/attach-canvas!}]
     [:div#rwy-label (str "RWY " runway)]

     [:div#controls-panel
      [:div [:kbd "W"] "/" [:kbd "S"] " throttle"]
      [:div [:kbd "↑"] "/" [:kbd "↓"] " pitch"]
      [:div [:kbd "←"] "/" [:kbd "→"] " roll"]
      [:div [:kbd "A"] "/" [:kbd "D"] " rudder"]
      [:div [:kbd "F"] "/" [:kbd "R"] " flaps ±"]
      [:div [:kbd "B"] " brakes"]
      [:div [:kbd "G"] " landing gear"]
      [:div [:kbd "M"] " mute"]
      [:div [:kbd "V"] " swap aircraft"]
      [:div [:kbd "C"] " / " [:kbd "1-4"] " camera"]
      [:div [:kbd "5"] " HUD overlay on/off"]
      [:div [:kbd "T"] " wireframe"]
      [:div [:kbd "Space"] " reset"]
      [:div [:kbd "P"] " pause"]
      [:div.hint "land + stop on a runway to refuel"]]]))

(defn- canvas-view []
  (r/create-class
    {:display-name "flight-postgraphics-canvas"
     :component-did-mount (fn [_] (start!))
     :component-will-unmount (fn [_] (stop!))
     :reagent-render
     (fn []
       [:div.sim-canvas-wrap
        [pg/postgraphics-widget
         frame-stream
         :canvas-attrs {:aria-label "Datom.World PostGraphics flight simulator"}
         :canvas-ref input/attach-canvas!
         :viewport-size input/viewport
         :resolve-resource (fn [source _state] source)
         :on-error #(swap! app-state assoc :render-error (str %))]])}))

(defn root-view []
  [:div.sim-shell
   [canvas-view]
   [hud-view]
   (when-let [err (:render-error @app-state)]
     [:div.error-banner
      [:strong "PostGraphics frame rejected: "] err])
   (when-not (:webgpu? @app-state)
     [:div.backend-warning "WebGPU unavailable — Datom.World software renderer active"])] )

(defn init []
  (when-let [app (.getElementById js/document "app")]
    (rdom/render [root-view] app)))
