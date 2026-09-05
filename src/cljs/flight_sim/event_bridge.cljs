(ns flight-sim.event-bridge
  (:require [clojure.string :as string]
            [dao.gui.event :as event]
            [dao.stream :as ds]
            [dao.stream.ringbuffer :as rb]))

(defonce ^:private runtime-input-stream
  (ds/open! {:dao.stream/type :ringbuffer
             :capacity 2048
             :eviction-policy :evict-oldest}))
(def ^:private output-keys [:effects :trace :pointer :keyboard :gesture :dispatch :diagnostic])
(defonce ^:private binding* (atom nil))
(defonce ^:private outputs* (atom nil))
(defonce ^:private cursors* (atom {}))
(defonce ^:private runtime-seq* (atom -1))
(defonce ^:private runtime-time* (atom -1))
(defonce ^:private keyboard-seq* (atom -1))
(defonce ^:private generation* (atom 1))
(defonce ^:private frame-id* (atom 1))
(defonce ^:private coordinate-space-id* (atom 1))
(defonce ^:private viewport* (atom [1.0 1.0]))
(defonce ^:private keyboard-consumer* (atom nil))
(defonce ^:private blur-consumer* (atom nil))
(defonce ^:private listeners* (atom nil))
(defonce ^:private resize-observer* (atom nil))
(defonce ^:private canvas* (atom nil))

(defn- open-output-streams []
  (into {} (map (fn [k] [k (ds/open! {:dao.stream/type :ringbuffer :capacity 256 :eviction-policy :evict-oldest})]) output-keys)))

(defn- drain-output! [k stream]
  (loop [cursor (get @cursors* k {:position 0})]
    (let [read (ds/next stream cursor)]
      (cond
        (map? read) (do (when (and (= k :keyboard) @keyboard-consumer*) (@keyboard-consumer* (:ok read)))
                        (recur (:cursor read)))
        (= :daostream/gap read) (swap! cursors* assoc k {:position (rb/tail-position stream)})
        :else (swap! cursors* assoc k cursor)))))

(defn- advance! []
  (when-let [binding @binding*]
    (loop [b binding n 0]
      (if (>= n 128)
        (reset! binding* b)
        (let [{next-binding :binding status :status} (event/advance b)]
          (if (= status :advanced) (recur next-binding (inc n)) (reset! binding* next-binding)))))
    (doseq [[k stream] @outputs*] (drain-output! k stream))))

(defn- append-runtime! [source value]
  (let [seq (swap! runtime-seq* inc)
        time-us (swap! runtime-time* #(max (inc %) (* 1000 (js/Date.now))))
        envelope {:runtime/seq seq :runtime/time-us time-us :runtime/source source :runtime/value value}]
    (when (= :ok (:result (ds/append! runtime-input-stream envelope))) (advance!))))

(defn- profile-message []
  {:message/kind :dao.terminal/input-profile :generation-id @generation* :profile-id 1
   :capabilities #{:keyboard :mouse}
   :thresholds {:motion/slop 18.0 :tap/max-duration-us 300000 :multi-tap/max-delay-us 300000
                :multi-tap/slop 100.0 :long-press/delay-us 500000 :swipe/min-distance 48.0
                :swipe/max-duration-us 500000 :swipe/min-velocity 500.0 :fling/min-velocity 50.0
                :fling/max-velocity 8000.0 :velocity/window-us 100000 :edge/width 20.0
                :pressure/start-threshold 0.5 :pressure/release-threshold 0.5}})

(defn- geometry-message [[w h]]
  {:message/kind :dao.terminal/presented-geometry :generation-id @generation* :frame-id @frame-id*
   :coordinate-space-id @coordinate-space-id*
   :nodes [{:node-id ::flight-surface
            :interaction/path [{:node-id ::flight-surface :recognizers [] :touch-action :none}]
            :touch-action :none :regions [{:bounds {:x 0.0 :y 0.0 :width w :height h} :paint-order 0}]}]})

(defn- coordinate-message [[w h]]
  {:message/kind :dao.terminal/coordinate-space-change :generation-id @generation*
   :coordinate-space-id @coordinate-space-id* :viewport {:width w :height h}})

(defn- keyboard-subscription []
  {:subscription/op :add :subscription/id "flight-keyboard" :subscriber/id ::flight-controller
   :node-id ::flight-surface :event-kind :keyboard :keyboard/phases #{:down :up :cancel}})

(defn- boot-runtime! []
  (append-runtime! :terminal (coordinate-message @viewport*))
  (append-runtime! :geometry (geometry-message @viewport*))
  (append-runtime! :profile (profile-message))
  (append-runtime! :subscription (keyboard-subscription)))

(defn- keyboard-code [code]
  (-> (str code) (string/replace #"([a-z0-9])([A-Z])" "$1-$2") string/lower-case keyword))

(defn- modifiers [^js e]
  (cond-> #{} (.-altKey e) (conj :alt) (.-ctrlKey e) (conj :control)
    (.-metaKey e) (conj :meta) (.-shiftKey e) (conj :shift)))

(defn- keyboard-packet [^js e phase]
  {:input/kind :keyboard :generation-id @generation* :input-seq (swap! keyboard-seq* inc)
   :time-us (js/Math.floor (* 1000 (.-timeStamp e))) :phase phase :focus-id nil
   :repeat? (boolean (.-repeat e)) :modifiers (modifiers e)
   :key {:code (keyboard-code (.-code e)) :logical (.-key e) :location :standard}})

(def ^:private captured-codes
  #{"KeyW" "KeyS" "KeyA" "KeyD" "KeyF" "KeyR" "KeyB" "KeyG" "KeyV" "KeyC" "KeyP" "KeyT" "KeyM"
    "Digit1" "Digit2" "Digit3" "Digit4" "Digit5" "Numpad5" "ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" "Space"})

(defn- install-keyboard-listeners! []
  (when-let [old @listeners*] (doseq [[kind handler] old] (.removeEventListener js/window kind handler)))
  (let [emit (fn [^js e phase]
               (when (contains? captured-codes (.-code e)) (.preventDefault e))
               (append-runtime! :keyboard (keyboard-packet e phase)))
        handlers {"keydown" #(emit % :down) "keyup" #(emit % :up)
                  "blur" (fn [_] (when @blur-consumer* (@blur-consumer*)))}]
    (doseq [[kind handler] handlers] (.addEventListener js/window kind handler))
    (reset! listeners* handlers)))

(defn- canvas-size [^js canvas]
  [(max 1.0 (double (.-clientWidth canvas))) (max 1.0 (double (.-clientHeight canvas)))])

(defn- report-resize! [size]
  (when (not= size @viewport*)
    (reset! viewport* size) (swap! frame-id* inc) (swap! coordinate-space-id* inc)
    (append-runtime! :terminal (coordinate-message size))
    (append-runtime! :geometry (geometry-message size))))

(defn attach-canvas! [canvas]
  (when-let [observer @resize-observer*] (.disconnect observer) (reset! resize-observer* nil))
  (reset! canvas* canvas)
  (when canvas
    (report-resize! (canvas-size canvas))
    (when (exists? js/ResizeObserver)
      (let [observer (js/ResizeObserver. (fn [_] (report-resize! (canvas-size canvas))))]
        (.observe observer canvas) (reset! resize-observer* observer)))))

(defn start!
  ([on-keyboard] (start! on-keyboard nil))
  ([on-keyboard on-blur]
   (reset! keyboard-consumer* on-keyboard) (reset! blur-consumer* on-blur)
   (reset! outputs* (open-output-streams)) (reset! cursors* {})
   (reset! binding* (event/bind {:inputs {:runtime-input runtime-input-stream} :outputs @outputs*}))
   (boot-runtime!) (install-keyboard-listeners!) :started))

(defn stop! []
  (when-let [old @listeners*] (doseq [[kind handler] old] (.removeEventListener js/window kind handler)))
  (reset! listeners* nil)
  (when-let [observer @resize-observer*] (.disconnect observer))
  (reset! resize-observer* nil) (reset! keyboard-consumer* nil) (reset! blur-consumer* nil) :stopped)

(defn viewport [] @viewport*)
