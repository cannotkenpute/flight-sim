(ns flight-sim.scene
  (:require [clojure.string :as string]
            [flight-sim.flight :as flight]
            [flight-sim.geometry :as geo]
            [flight-sim.heightfield :as hf]
            [flight-sim.math :as m]))

(def camera-modes [:chase :cockpit :tower :orbit])

(def camera-config
  {:prop {:cockpit {:up 1.1 :fwd 1.8} :chase 24.0 :chase-up 7.0 :orbit 42.0 :orbit-up 14.0}
   :jet {:cockpit {:up 1.2 :fwd 2.4} :chase 24.0 :chase-up 7.0 :orbit 42.0 :orbit-up 14.0}
   :b747 {:cockpit {:up 8.55 :fwd 25.8} :chase 105.0 :chase-up 31.0 :orbit 135.0 :orbit-up 42.0}})

(defn- clamp-above-terrain [p f]
  (let [[x y z] p
        min-y (+ (hf/height-at x z) (flight/ground-clearance f) 2.0)]
    [x (max y min-y) z]))

(defn camera-pose
  [{:keys [flight camera-mode orbit-angle]}]
  (let [{:keys [forward up]} (flight/axes flight)
        pos (:pos flight)
        cfg (get camera-config (:spec-key flight) (:prop camera-config))
        mode (or camera-mode :chase)]
    (case mode
      :cockpit
      (let [{:keys [up fwd]} (:cockpit cfg)
            body-up (:up (flight/axes flight))
            eye (-> pos
                    (m/v+ (m/v* body-up up))
                    (m/v+ (m/v* forward fwd)))
            target (m/v+ eye (m/v* forward 300.0))]
        {:position eye :rotation (m/look-rotation eye target)})

      :tower
      (let [eye [70.0 30.0 430.0]]
        {:position eye :rotation (m/look-rotation eye pos)})

      :orbit
      (let [a (double (or orbit-angle 0.0))
            eye (clamp-above-terrain
                  [(+ (nth pos 0) (* (m/cos a) (:orbit cfg)))
                   (+ (nth pos 1) (:orbit-up cfg))
                   (+ (nth pos 2) (* (m/sin a) (:orbit cfg)))]
                  flight)]
        {:position eye :rotation (m/look-rotation eye pos)})

      (let [eye (clamp-above-terrain
                  (-> pos
                      (m/v+ (m/v* forward (- (:chase cfg))))
                      (m/v+ [0.0 (:chase-up cfg) 0.0]))
                  flight)
            target (m/v+ pos (m/v* forward 15.0))]
        {:position eye :rotation (m/look-rotation eye target)}))))

(defn aircraft-matrix
  "Column-major local-aircraft -> world transform derived from the same axes as
   the flight model. Local -Z is the nose/forward direction."
  [f]
  (let [{:keys [right up forward]} (flight/axes f)
        [rx ry rz] right
        [ux uy uz] up
        [fx fy fz] forward
        [px py pz] (:pos f)]
    [rx ry rz 0.0
     ux uy uz 0.0
     (- fx) (- fy) (- fz) 0.0
     px py pz 1.0]))

(defn- line-color [mesh]
  (or (:fill mesh)
      (first (:colors mesh))
      [0.78 0.88 0.94 1.0]))

(defn- mesh-op
  ([mesh] (mesh-op mesh false))
  ([mesh wireframe?]
   (if wireframe?
     {:op/kind :draw3d/lines
      :vertices (:vertices mesh)
      :edges (or (:wire-edges mesh) (geo/wire-edges (:indices mesh)))
      :color (line-color mesh)
      :stroke-width 1.0}
     (-> mesh
         (dissoc :wire-edges)
         (assoc :op/kind :draw3d/mesh)))))

(defn- add-rotation [[rx ry rz] [ax ay az]]
  [(+ rx ax) (+ ry ay) (+ rz az)])

(defn- dynamic-component
  [component state]
  (let [f (:flight state)
        tag (:tag component)
        g (double (:gear f))
        retract (- 1.0 g)
        roll-in (double (get-in f [:input :roll] 0.0))
        pitch-in (double (get-in f [:input :pitch] 0.0))
        yaw-in (double (get-in f [:input :yaw] 0.0))
        prop-angle (double (or (:prop-angle state) 0.0))
        fan-angle (double (or (:fan-angle state) 0.0))]
    (case tag
      :prop (update component :rotate add-rotation [0.0 0.0 prop-angle])
      :gear (update component :scale
                    (fn [[sx sy sz]] [sx (* sy (max 0.04 g)) sz]))
      :burner (when (and (> (:throttle f) 0.6) (pos? (:fuel f)))
                (let [power (/ (- (:throttle f) 0.6) 0.4)
                      flicker (+ 1.0 (* 0.25 (m/sin (* (double (or (:sim-time state) 0.0)) 45.0))))]
                  (update component :scale
                          (fn [[_sx _sy _sz]]
                            [(+ 0.7 (* power 0.5))
                             (+ 0.7 (* power 0.5))
                             (* (+ 0.4 power) flicker)]))))
      :flap-l (update component :rotate add-rotation [(- (* (:flaps f) 0.42)) 0.0 0.0])
      :flap-r (update component :rotate add-rotation [(- (* (:flaps f) 0.42)) 0.0 0.0])
      :aileron-l (update component :rotate add-rotation [0.0 0.0 (* roll-in 0.18)])
      :aileron-r (update component :rotate add-rotation [0.0 0.0 (* roll-in -0.18)])
      :elevator (update component :rotate add-rotation [(* pitch-in 0.22) 0.0 0.0])
      :rudder (update component :rotate add-rotation [0.0 (* yaw-in 0.20) 0.0])
      :fan (update component :rotate add-rotation [0.0 0.0 fan-angle])
      :gear-main (-> component
                     (update :translate m/v+ [0.0 (* retract 2.8) (* retract 1.0)])
                     (update :rotate add-rotation [(* retract 0.28) 0.0 0.0]))
      :gear-nose (-> component
                     (update :translate m/v+ [0.0 (* retract 2.8) (* retract 1.0)])
                     (update :rotate add-rotation [(* retract 0.28) 0.0 0.0]))
      :gear-door (update component :rotate add-rotation [(* retract 0.50) 0.0 0.0])
      component)))

(defn- component-ops [component state]
  (when-let [component (dynamic-component component state)]
    (let [wireframe? (:wireframe? state)
          mesh (:mesh component)
          op (cond-> (mesh-op mesh wireframe?)
               (and (= :burner (:tag component)) (not wireframe?))
               (assoc :material/emissive [1.0 0.18 0.02]))]
      [{:op/kind :transform/push
        :translate (or (:translate component) [0.0 0.0 0.0])
        :rotate (or (:rotate component) [0.0 0.0 0.0])
        :scale (or (:scale component) [1.0 1.0 1.0])}
       op
       {:op/kind :transform/pop}])))

(defn- procedural-aircraft-ops [state]
  (let [f (:flight state)
        cockpit? (= :cockpit (:camera-mode state))
        components (get geo/aircraft-components (:spec-key f))
        components (if (and cockpit? (= :b747 (:spec-key f)))
                     (filter :cockpit-visible? components)
                     components)]
    (vec (mapcat #(or (component-ops % state) []) components))))

(defn- gltf-component-tag [name]
  (cond
    (string/includes? name "Prop") :prop
    (string/includes? name "Gear") :gear
    :else nil))

(defn- gltf-component-ops [component state]
  (let [tag (gltf-component-tag (:name component))
        f (:flight state)
        g (double (:gear f))
        prop-angle (double (or (:prop-angle state) 0.0))
        wireframe? (:wireframe? state)
        dynamic
        (case tag
          :prop {:rotate [0.0 0.0 prop-angle]}
          :gear {:scale [1.0 (max 0.04 g) 1.0]}
          nil)]
    (vec
      (concat
        [{:op/kind :transform/push :matrix (:matrix component)}]
        (when dynamic
          [{:op/kind :transform/push
            :translate [0.0 0.0 0.0]
            :rotate (or (:rotate dynamic) [0.0 0.0 0.0])
            :scale (or (:scale dynamic) [1.0 1.0 1.0])}])
        (map #(mesh-op % wireframe?) (:primitives component))
        (when dynamic [{:op/kind :transform/pop}])
        [{:op/kind :transform/pop}]))))

(defn- piper-asset-ops [state asset]
  (let [scale 1.4
        root-y (- (- (flight/ground-clearance (:flight state)))
                  (* scale (double (:min-y asset))))]
    (vec
      (concat
        [{:op/kind :transform/push
          :translate [0.0 root-y 0.0]
          :rotate [0.0 m/pi 0.0]
          :scale [scale scale scale]}]
        (mapcat #(gltf-component-ops % state) (:components asset))
        [{:op/kind :transform/pop}]))))

(defn- aircraft-ops [state]
  (let [f (:flight state)
        asset (get-in state [:aircraft-assets (:spec-key f)])
        body-ops (if (and (= :prop (:spec-key f)) asset)
                   (piper-asset-ops state asset)
                   (procedural-aircraft-ops state))]
    (vec
      (concat
        [{:op/kind :transform/push :matrix (aircraft-matrix f)}]
        body-ops
        [{:op/kind :transform/pop}]))))

(defn- home-runway-ops []
  [{:op/kind :transform/push :translate [0.0 0.12 0.0]}
   (mesh-op geo/runway-mesh false)
   {:op/kind :draw3d/lines
    :vertices [[0.0 0.18 (- (/ hf/runway-length 2.0))]
               [0.0 0.18 (/ hf/runway-length 2.0)]]
    :color [0.95 0.95 0.95 1.0]
    :stroke-width 2.0}
   {:op/kind :transform/pop}])

(defn- site-runway-ops [site]
  [{:op/kind :transform/push
    :translate [(:x site) (+ (:h site) 0.12) (:z site)]
    :rotate [0.0 (:angle site) 0.0]
    :scale [1.0 1.0 (:len site)]}
   (mesh-op geo/site-runway-mesh false)
   {:op/kind :transform/pop}
   {:op/kind :transform/push
    :translate [(:x site) (+ (:h site) 0.30) (:z site)]
    :rotate [0.0 (:angle site) 0.0]}
   {:op/kind :draw3d/lines
    :vertices [[0.0 0.0 (- (/ (:len site) 2.0))]
               [0.0 0.0 (/ (:len site) 2.0)]]
    :color [0.95 0.95 0.95 1.0]
    :stroke-width 2.0}
   {:op/kind :transform/pop}])

(defn frame-from-state
  [{:keys [flight terrain-mesh viewport wireframe?] :as state}]
  (let [[vw vh] (or viewport [1280.0 720.0])
        aspect (/ (max 1.0 vw) (max 1.0 vh))
        {:keys [position rotation]} (camera-pose state)
        [px _ pz] (:pos flight)
        runways (geo/nearby-runways px pz)]
    (vec
      (concat
        [{:op/kind :frame/clear :color [0.40 0.64 0.82 1.0]}
         {:op/kind :camera3d/set
          :camera3d/projection :perspective
          :camera3d/fov 62.0
          :camera3d/near 0.1
          :camera3d/far 9000.0
          :camera3d/aspect aspect
          :camera3d/position position
          :camera3d/rotation rotation}
         {:op/kind :state/depth-test :enabled true}
         {:op/kind :state/depth-write :enabled true}
         {:op/kind :state/lighting-enable :enabled true}
         {:op/kind :light/ambient :color [0.28 0.31 0.34]}
         {:op/kind :light/directional
          :direction [-0.45 1.0 -0.25]
          :color [1.0 0.96 0.88]
          :intensity 1.25}
         {:op/kind :transform/push :translate [px -10.0 pz]}
         (mesh-op geo/water-mesh false)
         {:op/kind :transform/pop}
         (mesh-op terrain-mesh wireframe?)]
        (home-runway-ops)
        (mapcat site-runway-ops runways)
        (aircraft-ops state)))))
