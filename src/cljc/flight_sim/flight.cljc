(ns flight-sim.flight
  (:require [flight-sim.math :as m]))

(def g 9.81)
(def gear-height 1.6)
(def drag-base 0.02)

(def specs
  {:prop {:name "PIPER HAWK"
          :ground-clearance 1.6
          :spawn {:x 0.0 :z 280.0 :heading 0.0}
          :thrust 14.0 :drag-k 0.0022 :lift-ref 42.0 :stall 26.0
          :pitch-rate 1.5 :roll-rate 2.6 :yaw-rate 0.6 :align 1.4
          :flap-drag 0.8 :flap-lift 0.5 :gear-drag 0.45
          :fuel-cap 48.0 :fuel-burn 9.0 :afterburner false}
   :jet {:name "F-16 VECTOR"
         :ground-clearance 1.6
         :spawn {:x 0.0 :z 280.0 :heading 0.0}
         :thrust 75.0 :drag-k 0.0000514 :lift-ref 55.0 :stall 35.0
         :pitch-rate 1.3 :roll-rate 3.4 :yaw-rate 0.5 :align 1.7
         :flap-drag 0.5 :flap-lift 0.35 :gear-drag 0.8
         :fuel-cap 750.0 :fuel-burn 1400.0 :afterburner true}
   :b747 {:name "BOEING 747-400"
          :ground-clearance 3.84
          :spawn {:x 0.0 :z 1350.0 :heading 0.0}
          :thrust 245.0 :drag-k 0.000047 :lift-ref 118.0 :stall 69.0
          :pitch-rate 0.47 :roll-rate 0.78 :yaw-rate 0.22 :align 0.65
          :flap-drag 1.25 :flap-lift 0.8 :gear-drag 1.6
          :fuel-cap 33000.0 :fuel-burn 4200.0 :afterburner false}})

(defn spec [k] (get specs k (:prop specs)))

(defn ground-clearance [state]
  (double (get-in state [:spec :ground-clearance] gear-height)))

(defn initial-state
  ([height-at] (initial-state height-at :prop))
  ([height-at spec-key]
   (let [s (spec spec-key)
         {:keys [x z heading]} (:spawn s)
         y (+ (height-at x z) (:ground-clearance s))]
     {:spec-key spec-key
      :spec s
      :pos [x y z]
      :vel [0.0 0.0 0.0]
      :pitch 0.0 :roll 0.0 :yaw heading
      :throttle 0.0 :flaps 0.0 :brake false
      :gear 1.0 :gear-target 1.0
      :on-ground true :crashed false
      :fuel (:fuel-cap s)
      :g-load 1.0 :lift-now 0.0
      :input {:pitch 0.0 :roll 0.0 :yaw 0.0}})))

(defn reset-state [height-at state]
  (initial-state height-at (:spec-key state)))

(defn switch-aircraft
  "Switches airframe while preserving airborne state and fuel percentage. A
   grounded switch re-seats the new airframe on the runway and locks gear down."
  [height-at state spec-key]
  (let [old-cap (double (get-in state [:spec :fuel-cap] 1.0))
        frac (m/clamp (/ (double (:fuel state)) old-cap) 0.0 1.0)
        new-spec (spec spec-key)
        state (assoc state
                     :spec-key spec-key
                     :spec new-spec
                     :fuel (* frac (:fuel-cap new-spec)))]
    (if (:on-ground state)
      (let [[x _ z] (:pos state)
            y (+ (height-at x z) (:ground-clearance new-spec))]
        (-> state
            (assoc :pos [x y z]
                   :gear 1.0
                   :gear-target 1.0
                   :on-ground true)
            (assoc-in [:vel 1] 0.0)))
      state)))

(defn fuel-fraction [state]
  (/ (double (:fuel state)) (double (get-in state [:spec :fuel-cap]))))

(defn speed [state]
  (m/length (:vel state)))

(defn axes [{:keys [pitch roll yaw]}]
  (let [cp (m/cos pitch) sp (m/sin pitch)
        cy (m/cos yaw) sy (m/sin yaw)
        cr (m/cos roll) sr (m/sin roll)]
    {:forward [(- (* sy cp)) sp (- (* cy cp))]
     :up [(- (+ (* cy sr) (- (* sy sp cr))))
          (* cp cr)
          (+ (* sy sr) (* cy sp cr))]
     :right [(+ (* cy cr) (* sy sp sr))
             (* cp sr)
             (+ (- (* sy cr)) (* cy sp sr))]}))

(defn heading-deg [state]
  (let [d (mod (* (- (:yaw state)) (/ 180.0 m/pi)) 360.0)]
    (if (neg? d) (+ d 360.0) d)))

(defn agl [height-at state]
  (let [[x y z] (:pos state)]
    (- y (ground-clearance state) (height-at x z))))

(defn aoa [state]
  (let [spd (speed state)]
    (if (< spd 3.0)
      (m/clamp (:pitch state) -0.3 0.3)
      (let [[_ vy _] (:vel state)
            path-pitch (m/asin (m/clamp (/ vy spd) -1.0 1.0))]
        (m/clamp (- (:pitch state) path-pitch) -0.35 0.35)))))

(defn lift-force [state]
  (let [s (:spec state)
        spd (speed state)]
    (if (<= spd 3.0)
      0.0
      (let [a (aoa state)
            cl (cond
                 (<= a 0.0) (m/clamp (+ 0.55 (* a 3.0)) 0.0 0.55)
                 (<= a 0.22) (+ 0.55 (* (/ a 0.22) 0.45))
                 :else (max 0.45 (- 1.0 (* (- a 0.22) 2.0))))
            lift-k (/ g (* (:lift-ref s) (:lift-ref s)))]
        (min (* spd spd lift-k cl (+ 1.0 (* (:flap-lift s) (:flaps state))))
             (* 2.4 g))))))

(defn can-rotate? [state]
  (>= (lift-force state) g))

(defn fuel-rate [state]
  (if (<= (:fuel state) 0.0)
    0.0
    (let [s (:spec state)
          throttle (:throttle state)
          base (* (:fuel-burn s) (+ 0.12 (* 0.88 throttle)))
          gph (if (and (:afterburner s) (> throttle 0.6))
                (* base (+ 1.0 (* 1.6 (/ (- throttle 0.6) 0.4))))
                base)]
      (/ gph 3600.0))))

(defn endurance [state]
  (let [rate (fuel-rate state)]
    (if (pos? rate) (/ (:fuel state) rate) ##Inf)))

(defn adjust-flaps [state dir]
  (update state :flaps #(m/clamp (+ % (* dir 0.25)) 0.0 1.0)))

(defn toggle-gear [state]
  (if (and (:on-ground state) (= 1.0 (:gear-target state)))
    state
    (update state :gear-target #(if (pos? %) 0.0 1.0))))

(defn- approach [v target rate dt]
  (+ v (* (- target v) (min 1.0 (* rate dt)))))

(defn step
  [height-at state dt]
  (let [dt (m/clamp (double dt) 0.0 0.05)
        s (:spec state)
        state (if (= (:gear state) (:gear-target state))
                state
                (let [dir (if (> (:gear-target state) (:gear state)) 1.0 -1.0)
                      g2 (+ (:gear state) (* dir 0.55 dt))
                      g2 (if (pos? dir)
                           (min g2 (:gear-target state))
                           (max g2 (:gear-target state)))]
                  (assoc state :gear g2)))
        {:keys [forward up]} (axes state)
        spd0 (speed state)
        authority (* (m/clamp (/ spd0 50.0) 0.0 1.0)
                     (if (:on-ground state) 0.5 1.0))
        pitch-rate (* (get-in state [:input :pitch]) (:pitch-rate s) authority)
        roll-rate (* -1.0 (get-in state [:input :roll]) (:roll-rate s) authority)
        yaw-rate0 (* -1.0 (get-in state [:input :yaw]) (:yaw-rate s) authority)
        yaw-rate (if (and (not (:on-ground state)) (> spd0 5.0))
                   (+ yaw-rate0 (/ (* g (m/tan (m/clamp (:roll state) -1.05 1.05)))
                                  (max spd0 20.0)))
                   yaw-rate0)
        pitch1 (+ (:pitch state) (* pitch-rate dt))
        roll1 (+ (:roll state) (* roll-rate dt))
        roll2 (if (zero? (get-in state [:input :roll]))
                (- roll1 (* roll1 (min 1.0 (* 0.8 dt))))
                roll1)
        pitch2 (if (and (not (:on-ground state)) (< spd0 (:stall s)))
                 (- pitch1 (* (- (:stall s) spd0) 0.03 dt))
                 pitch1)
        state (assoc state
                     :pitch (m/clamp pitch2 -1.2 1.2)
                     :roll (m/clamp roll2 -1.2 1.2)
                     :yaw (+ (:yaw state) (* yaw-rate dt)))
        {:keys [forward up]} (axes state)
        fuel2 (max 0.0 (- (:fuel state) (* (fuel-rate state) dt)))
        dry? (<= fuel2 0.0)
        thrust (if dry? 0.0 (* (:throttle state) (:thrust s)))
        [fx fy fz] forward
        [ux uy uz] up
        [vx0 vy0 vz0] (:vel state)
        drag-k (* (:drag-k s)
                  (+ 1.0 (* (:flap-drag s) (:flaps state)))
                  (+ 1.0 (* (:gear-drag s) (:gear state)))
                  (if (:brake state) 3.2 1.0))
        drag (+ (* spd0 spd0 drag-k) (* drag-base spd0))
        [ax0 ay0 az0] [(* fx thrust) (+ -9.81 (* fy thrust)) (* fz thrust)]
        [ax1 ay1 az1]
        (if (> spd0 0.01)
          [(- ax0 (* (/ vx0 spd0) drag))
           (- ay0 (* (/ vy0 spd0) drag))
           (- az0 (* (/ vz0 spd0) drag))]
          [ax0 ay0 az0])
        lift (lift-force state)
        [lx ly lz]
        (if (> spd0 3.0)
          (let [vn (m/normalize [vx0 vy0 vz0])
                d (m/dot [ux uy uz] vn)
                perp (m/v- [ux uy uz] (m/v* vn d))]
            (m/normalize perp))
          [ux uy uz])
        ax (+ ax1 (* lx lift))
        ay (+ ay1 (* ly lift))
        az (+ az1 (* lz lift))
        vx1 (+ vx0 (* ax dt))
        vy1 (+ vy0 (* ay dt))
        vz1 (+ vz0 (* az dt))
        g-load (/ (+ (* ax ux) (* (+ ay g) uy) (* az uz)) g)
        state (assoc state :fuel fuel2 :lift-now lift :g-load g-load :vel [vx1 vy1 vz1])
        spd1 (speed state)
        state
        (cond
          (and (:on-ground state) (> spd1 1.0))
          (let [[fx _ fz] forward
                fh (max 1.0e-9 (m/hypot2 fx fz))
                t (min 1.0 (* 5.0 dt))
                dx (+ vx1 (* (- (* (/ fx fh) spd1) vx1) t))
                dz (+ vz1 (* (- (* (/ fz fh) spd1) vz1) t))
                fr (* (if (:brake state) 7.0 0.5) dt)
                hs (m/hypot2 dx dz)
                k (if (> hs 0.01) (/ (max 0.0 (- hs fr)) hs) 1.0)]
            (assoc state :vel [(* dx k) vy1 (* dz k)]))

          (> spd1 1.0)
          (let [t (min 1.0 (* (:align s) dt))
                [dx0 dy0 dz0] (m/normalize [vx1 vy1 vz1])
                dx (+ dx0 (* (- fx dx0) t))
                dy (+ dy0 (* (- fy dy0) t))
                dz (+ dz0 (* (- fz dz0) t))
                [dx dy dz] (m/normalize [dx dy dz])]
            (assoc state :vel [(* dx spd1) (* dy spd1) (* dz spd1)]))

          :else state)
        state (if (and (:on-ground state)
                       (pos? (nth (:vel state) 1))
                       (< lift g))
                (assoc-in state [:vel 1] 0.0)
                state)
        [vx vy vz] (:vel state)
        [px py pz] (:pos state)
        pos2 [(+ px (* vx dt)) (+ py (* vy dt)) (+ pz (* vz dt))]
        [x2 y2 z2] pos2
        ground-y (+ (height-at x2 z2) (ground-clearance state))]
    (if (<= y2 (+ ground-y 0.01))
      (let [impact vy
            crash? (or (< impact -13.0)
                       (> (m/abs (:roll state)) 0.55)
                       (> (m/abs (:pitch state)) 0.5)
                       (< (:gear state) 0.9))]
        (-> state
            (assoc :pos [x2 ground-y z2]
                   :vel [vx (if (neg? vy) 0.0 vy) vz]
                   :on-ground true
                   :crashed crash?)
            (cond-> (not crash?)
              (update :roll #(* % (max 0.0 (- 1.0 (* 5.0 dt)))))
              (update :pitch #(m/clamp % -0.15 0.35)))))
      (assoc state
             :pos pos2
             :on-ground (if (> y2 (+ ground-y 0.3)) false (:on-ground state))))))
