(ns flight-sim.smoke-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [flight-sim.flight :as flight]
            [flight-sim.geometry :as geo]
            [flight-sim.heightfield :as hf]
            [flight-sim.math :as m]
            [flight-sim.scene :as scene]))

(def dt (/ 1.0 60.0))
(defn- finite-number? [x] (and (number? x) (js/Number.isFinite x)))
(defn- simulate [state seconds step-fn]
  (loop [s state n 0]
    (if (>= n (long (* seconds 60))) s (recur (step-fn s n) (inc n)))))

(deftest terrain-conformance
  (testing "terrain is finite and deterministic"
    (let [samples (for [x (range -100000 100001 10000) z (range -100000 100001 10000)] (hf/height-at x z))]
      (is (every? finite-number? samples)) (is (> (apply max samples) 100.0))
      (is (> (apply min samples) -200.0)) (is (< (apply max samples) 800.0))))
  (testing "mountains exist" (is (> (hf/height-at -40000.0 -32000.0) 250.0)))
  (testing "home runway is flat"
    (doseq [z (range -280 281 40) x [-100.0 -20.0 0.0 20.0 100.0]]
      (is (< (m/abs (hf/height-at x z)) 0.02))))
  (is (= (hf/height-at 1234.5 -987.6) (hf/height-at 1234.5 -987.6))))

(deftest prop-takeoff
  (let [end (simulate (flight/initial-state hf/height-at :prop) 30
                      (fn [s n]
                        (let [t (* n dt) spd (flight/speed s)
                              pitch (cond (and (> spd 40.0) (< t 12.0)) 0.6 (< t 16.0) 0.25 :else 0.0)]
                          (flight/step hf/height-at (assoc s :throttle 1.0 :input {:pitch pitch :roll 0.0 :yaw 0.0}) dt))))]
    (is (false? (:crashed end))) (is (false? (:on-ground end)))
    (is (> (nth (:pos end) 1) 100.0)) (is (< 35.0 (flight/speed end) 140.0))))

(deftest jet-takeoff-and-dash
  (let [takeoff (simulate (flight/initial-state hf/height-at :jet) 20
                          (fn [s n]
                            (let [t (* n dt) pitch (if (and (> (flight/speed s) 40.0) (< t 8.0)) 0.5 0.0)]
                              (flight/step hf/height-at (assoc s :throttle 1.0 :input {:pitch pitch :roll 0.0 :yaw 0.0}) dt))))
        dash-start (assoc takeoff :pitch 0.0 :gear 0.0 :gear-target 0.0 :input {:pitch 0.0 :roll 0.0 :yaw 0.0})
        dash (simulate dash-start 100 (fn [s _] (flight/step hf/height-at (assoc s :throttle 1.0) dt)))]
    (is (false? (:crashed takeoff))) (is (false? (:on-ground takeoff)))
    (is (> (nth (:pos takeoff) 1) 150.0)) (is (> (flight/speed dash) 1010.0))
    (is (< (flight/speed dash) 1060.0)) (is (false? (:crashed dash)))))

(deftest gear-behavior
  (let [f (flight/initial-state hf/height-at :prop)] (is (= 1.0 (:gear-target (flight/toggle-gear f)))))
  (let [f (assoc (flight/initial-state hf/height-at :prop) :on-ground false)] (is (= 0.0 (:gear-target (flight/toggle-gear f)))))
  (letfn [(steady [gear] (simulate (assoc (flight/initial-state hf/height-at :prop)
                                          :pos [0.0 600.0 0.0] :vel [0.0 0.0 -70.0]
                                          :on-ground false :gear gear :gear-target gear :throttle 1.0)
                                         40 (fn [s _] (flight/step hf/height-at s dt))))]
    (let [up (flight/speed (steady 0.0)) down (flight/speed (steady 1.0))] (is (< down (* up 0.85))))))

(deftest landing-gear-protects-touchdown
  (let [base (assoc (flight/initial-state hf/height-at :prop) :pos [0.0 5.0 0.0] :vel [0.0 -2.0 -40.0] :on-ground false)
        belly (simulate (assoc base :gear 0.0 :gear-target 0.0) 3 (fn [s _] (if (:crashed s) s (flight/step hf/height-at s dt))))
        safe (simulate base 2 (fn [s _] (flight/step hf/height-at s dt)))]
    (is (:crashed belly)) (is (false? (:crashed safe)))))

(deftest banked-turn-and-flaps-stay-stable
  (let [airborne (assoc (flight/initial-state hf/height-at :prop) :pos [0.0 500.0 0.0] :vel [0.0 0.0 -65.0]
                        :on-ground false :throttle 0.75 :gear 0.0 :gear-target 0.0)
        h0 (flight/heading-deg airborne)
        turned (simulate airborne 5 (fn [s _] (flight/step hf/height-at (assoc s :input {:pitch 0.0 :roll 1.0 :yaw 0.0}) dt)))
        raw (m/abs (- (flight/heading-deg turned) h0)) delta (if (> raw 180.0) (- 360.0 raw) raw)
        flapped (simulate (assoc turned :flaps 1.0 :throttle 0.4 :input {:pitch 0.0 :roll 0.0 :yaw 0.0}) 6
                          (fn [s _] (flight/step hf/height-at s dt)))]
    (is (> delta 60.0))
    (is (every? finite-number? (concat (:pos flapped) (:vel flapped) [(:pitch flapped) (:roll flapped) (:yaw flapped) (flight/speed flapped)])))))

(deftest long-run-stays-finite-and-above-terrain
  (let [end (loop [s (flight/initial-state hf/height-at :prop) n 0]
              (if (>= n (* 90 60)) s
                (let [phase (quot n 60)
                      s (assoc s :throttle (m/clamp (+ 0.5 (* 0.45 (m/sin (* phase 0.37)))) 0.0 1.0)
                               :input {:pitch (* 0.8 (m/sin (* phase 1.7))) :roll (* 0.9 (m/sin (* phase 0.83))) :yaw (* 0.7 (m/cos (* phase 1.13)))})
                      s (flight/step hf/height-at s dt) s (if (:crashed s) (flight/reset-state hf/height-at s) s)]
                  (recur s (inc n)))))]
    (is (every? finite-number? (concat (:pos end) (:vel end) [(:pitch end) (:roll end) (:yaw end) (flight/speed end)])))
    (let [[x y z] (:pos end)] (is (>= y (- (hf/height-at x z) 0.01))))))

(deftest airframe-switch-preserves-fuel-and-ground-contact
  (let [p (assoc (flight/initial-state hf/height-at :prop) :fuel 24.0)
        j (flight/switch-aircraft hf/height-at p :jet) b (flight/switch-aircraft hf/height-at j :b747)
        [x y z] (:pos b)]
    (is (= :b747 (:spec-key b))) (is (< (m/abs (- 0.5 (flight/fuel-fraction b))) 1.0e-9))
    (is (= 1.0 (:gear b))) (is (< (m/abs (- y (+ (hf/height-at x z) (flight/ground-clearance b)))) 1.0e-9))))

(deftest b747-accelerates-and-remains-finite
  (let [end (simulate (flight/initial-state hf/height-at :b747) 25
                      (fn [s n] (let [pitch (if (and (> (flight/speed s) 78.0) (< n (* 16 60))) 0.45 0.0)]
                                  (flight/step hf/height-at (assoc s :throttle 1.0 :input {:pitch pitch :roll 0.0 :yaw 0.0}) dt))))]
    (is (> (flight/speed end) 55.0)) (is (every? finite-number? (concat (:pos end) (:vel end))))))

(defn- mesh-valid? [mesh]
  (let [vc (count (:vertices mesh))]
    (and (pos? vc) (= vc (count (:normals mesh)))
         (or (nil? (:colors mesh)) (= vc (count (:colors mesh))))
         (every? (fn [[a b c]] (and (integer? a) (integer? b) (integer? c) (<= 0 a) (< a vc) (<= 0 b) (< b vc) (<= 0 c) (< c vc))) (:indices mesh)))))

(deftest procedural-render-meshes-are-well-formed
  (is (mesh-valid? geo/runway-mesh)) (is (mesh-valid? geo/site-runway-mesh)) (is (mesh-valid? geo/water-mesh))
  (doseq [[airframe components] geo/aircraft-components component components]
    (testing (str "mesh for " airframe "/" (:name component)) (is (mesh-valid? (:mesh component))))))

(deftest postgraphics-frame-is-lighting-valid
  (doseq [airframe [:prop :jet :b747]]
    (let [f (flight/initial-state hf/height-at airframe) [x _ z] (:pos f) [cx cz] (geo/terrain-key x z)
          state {:flight f :terrain-mesh (geo/build-terrain-mesh cx cz) :viewport [1280.0 720.0]
                 :camera-mode :chase :orbit-angle 0.0 :wireframe? false :aircraft-assets {}}
          frame (scene/frame-from-state state) meshes (filter #(= :draw3d/mesh (:op/kind %)) frame)]
      (is (= :frame/clear (:op/kind (first frame)))) (is (= :camera3d/set (:op/kind (second frame))))
      (is (some #(= :state/lighting-enable (:op/kind %)) frame)) (is (seq meshes))
      (doseq [mesh meshes] (is (= (count (:vertices mesh)) (count (:normals mesh))))))))
