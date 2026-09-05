(ns flight-sim.math
  (:refer-clojure :exclude [abs]))

(def pi #?(:clj Math/PI :cljs js/Math.PI))

(defn sin [x] #?(:clj (Math/sin (double x)) :cljs (js/Math.sin x)))
(defn cos [x] #?(:clj (Math/cos (double x)) :cljs (js/Math.cos x)))
(defn tan [x] #?(:clj (Math/tan (double x)) :cljs (js/Math.tan x)))
(defn asin [x] #?(:clj (Math/asin (double x)) :cljs (js/Math.asin x)))
(defn atan2 [y x] #?(:clj (Math/atan2 (double y) (double x)) :cljs (js/Math.atan2 y x)))
(defn sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))
(defn floor [x] #?(:clj (Math/floor (double x)) :cljs (js/Math.floor x)))
(defn round [x] #?(:clj (Math/round (double x)) :cljs (js/Math.round x)))
(defn abs [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn pow [x y] #?(:clj (Math/pow (double x) (double y)) :cljs (js/Math.pow x y)))

(defn clamp [v lo hi]
  (max lo (min hi v)))

(defn smoothstep [e0 e1 x]
  (let [t (clamp (/ (- x e0) (- e1 e0)) 0.0 1.0)]
    (* t t (- 3.0 (* 2.0 t)))))

(defn hypot2 [x y]
  (sqrt (+ (* x x) (* y y))))

(defn hypot3 [x y z]
  (sqrt (+ (* x x) (* y y) (* z z))))

(defn v+ [[ax ay az] [bx by bz]]
  [(+ ax bx) (+ ay by) (+ az bz)])

(defn v- [[ax ay az] [bx by bz]]
  [(- ax bx) (- ay by) (- az bz)])

(defn v* [[x y z] k]
  [(* x k) (* y k) (* z k)])

(defn dot [[ax ay az] [bx by bz]]
  (+ (* ax bx) (* ay by) (* az bz)))

(defn cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn length [[x y z]]
  (hypot3 x y z))

(defn normalize [v]
  (let [n (length v)]
    (if (< n 1.0e-9) [0.0 0.0 0.0] (v* v (/ 1.0 n)))))

(defn look-rotation
  "Euler XYZ [pitch yaw roll] for a camera whose local forward axis is -Z."
  [from to]
  (let [[dx dy dz] (normalize (v- to from))]
    [(asin (clamp dy -1.0 1.0))
     (atan2 (- dx) (- dz))
     0.0]))

(defn deg [rad] (* rad (/ 180.0 pi)))
(defn rad [deg] (* deg (/ pi 180.0)))
