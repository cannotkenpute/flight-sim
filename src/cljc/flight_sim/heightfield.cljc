(ns flight-sim.heightfield
  (:require [flight-sim.math :as m]))

(def water -10.0)
(def runway-length 3000.0)
(def runway-width 60.0)
(def runway-z0 (- (/ runway-length 2.0)))
(def runway-z1 (/ runway-length 2.0))
(def cell 2200.0)
(def site-width 40.0)

(def ^:private grad3
  [[1.0 1.0] [-1.0 1.0] [1.0 -1.0] [-1.0 -1.0]
   [1.0 0.0] [-1.0 0.0] [0.0 1.0] [0.0 -1.0]])

(defn- u32 [x]
  #?(:clj (bit-and (long x) 0xffffffff)
     :cljs (unsigned-bit-shift-right x 0)))

(defn- seeded-permutation [seed]
  (loop [i 255
         p (vec (range 256))
         n (u32 seed)]
    (if (<= i 0)
      (vec (concat p p))
      (let [n2 (u32 (+ (* n 1664525) 1013904223))
            r (/ (double n2) 4294967296.0)
            j (long (* r (inc i)))
            pi (nth p i)
            pj (nth p j)
            p2 (-> p (assoc i pj) (assoc j pi))]
        (recur (dec i) p2 n2)))))

(def ^:private perm (seeded-permutation 1337))
(def ^:private f2 (* 0.5 (- (m/sqrt 3.0) 1.0)))
(def ^:private g2 (/ (- 3.0 (m/sqrt 3.0)) 6.0))

(defn simplex2 [xin yin]
  (let [s (* (+ xin yin) f2)
        i (long (m/floor (+ xin s)))
        j (long (m/floor (+ yin s)))
        t (* (+ i j) g2)
        x0 (- xin (- i t))
        y0 (- yin (- j t))
        [i1 j1] (if (> x0 y0) [1 0] [0 1])
        x1 (+ (- x0 i1) g2)
        y1 (+ (- y0 j1) g2)
        x2 (+ (- x0 1.0) (* 2.0 g2))
        y2 (+ (- y0 1.0) (* 2.0 g2))
        ii (bit-and i 255)
        jj (bit-and j 255)
        contrib
        (fn [x y gi]
          (let [q (- 0.5 (* x x) (* y y))]
            (if (neg? q)
              0.0
              (let [q2 (* q q)
                    [gx gy] (nth grad3 (mod gi 8))]
                (* q2 q2 (+ (* gx x) (* gy y)))))))
        gi0 (nth perm (+ ii (nth perm jj)))
        gi1 (nth perm (+ ii i1 (nth perm (+ jj j1))))
        gi2 (nth perm (+ ii 1 (nth perm (+ jj 1))))]
    (* 70.0 (+ (contrib x0 y0 gi0)
               (contrib x1 y1 gi1)
               (contrib x2 y2 gi2)))))

(defn fbm [x y octaves]
  (loop [i 0 amp 1.0 freq 1.0 sum 0.0 norm 0.0]
    (if (>= i octaves)
      (/ sum norm)
      (recur (inc i)
             (* amp 0.5)
             (* freq 2.0)
             (+ sum (* amp (simplex2 (* x freq) (* y freq))))
             (+ norm amp)))))

(defn ridged [x y octaves]
  (loop [i 0 amp 0.5 freq 1.0 sum 0.0 norm 0.0]
    (if (>= i octaves)
      (/ sum norm)
      (recur (inc i)
             (* amp 0.5)
             (* freq 2.13)
             (+ sum (* amp (- 1.0 (m/abs (simplex2 (* x freq) (* y freq))))))
             (+ norm amp)))))

(defn raw-height [x z]
  (let [base (* (fbm (* x 0.0006) (* z 0.0006) 4) 45.0)
        mv (+ (* (fbm (+ (* x 0.00013) 37.7)
                         (- (* z 0.00013) 11.2)
                         3)
                   0.5)
              0.5)
        mask (m/smoothstep 0.48 0.72 mv)
        r (ridged (+ (* x 0.00035) 5.1)
                  (+ (* z 0.00035) 9.3)
                  4)]
    (+ base (* (m/pow r 2.1) 640.0 mask))))

(defn- cell-hash [ci cj k]
  (let [v (* (m/sin (+ (* ci 127.1) (* cj 311.7) (* k 74.7))) 43758.5453)]
    (- v (m/floor v))))

(defonce ^:private site-cache (atom {}))

(defn site-at [ci cj]
  (let [key [ci cj]]
    (if (contains? @site-cache key)
      (get @site-cache key)
      (let [home-cell (and (zero? ci) (zero? cj))
            site
            (loop [a 0]
              (if (>= a 3)
                nil
                (let [k (* a 10)
                      skip? (and (> a 0) (> (cell-hash ci cj (+ k 1)) 0.8))
                      x (* (+ ci 0.15 (* (cell-hash ci cj (+ k 2)) 0.7)) cell)
                      z (* (+ cj 0.15 (* (cell-hash ci cj (+ k 3)) 0.7)) cell)
                      angle (* (cell-hash ci cj (+ k 4)) m/pi)
                      len (+ 500.0 (* (cell-hash ci cj (+ k 5)) 700.0))
                      h0 (raw-height x z)
                      home-clear? (not (and home-cell (< (m/hypot2 x z) 1900.0)))
                      viable-height? (and (>= h0 5.0) (<= h0 200.0))
                      gentle?
                      (every?
                        (fn [s]
                          (let [d (* s len 0.5)
                                hh (raw-height (+ x (* (m/sin angle) d))
                                               (+ z (* (m/cos angle) d)))]
                            (and (>= hh 3.0) (<= (m/abs (- hh h0)) 130.0))))
                        [-1.0 -0.5 0.0 0.5 1.0])]
                  (if (and (not skip?) home-clear? viable-height? gentle?)
                    {:id (str ci "," cj)
                     :ci ci :cj cj :x x :z z :angle angle :len len
                     :h (m/clamp h0 4.0 90.0)}
                    (recur (inc a))))))]
        (swap! site-cache assoc key site)
        site))))

(defn nearest-runway [x z]
  (let [zc (m/clamp z runway-z0 runway-z1)
        base {:d (m/hypot2 x (- z zc)) :target 0.0 :x 0.0 :z zc}
        ci (long (m/round (/ x cell)))
        cj (long (m/round (/ z cell)))]
    (reduce
      (fn [best [i j]]
        (if-let [s (site-at i j)]
          (let [dx (- x (:x s))
                dz (- z (:z s))
                sa (m/sin (:angle s))
                ca (m/cos (:angle s))
                la (+ (* dx sa) (* dz ca))
                lc (- (* dx ca) (* dz sa))
                d (m/hypot2 (max 0.0 (- (m/abs la) (* (:len s) 0.5)))
                            (max 0.0 (- (m/abs lc) (* site-width 0.5))))]
            (if (< d (:d best))
              (let [pa (m/clamp la (- (* (:len s) 0.5)) (* (:len s) 0.5))
                    pc (m/clamp lc (- (* site-width 0.5)) (* site-width 0.5))]
                {:d d
                 :target (:h s)
                 :x (+ (:x s) (* pc ca) (* pa sa))
                 :z (+ (:z s) (* (- pc) sa) (* pa ca))})
              best))
          best))
      base
      (for [i (range (dec ci) (+ ci 2))
            j (range (dec cj) (+ cj 2))]
        [i j]))))

(defn height-at [x z]
  (let [raw (raw-height x z)
        nr (nearest-runway x z)
        t (m/smoothstep 260.0 960.0 (:d nr))]
    (+ (:target nr) (* (- raw (:target nr)) t))))
