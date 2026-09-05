(ns flight-sim.geometry
  (:require [flight-sim.heightfield :as hf]
            [flight-sim.math :as m]))

(def terrain-chunk 512.0)
(def terrain-radius 3)
(def terrain-segments 16)

(def ^:private bands
  [{:h -7.0 :c [0.78 0.72 0.52 1.0]}
   {:h 2.0 :c [0.55 0.62 0.35 1.0]}
   {:h 10.0 :c [0.32 0.51 0.25 1.0]}
   {:h 45.0 :c [0.26 0.42 0.22 1.0]}
   {:h 90.0 :c [0.23 0.38 0.20 1.0]}
   {:h 140.0 :c [0.40 0.42 0.33 1.0]}
   {:h 190.0 :c [0.50 0.47 0.45 1.0]}
   {:h 240.0 :c [0.68 0.66 0.64 1.0]}
   {:h 300.0 :c [0.94 0.95 0.97 1.0]}])

(defn- hash-noise [x z]
  (let [v (* (m/sin (+ (* x 12.9898) (* z 78.233))) 43758.5453)]
    (- v (m/floor v))))

(defn- lerp [a b t] (+ a (* (- b a) t)))

(defn- terrain-color [h slope x z]
  (let [[r g b _]
        (cond
          (<= h (:h (first bands))) (:c (first bands))
          (>= h (:h (last bands))) (:c (last bands))
          :else
          (let [[a b]
                (first
                  (filter (fn [[a b]] (<= (:h a) h (:h b)))
                          (partition 2 1 bands)))
                t (/ (- h (:h a)) (- (:h b) (:h a)))
                [ar ag ab _] (:c a)
                [br bg bb _] (:c b)]
            [(lerp ar br t) (lerp ag bg t) (lerp ab bb t) 1.0]))
        rock (m/clamp (/ (- slope 0.45) 0.55) 0.0 1.0)
        v (* (- (hash-noise x z) 0.5) 0.07)]
    [(+ (lerp r 0.44 rock) v)
     (+ (lerp g 0.40 rock) v)
     (+ (lerp b 0.37 rock) v)
     1.0]))

(defn- edge-key [a b]
  (if (< a b) [a b] [b a]))

(defn wire-edges
  "Returns a de-duplicated edge list for a triangle index vector."
  [indices]
  (loop [tris indices seen #{} out []]
    (if-let [[a b c] (first tris)]
      (let [edges [(edge-key a b) (edge-key b c) (edge-key c a)]
            [seen out]
            (reduce (fn [[seen out] e]
                      (if (contains? seen e)
                        [seen out]
                        [(conj seen e) (conj out e)]))
                    [seen out]
                    edges)]
        (recur (rest tris) seen out))
      out)))

(defn with-wire-edges [mesh]
  (if (:wire-edges mesh)
    mesh
    (assoc mesh :wire-edges (wire-edges (:indices mesh)))))

(defn chunk-coord [v]
  (long (m/round (/ v terrain-chunk))))

(defn terrain-key [x z]
  [(chunk-coord x) (chunk-coord z)])

(defn build-terrain-mesh
  "Builds the visible terrain neighborhood into one immediate-mode PostGraphics mesh."
  [center-cx center-cz]
  (let [seg terrain-segments
        n (inc seg)
        step (/ terrain-chunk seg)]
    (loop [chunks (for [cx (range (- center-cx terrain-radius)
                                   (+ center-cx terrain-radius 1))
                        cz (range (- center-cz terrain-radius)
                                  (+ center-cz terrain-radius 1))]
                    [cx cz])
           vertices [] normals [] colors [] indices []]
      (if-let [[cx cz] (first chunks)]
        (let [base (count vertices)
              [vs ns cs]
              (reduce
                (fn [[vs ns cs] [ix iz]]
                  (let [lx (- (* ix step) (/ terrain-chunk 2.0))
                        lz (- (* iz step) (/ terrain-chunk 2.0))
                        wx (+ (* cx terrain-chunk) lx)
                        wz (+ (* cz terrain-chunk) lz)
                        h (hf/height-at wx wz)
                        dhx (- (hf/height-at (+ wx step) wz)
                               (hf/height-at (- wx step) wz))
                        dhz (- (hf/height-at wx (+ wz step))
                               (hf/height-at wx (- wz step)))
                        slope (min 1.0 (* (/ (m/hypot2 dhx dhz) (* 2.0 step)) 2.2))
                        normal (m/normalize [(- dhx) (* 2.0 step) (- dhz)])]
                    [(conj vs [wx h wz])
                     (conj ns normal)
                     (conj cs (terrain-color h slope wx wz))]))
                [vertices normals colors]
                (for [iz (range n) ix (range n)] [ix iz]))
              is
              (reduce
                (fn [acc [ix iz]]
                  (let [a (+ base (* iz n) ix)
                        b (inc a)
                        c (+ a n)
                        d (inc c)]
                    (conj acc [a c b] [b c d])))
                indices
                (for [iz (range seg) ix (range seg)] [ix iz]))]
          (recur (rest chunks) vs ns cs is))
        (with-wire-edges
          {:vertices vertices
           :normals normals
           :colors colors
           :indices indices})))))

(defn- triangle-normal [a b c]
  (m/normalize (m/cross (m/v- b a) (m/v- c a))))

(defn- triangles-mesh
  "Flat-shaded mesh from explicit triangles. Each triangle gets independent vertices."
  [triangles color]
  (loop [ts triangles vertices [] normals [] colors [] indices []]
    (if-let [[a b c] (first ts)]
      (let [base (count vertices)
            n (triangle-normal a b c)]
        (recur (rest ts)
               (conj vertices a b c)
               (conj normals n n n)
               (conj colors color color color)
               (conj indices [base (inc base) (+ base 2)])))
      (with-wire-edges
        {:vertices vertices :normals normals :colors colors :indices indices}))))

(defn box-mesh
  "Axis-aligned box centered at origin. Returns independent faces for stable normals."
  [sx sy sz color]
  (let [x (/ sx 2.0) y (/ sy 2.0) z (/ sz 2.0)
        faces [[[0 1 0] [[(- x) y (- z)] [(- x) y z] [x y z] [x y (- z)]]]
               [[0 -1 0] [[(- x) (- y) z] [(- x) (- y) (- z)] [x (- y) (- z)] [x (- y) z]]]
               [[0 0 -1] [[x (- y) (- z)] [(- x) (- y) (- z)] [(- x) y (- z)] [x y (- z)]]]
               [[0 0 1] [[(- x) (- y) z] [x (- y) z] [x y z] [(- x) y z]]]
               [[1 0 0] [[x (- y) z] [x (- y) (- z)] [x y (- z)] [x y z]]]
               [[-1 0 0] [[(- x) (- y) (- z)] [(- x) (- y) z] [(- x) y z] [(- x) y (- z)]]]]]
    (with-wire-edges
      (reduce
        (fn [{:keys [vertices normals colors indices]} [normal quad]]
          (let [base (count vertices)
                vertices2 (into vertices quad)
                normals2 (into normals (repeat 4 normal))
                colors2 (into colors (repeat 4 color))]
            {:vertices vertices2
             :normals normals2
             :colors colors2
             :indices (conj indices [base (inc base) (+ base 2)]
                                    [base (+ base 2) (+ base 3)])}))
        {:vertices [] :normals [] :colors [] :indices []}
        faces))))

(defn cylinder-mesh
  "Tapered cylinder along local Z. r-front is at -Z and r-back at +Z."
  [r-front r-back length segments color]
  (let [z0 (- (/ length 2.0))
        z1 (/ length 2.0)
        side-tris
        (mapcat
          (fn [i]
            (let [a0 (* (/ i segments) 2.0 m/pi)
                  a1 (* (/ (inc i) segments) 2.0 m/pi)
                  p00 [(* (m/sin a0) r-front) (* (m/cos a0) r-front) z0]
                  p01 [(* (m/sin a1) r-front) (* (m/cos a1) r-front) z0]
                  p10 [(* (m/sin a0) r-back) (* (m/cos a0) r-back) z1]
                  p11 [(* (m/sin a1) r-back) (* (m/cos a1) r-back) z1]]
              [[p00 p10 p01] [p01 p10 p11]]))
          (range segments))
        front-center [0.0 0.0 z0]
        back-center [0.0 0.0 z1]
        caps
        (mapcat
          (fn [i]
            (let [a0 (* (/ i segments) 2.0 m/pi)
                  a1 (* (/ (inc i) segments) 2.0 m/pi)
                  f0 [(* (m/sin a0) r-front) (* (m/cos a0) r-front) z0]
                  f1 [(* (m/sin a1) r-front) (* (m/cos a1) r-front) z0]
                  b0 [(* (m/sin a0) r-back) (* (m/cos a0) r-back) z1]
                  b1 [(* (m/sin a1) r-back) (* (m/cos a1) r-back) z1]]
              [[front-center f1 f0] [back-center b0 b1]]))
          (range segments))]
    (triangles-mesh (concat side-tris caps) color)))

(defn cone-mesh
  "Cone along local Z with tip at -Z and base at +Z."
  [radius length segments color]
  (let [tip [0.0 0.0 (- (/ length 2.0))]
        z (/ length 2.0)
        center [0.0 0.0 z]
        tris
        (mapcat
          (fn [i]
            (let [a0 (* (/ i segments) 2.0 m/pi)
                  a1 (* (/ (inc i) segments) 2.0 m/pi)
                  p0 [(* (m/sin a0) radius) (* (m/cos a0) radius) z]
                  p1 [(* (m/sin a1) radius) (* (m/cos a1) radius) z]]
              [[tip p0 p1] [center p1 p0]]))
          (range segments))]
    (triangles-mesh tris color)))

(defn sphere-mesh
  [radius lat-segments lon-segments color]
  (let [vertices
        (vec
          (for [iy (range (inc lat-segments))
                ix (range (inc lon-segments))]
            (let [v (/ iy lat-segments)
                  u (/ ix lon-segments)
                  phi (* v m/pi)
                  theta (* u 2.0 m/pi)
                  sp (m/sin phi)
                  cp (m/cos phi)
                  st (m/sin theta)
                  ct (m/cos theta)]
              [(* radius sp st) (* radius cp) (* radius sp ct)])))
        normals (mapv #(m/normalize %) vertices)
        colors (vec (repeat (count vertices) color))
        n (inc lon-segments)
        indices
        (vec
          (mapcat
            (fn [[iy ix]]
              (let [a (+ (* iy n) ix)
                    b (inc a)
                    c (+ a n)
                    d (inc c)]
                (cond
                  (zero? iy) [[a c d]]
                  (= iy (dec lat-segments)) [[a c b]]
                  :else [[a c b] [b c d]])))
            (for [iy (range lat-segments) ix (range lon-segments)] [iy ix])))]
    (with-wire-edges
      {:vertices vertices :normals normals :colors colors :indices indices})))

(defn wing-prism-mesh
  "Extrudes an x/z polygon by thickness along Y. Polygon is expected CCW from above."
  [points thickness color]
  (let [y (/ thickness 2.0)
        top (mapv (fn [[x z]] [x y z]) points)
        bottom (mapv (fn [[x z]] [x (- y) z]) points)
        n (count points)
        top-tris (for [i (range 1 (dec n))] [(nth top 0) (nth top i) (nth top (inc i))])
        bottom-tris (for [i (range 1 (dec n))] [(nth bottom 0) (nth bottom (inc i)) (nth bottom i)])
        side-tris
        (mapcat
          (fn [i]
            (let [j (mod (inc i) n)
                  a (nth top i) b (nth top j)
                  c (nth bottom i) d (nth bottom j)]
              [[a c b] [b c d]]))
          (range n))]
    (triangles-mesh (concat top-tris bottom-tris side-tris) color)))

(defn transform-vertices
  "Bakes a simple local TRS into mesh vertices. Rotation is Euler XYZ."
  [mesh translate rotate scale]
  (let [[tx ty tz] (or translate [0.0 0.0 0.0])
        [rx ry rz] (or rotate [0.0 0.0 0.0])
        [sx sy sz] (or scale [1.0 1.0 1.0])
        crx (m/cos rx) srx (m/sin rx)
        cry (m/cos ry) sry (m/sin ry)
        crz (m/cos rz) srz (m/sin rz)
        xf (fn [[x y z]]
             (let [x (* x sx) y (* y sy) z (* z sz)
                   y1 (- (* y crx) (* z srx))
                   z1 (+ (* y srx) (* z crx))
                   x2 (+ (* x cry) (* z1 sry))
                   z2 (+ (* (- x) sry) (* z1 cry))
                   x3 (- (* x2 crz) (* y1 srz))
                   y3 (+ (* x2 srz) (* y1 crz))]
               [(+ x3 tx) (+ y3 ty) (+ z2 tz)]))
        normal-xf (fn [n]
                    (m/normalize (m/v- (xf n) (xf [0.0 0.0 0.0]))))]
    (-> mesh
        (assoc :vertices (mapv xf (:vertices mesh)))
        (assoc :normals (mapv normal-xf (:normals mesh)))
        (dissoc :wire-edges)
        with-wire-edges)))

(defn merge-meshes
  "Merges already-baked meshes. Useful for static windows/gear to reduce draw calls."
  [meshes]
  (with-wire-edges
    (reduce
      (fn [{:keys [vertices normals colors indices] :as acc} mesh]
        (let [base (count vertices)]
          (assoc acc
                 :vertices (into vertices (:vertices mesh))
                 :normals (into normals (:normals mesh))
                 :colors (into colors (:colors mesh))
                 :indices (into indices
                                (map (fn [[a b c]] [(+ base a) (+ base b) (+ base c)])
                                     (:indices mesh))))))
      {:vertices [] :normals [] :colors [] :indices []}
      meshes)))

(def runway-mesh
  (box-mesh hf/runway-width 0.18 hf/runway-length [0.20 0.22 0.24 1.0]))

(def site-runway-mesh
  (box-mesh hf/site-width 0.18 1.0 [0.20 0.22 0.24 1.0]))

(def water-mesh
  (box-mesh 12000.0 0.05 12000.0 [0.07 0.29 0.48 0.98]))

(defn nearby-runways [x z]
  (let [ci (long (m/round (/ x hf/cell)))
        cj (long (m/round (/ z hf/cell)))]
    (keep identity
          (for [i (range (dec ci) (+ ci 2))
                j (range (dec cj) (+ cj 2))]
            (hf/site-at i j)))))

(def ^:private white [0.94 0.95 0.97 1.0])
(def ^:private piper-red [0.82 0.20 0.17 1.0])
(def ^:private piper-dark [0.17 0.19 0.22 1.0])
(def ^:private piper-glass [0.37 0.72 0.91 0.80])
(def ^:private jet-hull [0.47 0.51 0.56 1.0])
(def ^:private jet-wing [0.43 0.46 0.51 1.0])
(def ^:private jet-dark [0.24 0.26 0.30 1.0])
(def ^:private jet-red [0.78 0.23 0.19 1.0])
(def ^:private b-blue [0.07 0.23 0.44 1.0])
(def ^:private b-stripe [0.18 0.47 0.72 1.0])
(def ^:private b-dark [0.13 0.16 0.21 1.0])
(def ^:private b-glass [0.09 0.17 0.26 1.0])
(def ^:private b-metal [0.62 0.66 0.71 1.0])
(def ^:private tire [0.08 0.10 0.12 1.0])

(defn- component
  ([name mesh translate] (component name mesh translate [0.0 0.0 0.0] [1.0 1.0 1.0] nil))
  ([name mesh translate rotate scale tag]
   {:name name :mesh mesh :translate translate :rotate rotate :scale scale :tag tag}))

(def procedural-piper-components
  [(component :fuselage (cylinder-mesh 0.80 1.00 9.0 10 white) [0.0 0.0 -0.5])
   (component :nose (cone-mesh 0.80 2.2 10 piper-red) [0.0 0.0 -6.1])
   (component :cockpit (box-mesh 1.2 0.9 2.2 piper-glass) [0.0 0.85 -2.6])
   (component :wing (box-mesh 15.0 0.25 2.8 piper-red) [0.0 0.35 -1.0])
   (component :tail-wing (box-mesh 5.5 0.2 1.6 piper-red) [0.0 0.3 4.0])
   (component :fin (box-mesh 0.2 2.2 1.8 piper-red) [0.0 1.3 4.1])
   (component :prop-blade-a (box-mesh 0.3 3.6 0.12 piper-dark) [0.0 0.0 -7.35] [0.0 0.0 0.0] [1.0 1.0 1.0] :prop)
   (component :prop-blade-b (box-mesh 0.3 3.6 0.12 piper-dark) [0.0 0.0 -7.35] [0.0 0.0 (/ m/pi 2.0)] [1.0 1.0 1.0] :prop)
   (component :main-strut-l (box-mesh 0.12 0.8 0.12 piper-dark) [-1.6 -0.85 -1.5] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear)
   (component :main-strut-r (box-mesh 0.12 0.8 0.12 piper-dark) [1.6 -0.85 -1.5] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear)
   (component :main-wheel-l (cylinder-mesh 0.35 0.35 0.3 10 piper-dark) [-1.6 -1.25 -1.5] [0.0 (/ m/pi 2.0) 0.0] [1.0 1.0 1.0] :gear)
   (component :main-wheel-r (cylinder-mesh 0.35 0.35 0.3 10 piper-dark) [1.6 -1.25 -1.5] [0.0 (/ m/pi 2.0) 0.0] [1.0 1.0 1.0] :gear)
   (component :tail-strut (box-mesh 0.12 0.8 0.12 piper-dark) [0.0 -0.6 3.6] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear)
   (component :tail-wheel (cylinder-mesh 0.35 0.35 0.3 10 piper-dark) [0.0 -1.0 3.6] [0.0 (/ m/pi 2.0) 0.0] [1.0 1.0 1.0] :gear)])

(def ^:private jet-wing-mesh
  (wing-prism-mesh [[0.0 -2.4] [6.4 2.2] [6.4 3.6] [0.0 1.4]] 0.22 jet-wing))
(def ^:private jet-stab-mesh
  (wing-prism-mesh [[0.0 -0.9] [3.1 0.9] [3.1 1.8] [0.0 1.0]] 0.16 jet-wing))

(def jet-components
  [(component :fuselage (cylinder-mesh 0.65 0.90 11.0 10 jet-hull) [0.0 0.0 0.0])
   (component :nose (cone-mesh 0.65 3.4 10 jet-hull) [0.0 0.0 -7.0])
   (component :canopy (box-mesh 0.95 0.75 2.8 piper-glass) [0.0 0.8 -3.0])
   (component :wing-r jet-wing-mesh [0.0 0.15 0.4])
   (component :wing-l jet-wing-mesh [0.0 0.15 0.4] [0.0 0.0 0.0] [-1.0 1.0 1.0] nil)
   (component :stab-r jet-stab-mesh [0.0 0.25 4.2])
   (component :stab-l jet-stab-mesh [0.0 0.25 4.2] [0.0 0.0 0.0] [-1.0 1.0 1.0] nil)
   (component :fin-r (box-mesh 0.14 1.9 2.4 jet-red) [1.05 1.25 4.6] [0.0 0.0 -0.32] [1.0 1.0 1.0] nil)
   (component :fin-l (box-mesh 0.14 1.9 2.4 jet-red) [-1.05 1.25 4.6] [0.0 0.0 0.32] [1.0 1.0 1.0] nil)
   (component :intake-l (box-mesh 0.5 0.7 2.2 jet-dark) [-0.95 -0.35 -2.2])
   (component :intake-r (box-mesh 0.5 0.7 2.2 jet-dark) [0.95 -0.35 -2.2])
   (component :nozzle (cylinder-mesh 0.50 0.65 1.4 10 jet-dark) [0.0 0.0 5.9])
   (component :burner (cone-mesh 0.42 2.6 10 [1.0 0.42 0.10 0.88]) [0.0 0.0 7.8] [m/pi 0.0 0.0] [1.0 1.0 1.0] :burner)
   (component :main-strut-l (box-mesh 0.12 0.8 0.12 jet-dark) [-1.3 -0.8 1.2] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear)
   (component :main-strut-r (box-mesh 0.12 0.8 0.12 jet-dark) [1.3 -0.8 1.2] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear)
   (component :main-wheel-l (cylinder-mesh 0.32 0.32 0.28 10 jet-dark) [-1.3 -1.22 1.2] [0.0 (/ m/pi 2.0) 0.0] [1.0 1.0 1.0] :gear)
   (component :main-wheel-r (cylinder-mesh 0.32 0.32 0.28 10 jet-dark) [1.3 -1.22 1.2] [0.0 (/ m/pi 2.0) 0.0] [1.0 1.0 1.0] :gear)
   (component :nose-strut (box-mesh 0.12 0.8 0.12 jet-dark) [0.0 -0.8 -4.0] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear)
   (component :nose-wheel (cylinder-mesh 0.32 0.32 0.28 10 jet-dark) [0.0 -1.22 -4.0] [0.0 (/ m/pi 2.0) 0.0] [1.0 1.0 1.0] :gear)])

(defn- fuselage-747-mesh []
  (let [sections [[-35.34 0.15 0.18] [-32.8 1.75 1.55] [-29.5 2.85 2.55]
                  [-24.0 3.45 3.0] [-12.0 3.65 3.18] [10.0 3.65 3.18]
                  [22.0 3.4 3.0] [29.0 2.85 2.45] [33.5 1.65 1.5]
                  [35.34 0.12 0.15]]
        radial 12
        vertices
        (vec
          (for [[z rx ry] sections j (range radial)]
            (let [a (* (/ j radial) 2.0 m/pi)]
              [(* (m/sin a) rx) (+ 5.35 (* (m/cos a) ry)) z])))
        normals
        (vec
          (for [[_z rx ry] sections j (range radial)]
            (let [a (* (/ j radial) 2.0 m/pi)]
              (m/normalize [(/ (m/sin a) (max rx 0.01))
                            (/ (m/cos a) (max ry 0.01))
                            0.0]))))
        colors (vec (repeat (count vertices) white))
        indices
        (vec
          (mapcat
            (fn [[i j]]
              (let [n (mod (inc j) radial)
                    a (+ (* i radial) j)
                    b (+ (* i radial) n)
                    c (+ (* (inc i) radial) j)
                    d (+ (* (inc i) radial) n)]
                [[a c b] [b c d]]))
            (for [i (range (dec (count sections))) j (range radial)] [i j])))]
    (with-wire-edges {:vertices vertices :normals normals :colors colors :indices indices})))

(def ^:private wing-747
  (wing-prism-mesh [[0.0 -1.5] [32.2 7.0] [32.2 16.4] [0.0 15.8]] 0.62 white))
(def ^:private tail-747
  (wing-prism-mesh [[0.0 -1.2] [12.0 2.7] [12.0 5.3] [0.0 5.0]] 0.34 white))

(def ^:private passenger-window-mesh
  (let [base (box-mesh 0.10 0.28 0.72 b-glass)]
    (merge-meshes
      (concat
        (for [side [-1.0 1.0]
              z (range -21.0 14.01 2.2)]
          (transform-vertices base [(* side 3.22) 1.25 z] [0.0 0.0 0.0]
                          [1.0 1.0 1.0]))
        (for [side [-1.0 1.0]
              z (range -20.0 -4.59 2.2)]
          (transform-vertices base [(* side 2.72) 3.55 z] [0.0 0.0 0.0]
                          [0.92 0.92 0.92]))))))

(def ^:private door-mesh
  (let [base (box-mesh 0.08 2.65 1.35 b-blue)]
    (merge-meshes
      (for [z [-19.0 -4.0 10.0] side [-1.0 1.0]]
        (transform-vertices base [(* side 3.62) 5.1 z] nil nil)))))

(def ^:private engine-pod-mesh
  (merge-meshes
    [(cylinder-mesh 1.55 1.75 5.7 12 b-metal)
     (transform-vertices (cylinder-mesh 1.34 1.34 0.16 16 b-dark) [0.0 0.0 -2.86] nil nil)
     (transform-vertices (cylinder-mesh 1.10 1.35 0.45 12 b-dark) [0.0 0.0 2.95] nil nil)]))

(def ^:private engine-fan-mesh
  (let [hub (cylinder-mesh 1.18 1.18 0.10 16 b-dark)
        blade (box-mesh 0.14 1.05 0.04 b-metal)]
    (merge-meshes
      (cons hub
            (for [i (range 6)]
              (transform-vertices blade [0.0 0.0 -0.08]
                                  [0.0 0.0 (* i (/ m/pi 3.0))]
                                  nil))))))

(def ^:private main-gear-mesh
  (let [leg (transform-vertices (box-mesh 0.42 3.0 0.42 b-metal) [0.0 -1.55 0.0] nil nil)
        wheel (cylinder-mesh 0.66 0.66 0.42 10 tire)]
    (merge-meshes
      (cons leg
            (for [dx [-0.85 -0.28 0.28 0.85]]
              (transform-vertices wheel [dx -3.05 0.0] [0.0 (/ m/pi 2.0) 0.0] nil))))))

(def ^:private nose-gear-mesh
  (let [leg (transform-vertices (box-mesh 0.36 3.0 0.36 b-metal) [0.0 -1.55 0.0] nil nil)
        wheel (cylinder-mesh 0.50 0.50 0.32 10 tire)]
    (merge-meshes
      (cons leg
            (for [side [-1.0 1.0]]
              (transform-vertices wheel [(* side 0.56) -3.05 0.0]
                                  [0.0 (/ m/pi 2.0) 0.0] nil))))))

(def b747-components
  (vec
    (concat
      [(component :fuselage (fuselage-747-mesh) [0.0 0.0 0.0])
       (component :upper-deck (sphere-mesh 1.0 8 16 white) [0.0 8.05 -13.1] [0.0 0.0 0.0] [2.9 1.55 10.4] nil)
       (component :cheatline (box-mesh 6.85 0.18 51.0 b-stripe) [0.0 5.3 -1.5])
       (component :belly (box-mesh 6.3 0.22 48.0 b-blue) [0.0 2.75 0.0])
       (assoc (component :cockpit (box-mesh 4.2 1.1 3.7 b-glass) [0.0 8.55 -25.8] [-0.1 0.0 0.0] [1.0 1.0 1.0] nil) :cockpit-visible? true)
       (assoc (component :upper-cockpit (box-mesh 3.8 0.55 2.4 b-glass) [0.0 9.3 -18.2]) :cockpit-visible? true)
       (component :windows passenger-window-mesh [0.0 0.0 0.0])
       (component :doors door-mesh [0.0 0.0 0.0])
       (component :wing-r wing-747 [0.0 3.95 0.0])
       (component :wing-l wing-747 [0.0 3.95 0.0] [0.0 0.0 0.0] [-1.0 1.0 1.0] nil)
       (component :winglet-r (box-mesh 0.45 3.2 2.6 b-blue) [32.1 5.35 11.9] [0.0 0.0 0.08] [1.0 1.0 1.0] nil)
       (component :winglet-l (box-mesh 0.45 3.2 2.6 b-blue) [-32.1 5.35 11.9] [0.0 0.0 -0.08] [1.0 1.0 1.0] nil)
       (component :tail-r tail-747 [0.0 6.0 25.0])
       (component :tail-l tail-747 [0.0 6.0 25.0] [0.0 0.0 0.0] [-1.0 1.0 1.0] nil)
       (component :vertical-tail (box-mesh 0.55 9.8 8.2 b-blue) [0.0 10.2 25.8] [0.12 0.0 0.0] [1.0 1.0 1.0] nil)
       (component :flap-l (box-mesh 16.5 0.42 3.2 b-stripe) [-17.6 3.65 12.7] [0.04 0.0 0.0] [1.0 1.0 1.0] :flap-l)
       (component :flap-r (box-mesh 16.5 0.42 3.2 b-stripe) [17.6 3.65 12.7] [0.04 0.0 0.0] [1.0 1.0 1.0] :flap-r)
       (component :aileron-l (box-mesh 12.5 0.3 2.2 white) [-29.9 4.35 13.2] [0.0 0.0 0.0] [1.0 1.0 1.0] :aileron-l)
       (component :aileron-r (box-mesh 12.5 0.3 2.2 white) [29.9 4.35 13.2] [0.0 0.0 0.0] [1.0 1.0 1.0] :aileron-r)
       (component :elevators (box-mesh 20.5 0.3 2.1 white) [0.0 6.0 27.5] [0.0 0.0 0.0] [1.0 1.0 1.0] :elevator)
       (component :rudder (box-mesh 0.6 5.4 3.8 b-blue) [0.0 10.5 29.1] [0.0 0.0 0.0] [1.0 1.0 1.0] :rudder)]
      (mapcat
        (fn [[side x]]
          [(component (keyword (str "engine-" side "-" x)) engine-pod-mesh [(* side x) 2.0 -5.9])
           (component (keyword (str "fan-" side "-" x)) engine-fan-mesh [(* side x) 2.0 -8.86] [0.0 0.0 0.0] [1.0 1.0 1.0] :fan)])
        (for [side [-1.0 1.0] x [10.7 16.5]] [side x]))
      [(component :gear-1 main-gear-mesh [-8.5 0.0 6.4] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-main)
       (component :gear-2 main-gear-mesh [8.5 0.0 6.4] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-main)
       (component :gear-3 main-gear-mesh [-13.3 0.0 4.9] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-main)
       (component :gear-4 main-gear-mesh [13.3 0.0 4.9] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-main)
       (component :nose-gear nose-gear-mesh [0.0 0.0 -25.0] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-nose)
       (component :gear-door-l (box-mesh 1.1 0.12 5.5 b-blue) [-8.5 1.0 5.8] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-door)
       (component :gear-door-r (box-mesh 1.1 0.12 5.5 b-blue) [8.5 1.0 5.8] [0.0 0.0 0.0] [1.0 1.0 1.0] :gear-door)])))

(def aircraft-components
  {:prop procedural-piper-components
   :jet jet-components
   :b747 b747-components})
