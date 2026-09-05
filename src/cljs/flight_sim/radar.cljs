(ns flight-sim.radar
  (:require [flight-sim.flight :as flight]
            [flight-sim.geometry :as geo]
            [flight-sim.heightfield :as hf]
            [flight-sim.math :as m]))

(def radar-px 192)
(def radar-res 104)
(def radar-range 3000.0)
(def rebuild-seconds 0.25)

(defonce ^:private canvas* (atom nil))
(defonce ^:private ctx* (atom nil))
(defonce ^:private terrain-canvas* (atom nil))
(defonce ^:private terrain-ctx* (atom nil))
(defonce ^:private terrain-image* (atom nil))
(defonce ^:private timer* (atom 0.0))

(def ^:private ramp
  [{:h (- hf/water 1.0) :c [24.0 58.0 92.0]}
   {:h 4.0 :c [96.0 88.0 58.0]}
   {:h 14.0 :c [46.0 84.0 42.0]}
   {:h 70.0 :c [30.0 60.0 32.0]}
   {:h 150.0 :c [84.0 78.0 66.0]}
   {:h 230.0 :c [128.0 124.0 118.0]}
   {:h 300.0 :c [210.0 216.0 224.0]}])

(defn- lerp [a b t] (+ a (* (- b a) t)))

(defn- ramp-color [h]
  (cond
    (<= h (:h (first ramp))) (:c (first ramp))
    (>= h (:h (last ramp))) (:c (last ramp))
    :else
    (let [[a b] (first (filter (fn [[a b]] (<= (:h a) h (:h b)))
                               (partition 2 1 ramp)))
          t (/ (- h (:h a)) (- (:h b) (:h a)))
          [ar ag ab] (:c a)
          [br bg bb] (:c b)]
      [(lerp ar br t) (lerp ag bg t) (lerp ab bb t)])))

(defn attach-canvas! [canvas]
  (reset! canvas* canvas)
  (if canvas
    (let [ctx (.getContext canvas "2d")
          terrain (.createElement js/document "canvas")]
      (set! (.-width canvas) radar-px)
      (set! (.-height canvas) radar-px)
      (set! (.-width terrain) radar-res)
      (set! (.-height terrain) radar-res)
      (let [tctx (.getContext terrain "2d")]
        (reset! ctx* ctx)
        (reset! terrain-canvas* terrain)
        (reset! terrain-ctx* tctx)
        (reset! terrain-image* (.createImageData tctx radar-res radar-res))
        (reset! timer* 0.0)))
    (do
      (reset! ctx* nil)
      (reset! terrain-canvas* nil)
      (reset! terrain-ctx* nil)
      (reset! terrain-image* nil))))

(defn- rebuild-terrain! [f]
  (when-let [tctx @terrain-ctx*]
    (let [image @terrain-image*
          data (.-data image)
          airborne (and (not (:on-ground f)) (> (flight/agl hf/height-at f) 50.0))
          step (/ (* radar-range 2.0) (dec radar-res))
          [px py pz] (:pos f)]
      (dotimes [iy radar-res]
        (let [wz (+ (- pz radar-range) (* iy step))]
          (dotimes [ix radar-res]
            (let [wx (+ (- px radar-range) (* ix step))
                  h (hf/height-at wx wz)
                  [r g b] (ramp-color h)
                  near? (and airborne (> h (- py 60.0)))
                  o (* 4 (+ (* iy radar-res) ix))]
              (if near?
                (do
                  (aset data o (min 255.0 (+ (* r 0.5) 130.0)))
                  (aset data (+ o 1) (* g 0.35))
                  (aset data (+ o 2) (* b 0.35)))
                (do
                  (aset data o r)
                  (aset data (+ o 1) g)
                  (aset data (+ o 2) b)))
              (aset data (+ o 3) 255)))))
      (.putImageData tctx image 0 0))))

(defn- draw-runways! [ctx f]
  (let [c (/ radar-px 2.0)
        [px _ pz] (:pos f)
        sites (geo/nearby-runways px pz)
        markers (cons {:x 0.0 :z 0.0 :angle 0.0 :len hf/runway-length} sites)]
    (set! (.-strokeStyle ctx) "#e8e8e8")
    (set! (.-lineWidth ctx) 2.0)
    (doseq [site markers]
      (let [rx (* (/ (- (:x site) px) radar-range) c)
            rz (* (/ (- (:z site) pz) radar-range) c)]
        (when (and (<= (m/abs rx) (* c 1.5))
                   (<= (m/abs rz) (* c 1.5)))
          (let [half-len (max 5.0 (min 14.0 (* (/ (* (:len site) 0.5) radar-range) c)))
                ux (* (m/sin (:angle site)) half-len)
                uz (* (m/cos (:angle site)) half-len)]
            (.beginPath ctx)
            (.moveTo ctx (- rx ux) (- rz uz))
            (.lineTo ctx (+ rx ux) (+ rz uz))
            (.stroke ctx)))))))

(defn- draw-nearest-arrow! [ctx f nr]
  (let [c (/ radar-px 2.0)
        [px _ pz] (:pos f)
        dx (- (:x nr) px)
        dz (- (:z nr) pz)
        d (m/hypot2 dx dz)]
    (when (> d 150.0)
      (let [dxn (/ dx d)
            dzn (/ dz d)
            pxp (- dzn)
            pzp dxn
            tip-r (- c 4.0)
            base-r (- c 17.0)]
        (set! (.-fillStyle ctx) "#ffd97a")
        (.beginPath ctx)
        (.moveTo ctx (* dxn tip-r) (* dzn tip-r))
        (.lineTo ctx (+ (* dxn base-r) (* pxp 6.0))
                     (+ (* dzn base-r) (* pzp 6.0)))
        (.lineTo ctx (- (* dxn base-r) (* pxp 6.0))
                     (- (* dzn base-r) (* pzp 6.0)))
        (.closePath ctx)
        (.fill ctx)))))

(defn- draw! [f nr]
  (when (and @ctx* @terrain-canvas*)
    (let [ctx @ctx*
          terrain @terrain-canvas*
          s (double radar-px)
          c (/ s 2.0)]
      (.clearRect ctx 0 0 s s)
      (.save ctx)
      (.beginPath ctx)
      (.arc ctx c c (- c 1.0) 0.0 (* 2.0 m/pi))
      (.clip ctx)
      (set! (.-fillStyle ctx) "#060e18")
      (.fillRect ctx 0 0 s s)
      (.translate ctx c c)
      (.rotate ctx (- (:yaw f)))
      (.drawImage ctx terrain (- c) (- c) s s)
      (draw-runways! ctx f)
      (draw-nearest-arrow! ctx f nr)
      (.restore ctx)

      (set! (.-strokeStyle ctx) "rgba(77,240,192,0.25)")
      (set! (.-lineWidth ctx) 1.0)
      (doseq [fraction [(/ 1.0 3.0) (/ 2.0 3.0)]]
        (.beginPath ctx)
        (.arc ctx c c (* c fraction) 0.0 (* 2.0 m/pi))
        (.stroke ctx))

      (set! (.-strokeStyle ctx) "rgba(77,240,192,0.45)")
      (doseq [deg (range 0 360 30)]
        (let [rad (m/rad deg)
              r1 (- c 1.0)
              r2 (- c (if (zero? (mod deg 90)) 8.0 5.0))]
          (.beginPath ctx)
          (.moveTo ctx (+ c (* (m/sin rad) r1)) (- c (* (m/cos rad) r1)))
          (.lineTo ctx (+ c (* (m/sin rad) r2)) (- c (* (m/cos rad) r2)))
          (.stroke ctx)))

      (set! (.-fillStyle ctx) "#ffd97a")
      (.beginPath ctx)
      (.moveTo ctx c (- c 7.0))
      (.lineTo ctx (- c 5.0) (+ c 5.0))
      (.lineTo ctx c (+ c 2.0))
      (.lineTo ctx (+ c 5.0) (+ c 5.0))
      (.closePath ctx)
      (.fill ctx)

      (set! (.-fillStyle ctx) "rgba(77,240,192,0.8)")
      (set! (.-font ctx) "9px monospace")
      (set! (.-textAlign ctx) "center")
      (.fillText ctx (str (.toFixed (/ radar-range 1000.0) 1) " KM") c (- s 8.0)))))

(defn update! [f nr dt]
  (when @ctx*
    (swap! timer* - dt)
    (when (<= @timer* 0.0)
      (reset! timer* rebuild-seconds)
      (rebuild-terrain! f))
    (draw! f nr)))
