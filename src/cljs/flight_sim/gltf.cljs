(ns flight-sim.gltf
  "Small GLB 2.0 loader that converts static glTF mesh primitives into
   dao.postgraphics :draw3d/mesh-compatible data. It intentionally owns only
   the subset used by the simulator assets (triangles, standard accessors,
   node TRS/matrices, PBR base color). No Three.js runtime is involved."
  (:require [clojure.string :as string]
            [flight-sim.geometry :as geo]
            [flight-sim.math :as m]))

(def ^:private glb-magic 0x46546c67)
(def ^:private json-chunk 0x4e4f534a)
(def ^:private bin-chunk 0x004e4942)

(defn- mat4-identity []
  [1.0 0.0 0.0 0.0
   0.0 1.0 0.0 0.0
   0.0 0.0 1.0 0.0
   0.0 0.0 0.0 1.0])

(defn- mat4-mul [a b]
  (vec
    (for [col (range 4) row (range 4)]
      (reduce +
              (for [k (range 4)]
                (* (nth a (+ (* k 4) row))
                   (nth b (+ (* col 4) k))))))))

(defn- quaternion-matrix [[x y z w]]
  (let [xx (* x x) yy (* y y) zz (* z z)
        xy (* x y) xz (* x z) yz (* y z)
        wx (* w x) wy (* w y) wz (* w z)]
    [(- 1.0 (* 2.0 (+ yy zz))) (* 2.0 (+ xy wz)) (* 2.0 (- xz wy)) 0.0
     (* 2.0 (- xy wz)) (- 1.0 (* 2.0 (+ xx zz))) (* 2.0 (+ yz wx)) 0.0
     (* 2.0 (+ xz wy)) (* 2.0 (- yz wx)) (- 1.0 (* 2.0 (+ xx yy))) 0.0
     0.0 0.0 0.0 1.0]))

(defn- trs-matrix [{:keys [translation rotation scale matrix]}]
  (if (seq matrix)
    (mapv double matrix)
    (let [[tx ty tz] (mapv double (or translation [0.0 0.0 0.0]))
          [sx sy sz] (mapv double (or scale [1.0 1.0 1.0]))
          r (quaternion-matrix (mapv double (or rotation [0.0 0.0 0.0 1.0])))
          sm [sx 0.0 0.0 0.0
              0.0 sy 0.0 0.0
              0.0 0.0 sz 0.0
              0.0 0.0 0.0 1.0]
          tm [1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              tx ty tz 1.0]]
      (mat4-mul tm (mat4-mul r sm)))))

(defn- transform-point [mat [x y z]]
  [(+ (* (nth mat 0) x) (* (nth mat 4) y) (* (nth mat 8) z) (nth mat 12))
   (+ (* (nth mat 1) x) (* (nth mat 5) y) (* (nth mat 9) z) (nth mat 13))
   (+ (* (nth mat 2) x) (* (nth mat 6) y) (* (nth mat 10) z) (nth mat 14))])

(defn- parse-chunks [array-buffer]
  (let [view (js/DataView. array-buffer)
        magic (.getUint32 view 0 true)
        version (.getUint32 view 4 true)
        total (.getUint32 view 8 true)]
    (when-not (= magic glb-magic)
      (throw (js/Error. "Not a GLB file")))
    (when-not (= version 2)
      (throw (js/Error. (str "Unsupported GLB version " version))))
    (loop [offset 12 json-data nil bin-offset nil bin-length nil]
      (if (>= offset total)
        {:json json-data
         :array-buffer array-buffer
         :bin-offset bin-offset
         :bin-length bin-length}
        (let [chunk-length (.getUint32 view offset true)
              chunk-type (.getUint32 view (+ offset 4) true)
              payload (+ offset 8)]
          (cond
            (= chunk-type json-chunk)
            (let [bytes (js/Uint8Array. array-buffer payload chunk-length)
                  text (.decode (js/TextDecoder. "utf-8") bytes)
                  clean (string/trim text)
                  parsed (js->clj (js/JSON.parse clean) :keywordize-keys true)]
              (recur (+ payload chunk-length) parsed bin-offset bin-length))

            (= chunk-type bin-chunk)
            (recur (+ payload chunk-length) json-data payload chunk-length)

            :else
            (recur (+ payload chunk-length) json-data bin-offset bin-length)))))))

(def ^:private component-info
  {5120 {:size 1 :read (fn [^js v o] (.getInt8 v o)) :max 127.0 :signed? true}
   5121 {:size 1 :read (fn [^js v o] (.getUint8 v o)) :max 255.0 :signed? false}
   5122 {:size 2 :read (fn [^js v o] (.getInt16 v o true)) :max 32767.0 :signed? true}
   5123 {:size 2 :read (fn [^js v o] (.getUint16 v o true)) :max 65535.0 :signed? false}
   5125 {:size 4 :read (fn [^js v o] (.getUint32 v o true)) :max 4294967295.0 :signed? false}
   5126 {:size 4 :read (fn [^js v o] (.getFloat32 v o true)) :float? true}})

(def ^:private type-components
  {:SCALAR 1 :VEC2 2 :VEC3 3 :VEC4 4 :MAT2 4 :MAT3 9 :MAT4 16})

(defn- normalize-component [v {:keys [float? signed? max]} normalized?]
  (if (or float? (not normalized?))
    (double v)
    (if signed?
      (max -1.0 (/ (double v) max))
      (/ (double v) max))))

(defn- read-accessor [{:keys [json array-buffer bin-offset]} accessor-index]
  (let [accessor (nth (:accessors json) accessor-index)
        buffer-view (nth (:bufferViews json) (:bufferView accessor))
        component-type (:componentType accessor)
        info (get component-info component-type)
        component-count (get type-components (keyword (:type accessor)))]
    (when-not info
      (throw (js/Error. (str "Unsupported glTF component type " component-type))))
    (when-not component-count
      (throw (js/Error. (str "Unsupported glTF accessor type " (:type accessor)))))
    (let [component-size (:size info)
          stride (or (:byteStride buffer-view) (* component-size component-count))
          start (+ bin-offset
                   (long (or (:byteOffset buffer-view) 0))
                   (long (or (:byteOffset accessor) 0)))
          count (:count accessor)
          view (js/DataView. array-buffer)]
      (mapv
        (fn [i]
          (let [base (+ start (* i stride))
                values
                (mapv (fn [j]
                        (normalize-component
                          ((:read info) view (+ base (* j component-size)))
                          info
                          (boolean (:normalized accessor))))
                      (range component-count))]
            (if (= 1 component-count) (first values) values)))
        (range count)))))

(defn- material-data [json material-index]
  (let [material (when (some? material-index) (nth (:materials json) material-index nil))
        pbr (:pbrMetallicRoughness material)
        fill (mapv double (or (:baseColorFactor pbr) [1.0 1.0 1.0 1.0]))
        metallic (double (or (:metallicFactor pbr) 0.0))
        roughness (double (or (:roughnessFactor pbr) 0.65))
        spec (+ 0.05 (* metallic 0.48))
        shininess (+ 4.0 (* (- 1.0 roughness) 92.0))]
    {:fill fill
     :material/specular [spec spec spec]
     :material/shininess shininess
     :texture-index (get-in pbr [:baseColorTexture :index])}))

(defn- primitive-mesh [parsed primitive]
  (let [json (:json parsed)
        mode (long (or (:mode primitive) 4))]
    (when-not (= mode 4)
      (throw (js/Error. (str "Only glTF TRIANGLES mode is supported; got " mode))))
    (let [attributes (:attributes primitive)
          vertices (read-accessor parsed (:POSITION attributes))
          normals (when-let [idx (:NORMAL attributes)] (read-accessor parsed idx))
          uvs (when-let [idx (:TEXCOORD_0 attributes)] (read-accessor parsed idx))
          raw-indices (if-let [idx (:indices primitive)]
                        (read-accessor parsed idx)
                        (vec (range (count vertices))))
          indices (mapv (fn [[a b c]] [(long a) (long b) (long c)])
                        (partition 3 raw-indices))
          mat (material-data json (:material primitive))
          mesh (cond-> {:vertices vertices
                        :indices indices
                        :fill (:fill mat)
                        :material/specular (:material/specular mat)
                        :material/shininess (:material/shininess mat)}
                 normals (assoc :normals normals)
                 uvs (assoc :uvs uvs))]
      (geo/with-wire-edges mesh))))

(defn- node-components [parsed node-index parent-matrix]
  (let [json (:json parsed)
        node (nth (:nodes json) node-index)
        local (trs-matrix node)
        world (mat4-mul parent-matrix local)
        own
        (if-let [mesh-index (:mesh node)]
          (let [mesh (nth (:meshes json) mesh-index)]
            [{:name (or (:name node) (str "node-" node-index))
              :matrix world
              :primitives (mapv #(primitive-mesh parsed %) (:primitives mesh))}])
          [])
        children (mapcat #(node-components parsed % world) (:children node))]
    (into own children)))

(defn parse-glb
  "Converts an ArrayBuffer GLB into {:components [...], :min-y ...}.
   Components retain their glTF local/world node matrices; simulator scene code
   wraps them in the aircraft transform and may add animation transforms."
  [array-buffer]
  (let [parsed (parse-chunks array-buffer)
        json (:json parsed)
        _ (when-not (and json (:bin-offset parsed))
            (throw (js/Error. "GLB is missing JSON or BIN chunk")))
        scene-index (long (or (:scene json) 0))
        roots (get-in json [:scenes scene-index :nodes])
        components (vec (mapcat #(node-components parsed % (mat4-identity)) roots))
        min-y
        (reduce
          (fn [best {:keys [matrix primitives]}]
            (reduce
              (fn [best primitive]
                (reduce (fn [best p]
                          (min best (nth (transform-point matrix p) 1)))
                        best
                        (:vertices primitive)))
              best
              primitives))
          ##Inf
          components)]
    {:components components
     :min-y (if (= min-y ##Inf) 0.0 min-y)}))

(defn load-glb!
  "Fetches and parses a GLB. Calls on-success with the converted model map and
   on-error with the browser exception."
  [url on-success on-error]
  (-> (js/fetch url)
      (.then (fn [response]
               (if (.-ok response)
                 (.arrayBuffer response)
                 (throw (js/Error. (str "GLB request failed: " (.-status response)))))))
      (.then (fn [buffer] (on-success (parse-glb buffer))))
      (.catch (fn [error]
                (when on-error (on-error error))))))
