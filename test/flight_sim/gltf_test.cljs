(ns flight-sim.gltf-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [flight-sim.gltf :as gltf]))

(defn- pad4 [n]
  (* 4 (js/Math.ceil (/ n 4))))

(defn- minimal-glb []
  (let [doc {:asset {:version "2.0"}
             :scene 0
             :scenes [{:nodes [0]}]
             :nodes [{:name "TestTriangle" :mesh 0}]
             :meshes [{:primitives [{:attributes {:POSITION 0 :NORMAL 1}
                                      :indices 2}]}]
             :buffers [{:byteLength 80}]
             :bufferViews [{:buffer 0 :byteOffset 0 :byteLength 36}
                           {:buffer 0 :byteOffset 36 :byteLength 36}
                           {:buffer 0 :byteOffset 72 :byteLength 6}]
             :accessors [{:bufferView 0 :componentType 5126 :count 3 :type "VEC3"}
                         {:bufferView 1 :componentType 5126 :count 3 :type "VEC3"}
                         {:bufferView 2 :componentType 5123 :count 3 :type "SCALAR"}]}
        json-bytes (.encode (js/TextEncoder.) (js/JSON.stringify (clj->js doc)))
        json-len (pad4 (.-length json-bytes))
        bin-len 80
        total (+ 12 8 json-len 8 bin-len)
        buffer (js/ArrayBuffer. total)
        view (js/DataView. buffer)
        json-out (js/Uint8Array. buffer 20 json-len)
        bin-start (+ 20 json-len)
        bin-view (js/DataView. buffer (+ bin-start 8) bin-len)]
    (.setUint32 view 0 0x46546c67 true)
    (.setUint32 view 4 2 true)
    (.setUint32 view 8 total true)
    (.setUint32 view 12 json-len true)
    (.setUint32 view 16 0x4e4f534a true)
    (.fill json-out 32)
    (.set json-out json-bytes 0)
    (.setUint32 view (+ bin-start 0) bin-len true)
    (.setUint32 view (+ bin-start 4) 0x004e4942 true)
    (doseq [[i v] (map-indexed vector [0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 0.0])]
      (.setFloat32 bin-view (* i 4) v true))
    (doseq [[i v] (map-indexed vector [0.0 0.0 1.0 0.0 0.0 1.0 0.0 0.0 1.0])]
      (.setFloat32 bin-view (+ 36 (* i 4)) v true))
    (doseq [[i v] (map-indexed vector [0 1 2])]
      (.setUint16 bin-view (+ 72 (* i 2)) v true))
    buffer))

(deftest parses-minimal-glb-into-postgraphics-mesh
  (let [asset (gltf/parse-glb (minimal-glb))
        component (first (:components asset))
        mesh (first (:primitives component))]
    (testing "scene and mesh structure"
      (is (= 1 (count (:components asset))))
      (is (= "TestTriangle" (:name component)))
      (is (= 3 (count (:vertices mesh))))
      (is (= [[0 1 2]] (:indices mesh)))
      (is (= 3 (count (:normals mesh))))
      (is (= 0.0 (:min-y asset))))
    (testing "mesh is ready for PostGraphics lighting"
      (is (= [0.0 0.0 0.0] (first (:vertices mesh))))
      (is (= [0.0 0.0 1.0] (first (:normals mesh))))
      (is (seq (:wire-edges mesh))))))
