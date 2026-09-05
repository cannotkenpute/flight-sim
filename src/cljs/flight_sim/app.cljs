(ns flight-sim.app
  (:require [dao.postgraphics.terminal :as terminal]
            [dao.postgraphics.web :as pg]
            [dao.stream :as ds]
            [dao.stream.ringbuffer]
            [flight-sim.audio :as audio]
            [flight-sim.event-bridge :as input]
            [flight-sim.flight :as flight]
            [flight-sim.geometry :as geo]
            [flight-sim.gltf :as gltf]
            [flight-sim.heightfield :as hf]
            [flight-sim.radar :as radar]
            [flight-sim.scene :as scene]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defonce frame-stream (ds/open! {:dao.stream/type :ringbuffer :capacity 4 :eviction-policy :evict-oldest}))
(defonce state* (r/atom nil))

(defn- seed-state []
  (let [f (flight/initial-state hf/height-at :prop)
        [x _ z] (:pos f)
        [cx cz] (geo/terrain-key x z)]
    {:flight f :terrain-mesh (geo/build-terrain-mesh cx cz) :viewport [1280.0 720.0]
     :camera-mode :chase :orbit-angle 0.0 :wireframe? false :aircraft-assets {}}))

(defn init []
  ;; CI compile harness: production app.cljs is copied into the release tree after
  ;; compiler/API conformance is established. Requiring every browser namespace here
  ;; forces Shadow-CLJS to compile the complete runtime dependency graph.
  (reset! state* (seed-state))
  (terminal/put-frame! frame-stream (scene/frame-from-state @state*))
  (when-let [el (.getElementById js/document "app")]
    (rdom/render
      [pg/postgraphics-widget frame-stream :canvas-ref input/attach-canvas! :viewport-size input/viewport]
      el)))
