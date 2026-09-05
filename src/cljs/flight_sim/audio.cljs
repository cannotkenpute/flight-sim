(ns flight-sim.audio)

(def ^:private asset-paths
  {:prop "/assets/sfx/prop-loop.mp3"
   :jet "/assets/sfx/jet-loop.mp3"
   :pullup "/assets/sfx/pull-up.mp3"
   :stall "/assets/sfx/stall.mp3"})

(defonce ^:private state*
  (atom {:ctx nil
         :buffers {}
         :engines {}
         :loading? false
         :muted? false
         :playing {}
         :last-played {}
         :last-spoken {}
         :voice nil}))

(defn muted? [] (:muted? @state*))

(defn- audio-context! []
  (or (:ctx @state*)
      (let [ctor (or (.-AudioContext js/window)
                     (.-webkitAudioContext js/window))]
        (when ctor
          (let [ctx (.construct js/Reflect ctor #js [])]
            (swap! state* assoc :ctx ctx)
            ctx)))))

(defn- create-engine! [key]
  (when-let [ctx (audio-context!)]
    (when-not (get-in @state* [:engines key])
      (let [gain (.createGain ctx)]
        (set! (.. gain -gain -value) 0.0)
        (.connect gain (.-destination ctx))
        (swap! state* assoc-in [:engines key] {:gain gain :source nil})))))

(defn- load-buffer! [key path]
  (when-let [ctx (audio-context!)]
    (-> (js/fetch path)
        (.then (fn [response]
                 (if (.-ok response)
                   (.arrayBuffer response)
                   (throw (js/Error. (str "Audio request failed: " path))))))
        (.then (fn [array-buffer] (.decodeAudioData ctx array-buffer)))
        (.then (fn [buffer]
                 (swap! state* assoc-in [:buffers key] buffer)
                 buffer))
        (.catch (fn [error]
                  (js/console.warn "flight audio load failed" path error))))))

(defn- choose-voice! []
  (when (exists? js/speechSynthesis)
    (let [voices (.getVoices js/speechSynthesis)
          english (first (filter #(re-find #"^en" (or (.-lang %) ""))
                                 (array-seq voices)))]
      (swap! state* assoc :voice english))))

(defn init! []
  (choose-voice!)
  (when (exists? js/speechSynthesis)
    (.addEventListener js/speechSynthesis "voiceschanged" choose-voice!))
  :ready)

(defn unlock! []
  (when-let [ctx (audio-context!)]
    (when (= "suspended" (.-state ctx))
      (.resume ctx))
    (when-not (:loading? @state*)
      (swap! state* assoc :loading? true)
      (create-engine! :prop)
      (create-engine! :jet)
      (doseq [[key path] asset-paths]
        (load-buffer! key path))))
  :unlocked)

(defn- ensure-source! [key]
  (let [ctx (:ctx @state*)
        eng (get-in @state* [:engines key])
        buffer (get-in @state* [:buffers key])]
    (when (and ctx eng buffer (nil? (:source eng)))
      (let [src (.createBufferSource ctx)]
        (set! (.-buffer src) buffer)
        (set! (.-loop src) true)
        (.connect src (:gain eng))
        (.start src)
        (swap! state* assoc-in [:engines key :source] src)))))

(defn update-engine! [aircraft-key throttle speed]
  (when-let [ctx (:ctx @state*)]
    (let [active-key (if (= aircraft-key :prop) :prop :jet)
          now (.-currentTime ctx)]
      (doseq [key [:prop :jet]]
        (ensure-source! key)
        (when-let [{:keys [gain source]} (get-in @state* [:engines key])]
          (when source
            (let [[target-gain rate]
                  (if (and (= key active-key) (not (:muted? @state*)))
                    (if (= key :prop)
                      [(+ 0.12 (* throttle 0.55))
                       (+ 0.85 (* throttle 0.55))]
                      [(+ 0.10 (* throttle 0.70))
                       (+ 0.60 (* throttle 0.75) (* (min speed 1100.0) 0.00018))])
                    [0.0 1.0])]
              (.setTargetAtTime (.-gain gain) target-gain now 0.12)
              (.setTargetAtTime (.-playbackRate source) rate now 0.20))))))))

(defn play-once!
  ([key min-gap-ms] (play-once! key min-gap-ms 1.0))
  ([key min-gap-ms volume]
   (let [{:keys [ctx buffers muted? playing last-played]} @state*
         now (js/performance.now)
         last (double (get last-played key 0.0))]
     (cond
       (or (nil? ctx) muted?) true
       (nil? (get buffers key)) false
       (get playing key) true
       (< (- now last) min-gap-ms) true
       :else
       (let [src (.createBufferSource ctx)
             gain (.createGain ctx)]
         (set! (.-buffer src) (get buffers key))
         (set! (.. gain -gain -value) volume)
         (.connect src gain)
         (.connect gain (.-destination ctx))
         (swap! state* assoc-in [:playing key] true)
         (swap! state* assoc-in [:last-played key] now)
         (set! (.-onended src)
               (fn [] (swap! state* assoc-in [:playing key] false)))
         (.start src)
         true)))))

(defn crash! []
  (let [{:keys [ctx muted?]} @state*]
    (when (and ctx (not muted?))
      (let [dur 1.6
            frames (long (* (.-sampleRate ctx) dur))
            buffer (.createBuffer ctx 1 frames (.-sampleRate ctx))
            data (.getChannelData buffer 0)]
        (dotimes [i frames]
          (let [t (/ i frames)]
            (aset data i (* (- (* (js/Math.random) 2.0) 1.0)
                            (js/Math.pow (- 1.0 t) 2.2)))))
        (let [src (.createBufferSource ctx)
              filter (.createBiquadFilter ctx)
              gain (.createGain ctx)
              now (.-currentTime ctx)]
          (set! (.-buffer src) buffer)
          (set! (.-type filter) "lowpass")
          (.setValueAtTime (.-frequency filter) 3000.0 now)
          (.exponentialRampToValueAtTime (.-frequency filter) 120.0 (+ now dur))
          (set! (.. gain -gain -value) 0.9)
          (.connect src filter)
          (.connect filter gain)
          (.connect gain (.-destination ctx))
          (.start src))))))

(defn speak! [text min-gap-ms]
  (when (and (exists? js/speechSynthesis) (not (:muted? @state*)))
    (let [now (js/performance.now)
          last (double (get-in @state* [:last-spoken text] 0.0))]
      (when (>= (- now last) min-gap-ms)
        (swap! state* assoc-in [:last-spoken text] now)
        (.cancel js/speechSynthesis)
        (let [utterance (js/SpeechSynthesisUtterance. text)]
          (when-let [voice (:voice @state*)]
            (set! (.-voice utterance) voice))
          (set! (.-rate utterance) 1.05)
          (set! (.-pitch utterance) 0.85)
          (set! (.-volume utterance) 0.95)
          (.speak js/speechSynthesis utterance))))))

(defn toggle-mute! []
  (let [muted (not (:muted? @state*))]
    (swap! state* assoc :muted? muted)
    (when (and muted (exists? js/speechSynthesis))
      (.cancel js/speechSynthesis))
    muted))
