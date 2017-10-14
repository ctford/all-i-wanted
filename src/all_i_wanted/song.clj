(ns all-i-wanted.song
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :as drums]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.temperament :as temperament]
            [leipzig.chord :as chord]))

; Generic machinery
(defsynth walker [out-bus 0 freq 0.5]
  (out:kr out-bus (lf-noise1:kr freq)))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk))
(def resonance (mul-add (in:kr random-walk) 300 1400))

(defn ? [& parts]
  (rand-nth parts))

(defn !? [part]
  (? [] part))

(defn !!? [part]
  (? [] part part))

; Instruments
(definst bass [freq 110 volume 1.0 dur 4]
  (let [decay (min 2 (- dur 0.5))]
    (-> (saw freq)
        (+ (* 0.1 (brown-noise)))
        (rlpf resonance (line:kr 0.3 0.1 4))
        (lpf 1800)
        (+ (sin-osc (* freq)))
        (* (square 3))
        (* (env-gen (perc 0.01 decay) :action FREE))
        (* volume 0.2))))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (rlpf (+ resonance 500))
      (rhpf (* 200 (+ 1 (square 9))) 0.1)
      (* (env-gen (adsr 0.001 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

(definst keen [freq 440 dur 1 volume 1.0 cutoff 800]
  (-> (square (+ freq (* 3 (sin-osc 9))))
      (rlpf (+ resonance cutoff))
      (lpf cutoff)
      (* (env-gen (adsr 0.08 0.2 0.1) (line:kr 1 0 dur) :action FREE))
      (* volume)))

(definst green [freq 440 dur 1 volume 1.0 cutoff 800]
  (-> (sin-osc (+ freq (* 3 (sin-osc 3))))
      (clip2 0.4)
      (rlpf (+ resonance cutoff) 0.1)
      (* (env-gen (adsr 0.001 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* volume)))

; Arrangement
(defmethod live/play-note :bass [{hertz :pitch}] (bass hertz))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))
(defmethod live/play-note :melody [{hertz :pitch seconds :duration}] (some-> hertz (keen seconds)))
(defmethod live/play-note :voice [{hertz :pitch seconds :duration}] (some-> hertz (green seconds :cutoff 1800)))
(defmethod live/play-note :drums [{hertz :pitch drum :drum}]
  (case drum
    :kick (drums/kick2 hertz)
    :hat  (drums/kick2 (* 2/3 hertz) :sustain 0.08 :noise 0.1)
    :hot  (drums/kick2 hertz :sustain 0.008 :noise 0.1)
    :hit  (drums/kick2 (* 3/2 hertz) :sustain 0.8 :noise 0.9)))

; Composition
(def bassline
  (->> (phrase (repeat 4) [0 3 1.5 4])
       (where :pitch (comp scale/lower scale/lower))
       (all :part :bass)))

(def alt-bass
  (->> (phrase (repeat 2) [0 -3 -4 -3 -4 -3 -7])
       (where :pitch scale/lower)
       (all :part :bass)))

(def accompaniment
  (->>
    (phrase (cycle [3 1]) [7 4 7 5 5.5 4 6 4])
    (where :pitch scale/lower)
    (all :part :accompaniment)))

(def high-accompaniment (->> accompaniment (where :pitch scale/raise)))

(def accompaniment2
  (->>
    (phrase (repeat 1/2) (concat (repeat 28 7) (repeat 4 6)))
    (with (phrase (repeat 1) (mapcat repeat [8 4 4] [14 12.5 15])))
    (all :part :accompaniment)))

(def melody
  (->>
    (phrase (cycle [3 1/2 1/2]) [2 1 0 3 1 0 -1.5 -1 0])
    (then (phrase [2 2] [1 3]))
    (all :part :melody)))

(def drums
  (->>
    (phrase (cycle [1 1 1/2 1/2 1]) (repeat 20 -14))
    (having :drum (cycle [:kick :kick :hat :hat :hit]))
    (all :part :drums)))

(def drumz
  (->>
    (phrase (repeat 15 1) (repeat -21))
    (after 1/2)
    (having :drum (repeat :hot))
    (all :part :drums)))

(def voice
  (->>
    (phrase (cycle [1/2 1/4 1/4 1/2 1/2 1/4 1/2 1/2 1/4]) (cycle [2 -3 0 2 -3 0 -3 2]))
    (with (phrase (repeat 1/4) (repeat 0)))
    (take-while #(-> % :time (< 16)))
    (where :pitch scale/raise)
    (all :part :melody)))

(def voiceX
  (->>
    (phrase (cycle [3 1/2 1/2]) [[0 4] 5 7 [0 4] 5 7 9 8 7 7 nil nil])
    (then (phrase (cycle [3 1/2 1/2]) [[0 4] 5 7 [0 4] 5 4 [0 2] 1 0 0 nil nil]))
    (where :pitch scale/raise)
    (all :part :voice)))

; Track
(def track
  (->>
    bassline
    (then (times 2 (with bassline drums       accompaniment melody)))
    (then (times 2 (with alt-bass drums voice)))
    (then (times 2 (with bassline drums drumz accompaniment melody)))
    (then (times 2 (with bassline drums drumz accompaniment accompaniment2)))
    (then (with voiceX (times 2 (with alt-bass drums voice))))
    (then (times 2 (with bassline drumz accompaniment accompaniment2)))
    (then (with voiceX (times 2 (with alt-bass drums voice))))
    (then (times 2 (with bassline high-accompaniment melody)))
    (then (times 2 (with accompaniment accompaniment2)))
    (then (with (take 1 accompaniment) (take 1 accompaniment2)))
    ;(with (!!? drums) (!? drumz) (!? accompaniment) (!? accompaniment2) (!? melody))
    (where :pitch (comp temperament/equal scale/F scale/major))
    (tempo (bpm 90))))

(defn -main []
  (live/play track))

(comment
  ; Loop the track, allowing live editing.
  (live/jam (var track))
  (live/play track)
  (recording-start "all-i-wanted.wav")
  (recording-stop)
)
