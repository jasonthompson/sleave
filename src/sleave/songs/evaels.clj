(ns sleave.songs.evaels
  (:use
   [overtone.live]
   [overtone.inst.drum]))

;; Modified version of ctford's shudder
;; https://github.com/ctford/whelmed/blob/master/src/whelmed/instrument.clj
(definst shudder [freq 440 vibrato 6]
  (let [envelope (env-gen (perc 2 3) :action FREE)]
    (*
     (* envelope (sin-osc:kr vibrato))
     (sin-osc freq))))

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn note->hz [music-note]
  (midi->hz (note music-note)))

(defn shudder2 [music-note]
  (shudder (midi->hz (note music-note))))

(defn saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))


(defn play-chord [a-synth a-chord]
  (doseq [note a-chord] (a-synth note)))

;; a-chord is a vector of notes

(play-chord saw2 (chord :C4 :major))

(defonce metro (metronome 120))


(metro)

(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num))(play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num))(play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num))(play-chord (chord :A3 :minor)))
  (at (m (+ 12 beat-num))(play-chord (chord :G3 :major)))
  (apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) []))

;; Generalize so any synth can be passed into progression

(defn chord-progression [m beat-num a-synth]
  (at (m (+ 0 beat-num))(play-chord a-synth (chord :C4 :major)))
  (at (m (+ 4 beat-num))(play-chord a-synth (chord :G3 :major)))
  (at (m (+ 8 beat-num))(play-chord a-synth (chord :A3 :minor)))
  (at (m (+ 12 beat-num))(play-chord  a-synth(chord :G3 :major)))
  (apply-at (m (+ 16 beat-num)) chord-progression m (+ 16 beat-num) a-synth []))

(chord-progression metro (metro) shudder2)

(chord-progression metro (metro) saw2)


(stop)

(definst easy [freq 440]
  (sin-osc freq))

(easy 440)

(def partials [0.99 1.1 1.4])

(defsynth easy2 [freq 440 vibrato 10 attack 0.4 sustain 0.4 release 2]
  (let [lfo (sin-osc:kr vibrato)
        osc1 (sin-osc freq)
        osc2 (sin-osc (* freq (nth partials 0)))
        osc3 (sin-osc (* freq (nth partials 1)))
        osc4 (sin-osc (* freq (nth partials 2)))
        env1 (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
        env2 (env-gen (lin-env attack (* sustain 0.96) release) 1 1 0 1 FREE)
        env3 (env-gen (lin-env attack (* sustain 1.2) release) 1 1 0 1 FREE)
        env4 (env-gen (lin-env attack (* sustain 1.1) release) 1 1 0 1 FREE)]
    (out 0 (pan2 (mix [(* osc1 env1) (* osc2 env2) (* osc3 env3) (* osc4 env4)])))))

(defsynth blippish [freq 440 attack 0.01 release 1 level 1 curve -4]
  (let [env (env-gen (perc attack release level curve) :action FREE)
        snd (sin-osc freq)
        snd (distort snd)]
    (out 0 (pan2 (* env snd)))))

(blippish 440)
(easy2 440)
(stop)

(def metro (metronome 100))

(metro)

(defn play [m beat-num]
  (at (m (+ beat-num 0)) (easy2 (midi->hz (note "C3"))))
  (at (m (+ beat-num 4)) (easy2 (midi->hz (note "C3"))))
  (at (m (+ beat-num 8)) (easy2 (midi->hz (note "C3"))))
  (at (m (+ beat-num 12)) (easy2 (midi->hz (note "C3"))))
  (apply-at (m (+ beat-num 16)) play metro (+ 16 beat-num) []))

(play metro (metro))

(defn play-blippish [m beat-num]
  (at (m (+ beat-num 0)) (blippish (midi->hz (note "C4"))))
  (at (m (+ beat-num 0.5)) (blippish (midi->hz (note "G3"))))
  (at (m (+ beat-num 1)) (blippish (midi->hz (note "B3"))))
  (at (m (+ beat-num 1.5)) (blippish (midi->hz (note "F3"))))
  (at (m (+ beat-num 3)) (blippish (midi->hz (note "B4"))))
  (apply-at (m (+ beat-num 8)) play-blippish metro (+ beat-num 8) []))

(play metro (metro))
(play-blippish metro (metro))

(stop)
