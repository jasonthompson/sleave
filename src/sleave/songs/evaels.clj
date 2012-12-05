(ns sleave.songs.evaels

  (:use
   [overtone.live]
   [overtone.inst.drum]))


(defn note->hz [music-note]
  (midi->hz (note music-note)))


(defn play-chord [a-synth a-chord]
  (doseq [note a-chord]
    (a-synth note)))


;; Generalize so any synth can be passed into progression

(defn chord-progression [m beat-num a-synth]
  (at (m (+ 0 beat-num))(play-chord a-synth (chord :C4 :major)))
  (at (m (+ 4 beat-num))(play-chord a-synth (chord :G3 :major)))
  (at (m (+ 8 beat-num))(play-chord a-synth (chord :A3 :minor)))
  (at (m (+ 12 beat-num))(play-chord  a-synth(chord :G3 :major)))
  (apply-at (m (+ 16 beat-num)) chord-progression m (+ 16 beat-num) a-synth []))


(definst easy [freq 440 duration 1 level 1]
  (let [env (env-gen (triangle duration level) :action FREE)]
    (* env (sin-osc freq))))




(def partials [0.99 1.1 1.4])

(defsynth easy2 [freq 440 vibrato 10 attack 0.4 sustain 2 release 2]
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

(easy2 440)
(blippish 440)
(stop)

(def metro (metronome 100))

(metro)

(defsynth saw-synth [freq 440 attack 0.01 release 1 level 1 curve -4]
  (let [src (saw freq)
        env (env-gen (perc attack release level curve) :action FREE)
        out-snd (rlpf (* src env) 440 0.01)]
    (out 0 (pan2 out-snd))))


(defn play [m beat-num]
  (at (m (+ beat-num 0)) (easy2 (midi->hz (note "C3"))))
  (at (m (+ beat-num 4)) (easy2 (midi->hz (note "C3"))))
  (at (m (+ beat-num 8)) (easy2 (midi->hz (note "C3"))))
  (at (m (+ beat-num 12)) (easy2 (midi->hz (note "C3"))))
  (apply-at (m (+ beat-num 16)) play metro (+ 16 beat-num) []))

;; Next Step: generalize this so it will take any chord length
(defn saw-arpeggio [m beat-num a-chord]
  (at (m (+ beat-num 0)) (saw-synth (midi->hz (nth a-chord 0))))
  (at (m (+ beat-num 0.25)) (saw-synth (midi->hz (nth a-chord 1))))
  (at (m (+ beat-num 0.5)) (saw-synth (midi->hz (nth a-chord 2))))
  (apply-at (m (+ beat-num 1)) saw-arpeggio metro (+ 1 beat-num) a-chord []))


(blippish 200)
(saw-arpeggio metro (metro) (chord :c3 :minor))
(stop)

(chord :c3 :minor)
(stop)
(ctl blippish :release 0.25)

(play metro (metro))
(play-blippish metro (metro))

(stop)
