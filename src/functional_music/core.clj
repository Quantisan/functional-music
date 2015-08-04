(ns functional-music.core
  (:require [overtone.live :refer :all]
            ;; using sampled-piano instead of piano to save downloading 500MB
            [overtone.inst.sampled-piano :refer :all]
            [overtone.synth.stringed :refer :all]

            [overtone.inst.synth :as synth]
            [clojure.string :as s]))

;; Clojure's doc func
;(doc odoc)

(comment
  ;; 'odoc' is from overtone, get documentation for synth/ping
  (odoc synth/ping)

  ;; so `synth` contains a bunch of instrument definitions that you can
  ;; use already. you can try them by calling e.g.
  (synth/ping)

  ;; some of them will fade away after a while.
  (synth/rise-fall-pad)

  ;; some of them don't
  (synth/vintage-bass)

  ;; switches off all sounds
  (stop))

;; other synths to try
(comment
  (synth/daf-bass)
  (synth/buzz)
  (synth/cs80lead)
  (synth/ticker)
  (synth/pad)
  (synth/bass)
  (synth/daf-bass)
  (synth/grunge-bass)
  (synth/ks1)

  (stop))

;; now let's play a little melody
;; NOTE: this fn is a hack and needs tidying up
(comment
  (let [beat-ms      250
        base-line    [:D3 :A2 :A#2 :C3
                      :D3 :F3 :G3 :C4
                      :D4 :A3 :F3 :C3
                      :D3 :F2 :G2 :A#2]
        num-measures (count base-line)]

    ;; first, define a recursive note player
    (letfn [(play-it
              ([interval instrument values]
               (play-it (now) interval instrument values 0))
              ([time interval instrument values counter]
               (when (= (mod counter 4) 0)
                 (ctl instrument :gate 0))
               (if-not (empty? values)
                 (let [value (first values)
                       next-time (+ time interval)]
                   (when value
                     (at time (instrument value)))
                   (apply-at next-time
                             play-it [next-time interval instrument (rest values) (inc counter)])))))]

      ;; baseline plays once per 4 notes
      (play-it (* 4 beat-ms)
        synth/vintage-bass
        ;; cycle the base line forever (but in reality, just for
        ;; `num-measures` times)
        (take num-measures (cycle (map note base-line))))

      (play-it beat-ms
        sampled-piano
        ;; concat the sets-of-4-note-chords into a single
        ;; seq of notes to send to play-it
        (apply concat
               (take num-measures
                     ;; for each root note, use overtone's rand-chord
                     ;; to construct a 4-note chord that spans up to
                     ;; 24 degrees
                     (map (fn [root-note] (rand-chord root-note :major 4 24))
                          (cycle base-line))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ok, let's get back to more basics and build up to other exciting things.

;; playing with the guitar synth
(comment
  (guitar-pick (guitar) 0 0)) ;; String 0 (Low E) Fret 0 (open)

;; we need a function that will play a sequence of notes; notes will
;; contain an arbitrarily long sequence of pairs of numbers denoting
;; `string-index` `fret-value`
(defn guitar-pick-note-sequence
  "play a sequence of notes [string fret] on instrument instrument
   spaced by interval milliseconds

   interval = milliseconds between each play
   noteseq = [[string-index-1 fret-index-1
   string-index-2 fret-index-2 ... ]]
   "
  [interval noteseq]
  (let [playguitar (partial guitar-pick (guitar))
        timeseq    (range (now) (+ (now) (* interval (count noteseq))) interval)]
    (doseq [[string-fret-seq timeval] (map vector noteseq timeseq)]
      (doseq [[string fret] (partition 2 string-fret-seq)]
        (playguitar string fret timeval)))))

(def playguitar320
  (partial guitar-pick-note-sequence 320))  ;; 320ms delay between picked strings

(def everybody-hurts
 "
  ;; Guitar Tab for Everybody Hurts by REM
  ;;
  ;;       D                        G
  ;; E:5]--------2-----------2------------3-----------3-----[
  ;; B:4]------3---2-------3---3--------0---0-------0---0---[
  ;; G:3]----2-------2---2-------2----0-------0---0-------0-[
  ;; D:2]--0-----------0------------------------------------[
  ;; A:1]---------------------------------------------------[
  ;; E:0]---------------------------3-----------3-----------[
")

;; now we can play multiple notes at the same time
(comment
  (playguitar320 [[0 3, 3 0, 4 0, 5 3]]))

;; we're going to fetch the tab from
;; http://tabs.ultimate-guitar.com/t/tracy_chapman/fast_car_ver8_tab.htm
(def fast-car-html (slurp "http://tabs.ultimate-guitar.com/t/tracy_chapman/fast_car_ver8_tab.htm"))
;; ok, it's working, now save the tab
(def fast-car-tab
  (-> fast-car-html
      (.split "Tabbed by")
      second
      (.split "hammer-on")
      first))

;; let's iteratively write a simple parser for this

(defn collect-into-consecutive
  "collect indexes into sets of N consecutive indexes,
  discarding all other indexes.

  N defaults to 6 (for guitar tabs)"

  ([index-list]
   (collect-into-consecutive index-list 6))

  ([index-list required-length]
   (loop [input (sort index-list)
          ;; use a buffer to store incoming consecutive indexes
          buf []
          rtn []]
     ;; we need to add buf to rtn if buf has 6 elements
     (let [buf-full? (= required-length (count buf))
           next-rtn (if buf-full? (conj rtn buf) rtn)]
       (if (empty? input)
         next-rtn
         (let [last-val (last buf)
               cur-val (first input)]
           (recur (rest input)
                  ;; we need to send a fresh buffer if:
                  ;; it is full, OR
                  ;; the incoming value is not a consecutive index
                  ;; relative to the last value in the buffer, OR
                  ;; the buffer is empty (i.e. first iteration,
                  ;; thus last-val is nil)
                  (if (or (nil? last-val) ;; also short-circuits the subtraction
                          buf-full?
                          (not= 1 (- cur-val last-val)))
                    [cur-val]           ;; fresh buffer
                    (conj buf cur-val)) ;; add to buffer
                  next-rtn))))))
  )

;; we want a function that takes a regex and returns the position of the match...
;; turns out someone asked for the exact same thing on SO!
;; http://stackoverflow.com/questions/21191045/get-string-indices-from-the-result-of-re-seq
(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
      (when (. m find)
        (cons {:start (. m start) :end (. m end) :group (. m group)}
              (lazy-seq (step))))))))

(defn parse-guitar-tab-line
  "reads a single well-formed line from the guitar tab
  and returns a seq of seq, where each inner seq takes the form of
  [index fret-value]
  index = at what index in time the pluck should occur
  fret-value = being which fret to pluck; nil if it should be rest
  "
  [line]
  ;; filter out everything before the actual guitar line "- ..." part
  ;; note: drop-while predicate needs a char (\-), not a string ("-")!
  (let [tab-string (apply str (drop-while #(not= % \-) line))]
    ;; merge hashmaps, with tab string values taking precedence
    (into
     ;; construct a hashmap of index -> stuff to play
     ;; spanning the length of the line
     (zipmap (range (count tab-string)) (cycle [nil]))
     ;; now we will have a seq of [index value] pairs
     (map
      (fn [m] [(:start m) (Integer/parseInt (:group m))])
      (re-seq-pos #"\d+" tab-string)))))

;; extract out the parsing section from play-tab() so we can use the
;; data for visualization
(defn parse-guitar-tab [guitar-tab]
  (let [index-line-list (filter
                         ;; a tab line must contain "-"
                         (fn [[_ s]] (.contains s "-"))
                         (into {} (map-indexed vector (s/split-lines guitar-tab))))
        index-to-line (into {} index-line-list)

        ;; collect-into-consecutive will return
        ;; a seq containing sets of 6 consecutive indexes [i_{n+0} ... i_{n+5}]
        ;; which we assume map to sets of 6 lines within the tab file.
        ;; in other words we are ignoring all the intervening lines, be them
        ;; comments or dynamics or major chord labels or whatnot
        ;; NOTE keys() here acts the same as (map first index-line-list)
        ;; since index-line-list is not a hash-map
        index-group-list (collect-into-consecutive (keys index-line-list))

        ;; from the 6-line groups, get lines from the original index-line maps
        ;; and sorted by increasing line indexes.
        ;; how this works:
        ;; 1. each set of 6 indexes gets passed to the lambda function
        ;; 2. using index-to-line, select the 6 matching lines as a hash-map
        ;; 3. sort by keys to ensure increasing line index order
        ;; 4. get the vals, i.e., the lines in correct order
        ;; now, we have a seq containing sets of 6 lines
        sorted-grouped-line-set-list (map #(vals (sort (select-keys index-to-line %)))
                                          ;; these will be grouped into sets of 6 lines, with indexes
                                          (sort index-group-list))
        ]

    ;; now we apply to everything
    (apply
     concat
     (map (fn [play-map]
            (let [max-beat (apply max (keys play-map))]
              (map play-map (range max-beat))))
          (for [line-set sorted-grouped-line-set-list]
            ;; map over each line in the line-set, with string index
            (apply
             merge-with concat
             ;; note the reverse
             (for [[string-index tab-line] (map-indexed vector (reverse line-set))]
               ;; now we need to parse the tab-line...
               ;; collect each string's results into into {}
               (into {}
                     ;; create index -> []
                     ;; when no string is played, for rest
                     (map (fn [[k v]] [k (if v
                                          [string-index v]
                                          [])])
                          (parse-guitar-tab-line tab-line))))))))))

;; first, filter out all lines that don't look like guitar lines
;; use map-index because we need to keep ordering information
(defn play-tab [guitar-tab]
  (let [play! (partial guitar-pick-note-sequence 80)]
    (play!
     (parse-guitar-tab guitar-tab))))

(comment
  (play-tab fast-car-tab))

