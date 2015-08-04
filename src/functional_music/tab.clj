(ns functional-music.tab
  (:require [clojure.string :as s]))

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

