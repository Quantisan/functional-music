(ns functional-music.tab
  (:require [clojure.string :as s]))

(defn select-html
  "Select the tab block from HTML"
  ;; NOTE: should use a CSS selector
  [s]
  (-> s
      (.split "Tabbed by")
      second
      (.split "hammer-on")
      first))

(defn filter-tab-lines
  "Only keep the lines that are tabs. Assuming that tab lines must have a '-' and '|' characters."
  ; hint: filter, defn, .contains, ->, ->>
  [coll]
  )

(defn- join-tabs
  "Join two sets of tabs together"
  [x y]
  (map (comp #(apply str %) concat) x y))

(defn continuous-tab
  "Group all the tabs into one long piece"
  [coll]
  (->> coll
       (partition 6)
       (reduce join-tabs)))

(defn remove-format [coll]
  ; hint: map, s/replace, #
  )

(defn to-string-fret [[s f]]
  [s (java.lang.Character/getNumericValue f)])

(defn tab->string-fret [e B G D A E]
  ; hint: map-indexed, vector, filter, re-seq, map
  )

(defn parse-guitar-tab [s]
  (->> s
       (s/split-lines)
       (filter-tab-lines)
       (remove-format)
       (continuous-tab)
       (apply map tab->string-fret)))

