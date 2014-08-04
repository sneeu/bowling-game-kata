(ns solution
  (:use clojure.test))


(defn split-into-frames
  "Splits a set of rolls into frames."
  [rolls]
  (cond (empty? rolls) []
        (= (first rolls) 10) (cons '(10 0) (split-into-frames (rest rolls)))
        :else (cons (take 2 rolls) (split-into-frames (drop 2 rolls)))))

(defn is-strike
  "true if the first item in rolls is a strike"
  [rolls]
  (= 10 (first rolls)))

(defn is-spare
  "true if the first two items in rolls make a spare"
  [rolls] 
  (= 10 (+ (first rolls) (second rolls))))

(defn frame-score
  "Calculates a frame's score"
  [rolls]
  (cond (is-strike rolls) (apply + (take 4 rolls))
        (is-spare rolls) (apply + (take 3 rolls))
        :else (apply + (take 2 rolls))))

(defn score 
  "Scores a collection of rolls"
  [rolls]
  (loop [frame-rolls (flatten (split-into-frames rolls))
         frames 0
         score 0]
    (if (or
          (<= (count frame-rolls) 1)
          (= frames 10))
      score
      (recur (drop 2 frame-rolls) (inc frames) (+ score (frame-score frame-rolls))))))


(deftest score-from-example
  (is (= 133 (score [1 4 4 5 6 4 5 5 10 0 1 7 3 6 4 10 2 8 6])))
  (is (= 17 (score [0 10 3 1])))
  (is (= 18 (score [10 3 1])))
  (is (= 45 (score [10 5 5 6 3])))
  (is (= 0 (score []))))


(run-tests)
