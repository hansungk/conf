;; Built-in functions re-done using recursion

; take -- without recur
(defn take' [n xs]
  (if (or (<= n 0) (empty? xs))
    '()
    (concat (list (first xs)) (take' (dec n) (drop 1 xs)))))

(defn search-item [coll item]
  (if (= [] coll)
    -1
    (if (= (nth coll 0) item)
      coll
      (search-item (drop 1 coll) item))))

(defn search-string [ori tar]
  (if (search-item ori (nth tar 0))
    true
    false))

;; Built-in functions re-done using recursion

; take -- without recur
(defn take' [n xs]
  (if (or (<= n 0) (empty? xs))
    '()
    (concat (list (first xs)) (take' (dec n) (drop 1 xs)))))

(defn search-item [coll item]
  (if (= [] coll)
    -1
    (if (= (nth coll 0) item)
      coll
      (search-item (drop 1 coll) item))))

(defn search-string [ori tar]
  (if (search-item ori (nth tar 0))
    true
    false))
