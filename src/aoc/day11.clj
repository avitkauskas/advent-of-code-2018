(ns aoc.day11)

(def grid-serial 7803)
(def grid-size 300)

(defn hundreds-digit [n]
  (-> n (quot 100) (rem 10)))

(defn cell-power [x y]
  (let [rack-id (+ x 10)
        init-power (* rack-id y)
        incr-power (+ init-power grid-serial)
        mult-power (* incr-power rack-id)
        digit (hundreds-digit mult-power)
        final-power (- digit 5)]
    final-power))

(def grid
  (for [y (range 1 (inc grid-size))
        x (range 1 (inc grid-size))]
    (cell-power x y)))

(defn row-sets [size grid]
  (->> grid
       (partition grid-size)
       (partition size 1)))

(defn cell-sets [size row-set]
  (->> (map #(partition size 1 %) row-set)
       (apply map concat)))

(defn squares [size grid]
  (->> grid
       (row-sets size)
       (map #(cell-sets size %))
       (apply concat)))

(defn max-power-indexed [size squares]
  (->> (map-indexed (fn [i powers] [i (apply + powers)]) squares)
       (apply max-key second)
       (#(conj % size))))

(defn idx-max-size [size]
  (->> grid
       (squares size)
       (max-power-indexed size)))

(defn index->coords [[idx _ size]]
  (let [squares-in-row (inc (- grid-size size))
        ix (rem idx squares-in-row)
        iy (quot idx squares-in-row)]
    [(inc ix) (inc iy) size]))

(defn part-1 []
  (-> (idx-max-size 3) index->coords))

(defn part-2 []
  (->> (map #(idx-max-size %) (range 1 (inc grid-size)))
       (apply max-key second)
       index->coords))

(comment
  (part-1)
  #_(part-2)); takes ages → needs refactoring)
  
