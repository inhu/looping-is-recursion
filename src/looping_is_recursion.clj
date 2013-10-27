
(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [seq]
                 (if (or (= (count seq) 1) (empty? seq))
                   (first seq)
                   (recur (rest seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                  (and (empty? a-seq) (empty? b-seq))true
                  (or (empty? a-seq) (empty? b-seq)) false
                  (not (= (first a-seq) (first b-seq))) false
                  :else (recur (rest a-seq) (rest b-seq))
                  ))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pre pred
         seq a-seq
         ind 0]
    (cond
     (empty? seq) nil
     (pre (first seq)) ind
     :else (recur pre (rest seq) (inc ind)))))

(defn avg [a-seq]
  (loop [seq a-seq
         sum 0
         ind 0]
    (if (empty? seq)
      (/ sum ind)
      (recur (rest seq) (+ sum (first seq)) (inc ind)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq a-seq
         set #{}]
    (if (empty? seq)
      set
      (recur (rest seq) (toggle set (first seq))))))

(defn fast-fibo [n]
  (loop [n n
         ede 0
         nyt 1]
    (if (= n 0)
      ede
     (recur (dec n) nyt (+ nyt ede)))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         vec []
         set #{}]
    (if (or (empty? seq) (contains? set (first seq)))
      vec
     (recur (rest seq) (conj vec (first seq)) (conj set (first seq))))))

(cut-at-repetition [1 1 1 1 1])




