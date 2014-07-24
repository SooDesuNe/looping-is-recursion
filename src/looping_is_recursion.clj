(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp))) 

(defn last-element [a-seq]
  (let [helper (fn [seq]
                 (if (empty? (rest seq))
                   (first seq)
                   (recur (rest seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                   (and (empty? a-seq) (empty? b-seq))
                     true
                   (or (empty? a-seq) (empty? b-seq))
                     false
                   (= (first a-seq) (first b-seq))
                     (recur (rest a-seq) (rest b-seq))
                   :else
                     false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0,
         seq a-seq]
    (cond 
      (empty? seq)
        nil
      (pred (first seq))
        idx
      :else
        (recur (inc idx) (rest seq)))))

(defn avg [a-seq]
  (loop [n 0,
         total 0,
         seq a-seq]
    (if (empty? seq)
      (/ total n)
      (recur (inc n) (+ total (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-elems #{},
         seq a-seq]
    (if (empty? seq)
      odd-elems
      (recur (toggle odd-elems (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [f-n 0,
         f-n-1 0,
         k 0]
      (cond
        (== k n)
          f-n
        (== k 0)
          (recur 1 0 1)
        :else
          (recur (+ f-n f-n-1) f-n (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [uniq-vec [],
         seen-elems #{},
         seq a-seq]
    (cond
      (empty? seq)
        uniq-vec
      (contains? seen-elems (first seq))
        uniq-vec
      :else
        (recur (conj uniq-vec (first seq)) 
               (conj seen-elems (first seq))
               (rest seq)))))

