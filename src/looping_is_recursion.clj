(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp))) 

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         remaining-seq a-seq]
    (cond
      (empty? remaining-seq) nil
      (pred (first remaining-seq)) i
      :else (recur (inc i) (rest remaining-seq)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         remaining-seq a-seq]
    (if (empty? remaining-seq)
      (if (> n 0) (/ sum n) 0)
      (recur (inc n) (+ sum (first remaining-seq)) (rest remaining-seq)))))

(defn- toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [set-of-odds #{}
         remaining-seq a-seq]
    (if (empty? remaining-seq)
      set-of-odds
      (recur (toggle set-of-odds (first remaining-seq)) (rest remaining-seq)))))

(defn fast-fibo [n]
  (cond
    (zero? n) 0
    (<= n 2) 1
    :else
    (loop [fm-1 1
           fm-2 1
           i 3]
      (let [fm (+ fm-1 fm-2)]
        (if (= i n)
          fm
          (recur fm fm-1 (inc i)))))))

(defn cut-at-repetition [a-seq]
  (loop [occured-elements #{}
         to-return []
         remaining-seq a-seq]
    (let [next-elem (first remaining-seq)]
      (cond 
        (empty? remaining-seq) to-return
        (contains? occured-elements next-elem) to-return
        :else
        (recur (conj occured-elements next-elem)
             (conj to-return next-elem) 
             (rest remaining-seq))))))

