(ns read-log.core
 (:require [clojure.string :as str])
  (:import [java.text SimpleDateFormat]))

(defn user-map [txt]
  (let [c     (slurp (str "resources/" txt ".txt"))
        lines (str/split-lines c)]
    (reduce (fn [acc x]
              (let [s (conj (str/split x #",") txt)
                    snd (second s)
                    fst (first s)
                    gk (get acc fst)] 
                (if gk
                  (update acc fst (constantly (merge (get acc fst)
                                                    {snd {:count (inc (or (get-in acc (first [snd :count])) 0))}})))
                  (assoc acc fst [{snd {:count 1}}]))))
            {}
            lines)))

(defn -get-count [user d] 
  {(key user) (reduce (fn [acc x]
                        (let [snd (first (keys x))
                              gk (get acc snd)]
                          (if gk
                            (update acc snd 
                                    (constantly {:count (inc (get-in acc [snd :count]))
                                                  :date d}))
                            (assoc acc snd {:count 1 :date d}))))
                      {}
                      (val user))})

(defn filter-user-game [d]
  (reduce (fn [acc x]
            (assoc acc (first x) [(second x)]))
          {} 
          (concat (let [usr      (user-map d)
                        cnt-data (map #(-get-count % d) usr)] 
                    (mapcat (fn [x] 
                              (let [p (first (filter
                                              (fn [t]
                                                (let [k (key t)
                                                      v (val t)]
                                                  (when  (>= (get-in v [:count]) 2)
                                                    {k v})))
                                              (first (vals x))))]
                                (when-not (empty? p)
                                  {(first (keys x)) (assoc {} (first p) (second p))})))
                            cnt-data)))))

(defn date-difference [d1 d2]
  (let [date-format (SimpleDateFormat. "yyyy-MM-dd")]
    (- (.getDay (.parse date-format d1)) 
       (.getDay (.parse date-format d2)))))

(defn get-game-record [d1 d2]
  (let [result (merge-with into
                           (filter-user-game d1) 
                           (filter-user-game d2))]
    (map (fn [x] 
           {(key x) 
            (let [p (map #(get-in % [(first (keys %)) :date]) (val x))]
                      (when (= 1 (date-difference (second p) (first p)))
                        (first (keys (first (val x))))))})
         result)))

(defn user-details [d1 d2]
  (let [record (get-game-record d1 d2)]
    (map (fn [x] 
           (format "User :- %s with game id %s has played twice in a day in consecutive days" (first (keys x)) (first (vals x))))
         record)))


(user-details "2023-02-01" "2023-02-02")
