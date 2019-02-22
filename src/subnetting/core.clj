(ns subnetting.core
	(:require [clojure.string :as s]))

(defn to-int
  "Converts string to int"
	[str]
	(if (> (count str) 0)
	  (Integer. (re-find #"\d+" str))
	  0))

(defn vec-ip->int
  "Converts vector IP to integer IP ([0 0 0 0] -> 0)"
  [v]
  (reduce #(+ %2 (bit-shift-left %1 8)) 0 v))

(defn int-ip->vec
  "Converts integer IP to vector IP (0 -> [0 0 0 0])"
  [int-ip]
  (let [mask 16rff]
    [(bit-and (bit-shift-right int-ip 24) mask)
     (bit-and (bit-shift-right int-ip 16) mask)
     (bit-and (bit-shift-right int-ip 8) mask)
     (bit-and int-ip mask)]))

(defn vec-ip->str
  "Converts vector IP to string IP ([10 0 0 0] -> 10.0.0.0)"
  [[a b c d]]
  (str a "." b "." c "." d))

(defn str-ip->vec
  "Converts string IP to vector IP (10.0.0.0 -> [0 0 0 0])"
  [str]
  (map to-int (s/split str #"\.")))

(def int-ip->str (comp vec-ip->str int-ip->vec))

(defn rev
  "Reverses vector"
  [seq]
  (-> seq sort vec rseq))

(defn exp
  "Returns the nth power of x"
  [x n]
  (int (Math/pow x n)))

(defn max-power-2
  "Returns a number x where (x >= n) && (2^k == x)  && (2^(k-1) < x)"
	[n]
	(loop [pow 2]
		(if (<= pow n)
			(recur (bit-shift-left pow 1))
			(bit-shift-right pow 1))))

(defn split-max
	[n]
	(loop [subs []
         num n
         power (max-power-2 num)]
		(if (> num 0)
			(recur (conj subs power)
             (- num power)
             (max-power-2 (- num power)))
			subs)))

;(print "Network address: ")
(def network-address [10 0 0 0])
;(def network-address (str-ip->vec (read-line)))

;(print "Prefix: ")
(def prefix 24)
;(def prefix (to-int (read-line)))

;(print "Interfaces per subnet: ")
(def subnets [1000 127])
;(def subnets (sort (filter pos? (map to-int (s/split (read-line) #"\ ")))))

(defn p-compl
  [p]
  (- 32 p))

(def max-ip (+ (vec-ip->int network-address) (exp 2 (p-compl prefix))))

(defn find-sub-prefix
	[interfaces]
	(loop [ips (+ interfaces 2)
         sub-prefix 31
         availability 2]
    (if (>= availability ips)
      sub-prefix
      (recur ips (dec sub-prefix) (* 2 availability)))))

(defn subnet
  [network-address prefix subnets]
  (let [ip (vec-ip->int network-address)
        sorted-subnets (rev subnets)
        sub-prefixes (map find-sub-prefix sorted-subnets)
        availabilities (map #(exp 2 (p-compl %)) sub-prefixes)
        subnet-ips (sort (reduce #(cons (+ (first %1) %2) %1) [ip] availabilities))
        str-subnets (map #(str (int-ip->str %1) "/" %2) (butlast subnet-ips) sub-prefixes)]
    (if (> ip  (- max-ip  (apply + availabilities)))
      [[] 0]
      [str-subnets (last subnet-ips)])))

(defn rev-subnet
  [network-address prefix subnets]
  (let [ip network-address
        sorted-subnets (sort subnets)
        sub-prefixes (map find-sub-prefix (map #(- % 2)sorted-subnets))
        subnet-ips (sort (reduce #(cons (+ (first %1) %2) %1) [ip] sorted-subnets))]
    (map #(str (int-ip->str %1) "/" %2) (butlast subnet-ips) sub-prefixes)))

(defn main
  []
  (let [[subnet-ips last-ip] (subnet network-address prefix subnets)
        ips-remaining (- max-ip last-ip)
        max-free-subnets (split-max ips-remaining)]
    (if (zero? last-ip)
      (println "Error: interfaces exceed network capacity")
      (do
        (println "Subnet IPs")
        (if (empty? subnet-ips)
          (println "none")
          (do (run! println subnet-ips)
              (println "--------------------------")
              (println "Available Networks")
              (run! println (rev-subnet last-ip prefix max-free-subnets))))))))

(main)

