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

(def int-ip->str
  (comp vec-ip->str int-ip->vec))

(defn rev
  "Reverses vector"
  [seq]
  (-> seq sort vec rseq))

(defn exp
  "Returns the nth power of x"
  [x n]
  (int (Math/pow x n)))

(defn smallest-pow2
  "Returns the smallest power of 2 of n"
  [n]
  (-> n
      bit-not
      inc
      (bit-and n)))

(defn factorize-2
  "Factorizes a number down to the powers of 2 it is made up of"
  [n]
  (loop [number n
         factors []]
    (if (zero? number)
      factors
      (let [factor (smallest-pow2 number)
            new-number (bit-xor number factor)]
        (recur new-number (conj factors factor))))))

(defn p-compl
  "Complement of p with respect to 32, used to translate the subnet IP requirements into prefix "
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
    (if (> ip (- max-ip  (apply + availabilities)))
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
  (print "Network address: ")
  (def network-address (str-ip->vec (read-line)))

  (print "Prefix: ")
  (def prefix (to-int (read-line)))

  (print "Interfaces per subnet: ")
  (def subnets (sort (filter pos? (map to-int (s/split (read-line) #"\ ")))))

  (let [[subnet-ips last-ip] (subnet network-address prefix subnets)
        ips-remaining (- max-ip last-ip)
        total-free-subnets (factorize-2 ips-remaining)]
    (if (zero? last-ip)
      (println "Error: interfaces exceed network capacity")
      (do
        (println "Subnet IPs")
        (if (empty? subnet-ips)
          (println "none")
          (do (run! println subnet-ips)
              (println "--------------------------")
              (println "Available Networks")
              (run! println (rev-subnet last-ip prefix total-free-subnets))))))))

;(main)

