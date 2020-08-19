(ns jam.box.unit)

(defn GiB
  "Converts GiB to bytes."
  [gigi-bytes]
  (* gigi-bytes
     (Math/pow 2 (* 10 3))))

(defn ->GiB
  "Converts bytes to GiB."
  [bytes]
  (/ bytes
     (Math/pow 2 (* 10 3))))
#_ (pprint (->GiB (* 2 1024 1024 1024)))

(defn ->GB
  "Converts bytes to GB."
  [bytes]
  (/ bytes
     (Math/pow 10 (* 3 3))))
#_ (pprint (->GB (* 2 1000 1000 1000)))
