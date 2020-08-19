(ns jam.log)

;; TODO console viewer (pod)
;;   - context
;;     - tag #build
;;     - nested tag #build #venus
;;   - colors
;;     - level, time, tag, data
;;   - relative time

(defn info
  [& args]
  (println "----" (apply pr-str args) "----"))

(defn die
  [& args]
  (println "ERROR: " (pr-str args))
  (System/exit 1))
