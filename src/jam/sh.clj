(ns jam.sh
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]
            [jam.log :refer [die]]))

(defn kw->opt
  "Converts keyword opt into a command line option string."
  [opt]
  (str "--" (name opt)))

(defn $
  [& args]
  (let [[args {:keys [directory] :as _opts}] (if (map? (last args))
                                               [(butlast args) (last args)]
                                               [args {}])

        args (->> args flatten (map str) (into []))
        pb   (ProcessBuilder. args)
        _    (when directory
               (.directory pb directory))
        _    (.inheritIO pb)
        _    (println "$" (pr-str args))
        p    (.start pb)
        _    (.waitFor p)
        exit (.exitValue p)]
    (when-not (zero? exit)
      (die (str "Command " (pr-str args) " exited with code " exit)))
    true))

;; - fg; same IO; check exit   $
;; - bg; capture IO; ?         $&
;; - fg; capture IO;           $>     $()
;; - sudo                      $#

(defn $>
  [& args]
  (let [{:keys [exit out] :as result} (apply shell/sh args)]
    (if (zero? exit)
      (str/trimr out)
      (die (str "Command " (pr-str args) " failed.") result))))
