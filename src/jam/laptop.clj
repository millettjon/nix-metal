(ns jam.laptop
  (:require [jam.box :as box]))

(def laptop
  {:name        "venus"
   :type        :local
   :disk-groups [#_[{:selector {:type :nvme}}
                  [:efi-system]
                  [:swap [:luks [:raid]]]
                  [:zfs/pool {:name "root_pool"
                              :datasets [{:name "root" :mount "/"}
                                         {:name "nix" :mount "/nix"}]}
                   [:luks]]]
                 [{:selector {:type :sd}}
                  [:zfs/pool {:name "data_pool"
                              :datasets [{:name "data" :mount "/data"}]}
                   #_[:luks]]]]})

(defn -main
  []
  (box/build laptop))
