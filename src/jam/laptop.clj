(ns jam.laptop
  (:require [jam.box :as box]))

(def laptop
  {:name        "venus"
   :type        :local
   :disk-groups [[{:selector {:type :nvme}}
                  [:efi-system {:mount "/boot"}]
                  [:swap [:luks [:raid]]]
                  [:zfs/pool {:name "root_pool"
                              :datasets [{:name "root" :mount "/"}
                                         {:name "nix" :mount "/nix"}]}
                   [:luks]]]
                 [{:selector {:type :sd}}
                  [:zfs/pool {:name "data_pool"
                              :datasets [{:name "data" :mount "/data"}]}
                   [:luks]]]]})

(defn -main
  []
  (box/build laptop))
