(ns jam.path
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn symlink?
  [path]
  (-> path io/file .toPath
      java.nio.file.Files/isSymbolicLink))

(defn readlink
  [path]
  (when (symlink? path)
    (-> path io/file .toPath
        java.nio.file.Files/readSymbolicLink)))

(defn exists?
  [path]
  (-> path io/file .exists))

(defn basename
  [path]
  (.getName (io/file (str path))))

(defn split
  [path]
  (str/split path #"/"))
