(ns vlaaad.reveal.io
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io File]
           [java.lang ProcessBuilder$Redirect]
           [java.util List]))

(set! *warn-on-reflection* true)

(def ^String os (.toLowerCase (System/getProperty "os.name")))

(def app-data-dir
  (io/file
    (cond
      (.contains os "win") (System/getenv "APPDATA")
      (.contains os "mac") (str (System/getProperty "user.home") "/Library/Application Support")
      :else (System/getProperty "user.home"))
    ".reveal"))

(defn path [filename]
  (str (io/file app-data-dir filename)))

(defn- slurp-edn* [^File f]
  (when (.exists f)
    (try
      (with-open [reader (LineNumberingPushbackReader. (io/reader f))]
        (edn/read reader)))))

(defn slurp-edn [filename]
  (slurp-edn* (io/file app-data-dir filename)))

(defn update-edn [filename f & args]
  (let [file (io/file app-data-dir filename)]
    (io/make-parents file)
    (let [ret (apply f (slurp-edn* file) args)]
      (spit file (pr-str ret))
      ret)))

(defn run-command!
  "Executes a command-line string using the system shell. Returns the Process."
  ^Process [command-line]
  {:pre [(string? command-line)]}
  (let [^List command (if (.contains os "win")
                        ["cmd.exe" "/c" command-line]
                        ["/bin/sh" "-lc" command-line])]
    (-> (ProcessBuilder. command)
        (.redirectOutput ProcessBuilder$Redirect/DISCARD)
        (.redirectError ProcessBuilder$Redirect/DISCARD)
        (.start))))