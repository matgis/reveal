(ns vlaaad.reveal.repl
  (:require [clojure.main :as m]
            [vlaaad.reveal.ui :as ui]
            [vlaaad.reveal.stream :as stream]
            [clojure.string :as str]))

(defn- stream-read [form ui]
  (ui (stream/as form
        (stream/raw-string
          (binding [*print-namespace-maps* false]
            (pr-str form))
          {:fill :util}))))

(defn- wrap-read [ui read]
  (fn [request-prompt request-exit]
    (let [ret (read request-prompt request-exit)]
      (condp = ret
        request-exit request-exit
        :repl/quit request-exit
        request-prompt request-prompt
        (doto ret (stream-read ui))))))

(defn- wrap-print [ui print]
  (fn [x]
    (ui
      (if (ui/command? x)
        x
        (stream/horizontal
          (stream/raw-string "=>" {:fill :util})
          stream/separator
          (stream/stream x))))
    (print x)))

(defn- wrap-caught [ui caught]
  (fn [ex]
    (ui (stream/as ex
          (stream/raw-string (-> ex Throwable->map m/ex-triage m/ex-str)
                             {:fill :error})))
    (caught ex)))

(defn- make-tap [ui]
  (fn [x]
    (ui
      (if (ui/command? x)
        x
        (stream/horizontal
          (stream/raw-string "tap>" {:fill :util})
          stream/separator
          (stream/stream x))))))

(defn- make-print [ui out fill]
  (PrintWriter-on
    #(do
       (ui (stream/as %
             (stream/raw-string (str/replace % #"\r?\n$" "") {:fill fill})))
       (binding [*out* out]
         (print %)
         (flush)))
    nil))

(defn- wrap-eval [ui eval]
  (let [out (make-print ui *out* :string)
        err (make-print ui *err* :error)]
    (fn [form]
      (binding [*out* out
                *err* err]
        (let [ret (eval form)]
          (flush)
          ret)))))

(defn- init []
  (apply require m/repl-requires))

(defn repl [args]
  (let [ui (ui/make :title (:title args "repl"))
        tap (make-tap ui)
        version (str "Clojure " (clojure-version))
        repl-args (-> args
                      (dissoc :title)
                      (update :init #(or % init))
                      (update :read #(wrap-read ui (or % m/repl-read)))
                      (update :eval #(wrap-eval ui (or % eval)))
                      (update :print #(wrap-print ui (or % prn)))
                      (update :caught #(wrap-caught ui (or % m/repl-caught))))]
    (ui (stream/as *clojure-version*
          (stream/raw-string version {:fill :util})))
    (println version)
    (add-tap tap)
    (try
      (apply m/repl (mapcat identity repl-args))
      (finally
        (remove-tap tap)
        (ui)))))
