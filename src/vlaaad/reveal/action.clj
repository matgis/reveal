(ns vlaaad.reveal.action
  (:require [clojure.core.specs.alpha :as specs]
            [clojure.datafy :as d]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [lambdaisland.deep-diff2.diff-impl :as diff]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.io :as rio]
            [vlaaad.reveal.prefs :as prefs]
            [vlaaad.reveal.source-location :as source-location]
            [vlaaad.reveal.stream :as stream])
  (:import [clojure.lang IDeref]
           [java.awt Desktop]
           [java.io File]
           [java.net URI URL]
           [java.util.concurrent Future]))

(defonce ^:private *registry
  (atom {}))

(defn register! [id check]
  (swap! *registry assoc id check)
  id)

(s/def ::id qualified-keyword?)

(s/fdef defaction
  :args (s/cat :id ::id
               :bindings (s/every ::specs/binding-form :kind vector? :min-count 1 :max-count 2)
               :body (s/+ any?))
  :ret ::id)

(defmacro defaction [id bindings & body]
  (let [name (symbol (str (name id) "-action-body"))]
    `(register! ~id (fn ~name ~@(case (count bindings)
                                  1 `((~bindings ~@body)
                                      ([~'value ~'annotation] (~name ~'value)))
                                  2 `(([~'value] (~name ~'value nil))
                                      (~bindings ~@body)))))))

(defn collect [annotated-value]
  (let [{:keys [value annotation]} annotated-value
        actions (->> @*registry
                     (keep (fn [[id check]]
                             (try
                               (when-let [f (check value annotation)]
                                 (let [label (name id)]
                                   {:id id
                                    :label label
                                    :form (stream/horizontal
                                            (stream/raw-string "(" {:fill :util})
                                            (stream/raw-string label {:fill :symbol})
                                            stream/separator
                                            (stream/stream value annotation)
                                            (stream/raw-string ")" {:fill :util}))
                                    :invoke f}))
                               (catch Exception _)))))
        freqs (->> actions (map :label) frequencies)]
    (->> actions
         (sort-by (juxt :label :id))
         (map #(cond-> % (< 1 (freqs (:label %))) (assoc :label (str (symbol (:id %))))))
         (into []))))

(defaction ::datafy [x]
  (let [d (d/datafy x)]
    (when-not (= d x)
      (constantly d))))

(defaction ::nav [x {:vlaaad.reveal.nav/keys [coll key val]
                     :or {key ::not-found
                          val ::not-found}}]
  (let [datafied-coll (d/datafy coll)]
    (when (= datafied-coll coll)
      (cond
        (not= key ::not-found) #(d/nav datafied-coll key x)
        (not= val ::not-found) #(d/nav datafied-coll x val)))))

(defaction ::deref [v]
  (when (or (instance? IDeref v)
            (instance? Future v))
    #(deref v)))

(defaction ::meta [v]
  (when-let [m (meta v)]
    (constantly m)))

(defn- open-uri-result [^URI uri]
  (with-meta #(deref (future (.browse (Desktop/getDesktop) uri)))
             {:vlaaad.reveal.ui/ignore-action-result true}))

(defaction ::browse:external [v]
  (cond
    (instance? URI v)
    (open-uri-result v)

    (instance? URL v)
    (recur (.toURI ^URL v))

    (and (instance? File v) (.exists ^File v))
    (recur (.normalize (.toURI ^File v)))

    (and (string? v) (re-matches #"^https?://.+" v))
    (recur (URI. v))))

(defaction ::vec [v]
  (when (and v (.isArray (class v)))
    #(vec v)))

(def ^:private diffable-map-keys
  (into []
        (mapcat (fn [diffable-keys]
                  [diffable-keys
                   [(name (first diffable-keys)) (name (second diffable-keys))]]))
        [[:before :after]
         [:expected :actual]
         [:old :new]]))

(defaction ::diff [x]
  (cond
    (and (vector? x) (= 2 (count x)))
    #(apply diff/diff x)

    (map? x)
    (if (and (#{:pass :fail} (:type x))
             (contains? x :actual)
             (let [expected (:expected x ::not-found)]
               (or (nil? expected)
                   (seq? expected))))

      ;; Treat the value like a test report.
      (fn []
        (let [{:keys [expected actual]} x]
          (if (or (not= '= (first expected))
                  (not (seq? actual)))

            ;; The report is either from an uncaught exception or an (is ...)
            ;; expression that doesn't use the = operator. Wrap the diffed
            ;; values in maps with non-matching keys so that the diff algorithm
            ;; does not attempt to diff the code expressions themselves.
            (diff/diff {:expected expected} {:actual actual})

            ;; The report is from an (is ...) expression that uses the =
            ;; operator. In this situation, the :expected entry contains the
            ;; checked code expression, and the :actual expression will be
            ;; the checked code expression with the results inlined. If the
            ;; report is from a failed test, the :actual expression will be
            ;; wrapped in a (not ...) expression.
            (let [expected-form-with-actual-results (condp = (first actual)
                                                      'not (second actual)
                                                      '= actual
                                                      nil)]
              (when (and (seq? expected-form-with-actual-results)
                         (= '= (first expected-form-with-actual-results)))
                (let [[expected-result & actual-results] (rest expected-form-with-actual-results)
                      compared-result (reduce (fn [_ actual-result]
                                                (if (= expected-result actual-result)
                                                  expected-result
                                                  (reduced actual-result)))
                                              expected-result
                                              actual-results)]
                  (if (identical? expected-result compared-result)
                    expected-result
                    (diff/diff expected-result compared-result))))))))

      ;; Treat the value like a regular map. Allow users to diff map values from
      ;; commonly used "before" and "after" keys.
      (when-some [[a b] (some (fn [diffed-keys]
                                (when (every? #(contains? x %) diffed-keys)
                                  (mapv #(get x %) diffed-keys)))
                              diffable-map-keys)]
        #(diff/diff a b)))))

(defaction ::prettify [x]
  (cond
    (instance? Throwable x)
    #(stream/thrown x)

    (and (map? x)
         (vector? (:via x))
         (vector? (:trace x))
         (string? (:cause x)))
    #(stream/datafied-thrown x)))

(defaction ::why-is-this-boolean-red? [x]
  (when (and (boolean? x)
             (not (or (identical? Boolean/TRUE x)
                      (identical? Boolean/FALSE x))))
    (open-uri-result (URI. "https://vlaaad.github.io/illegal-booleans"))))

(defaction ::editor [x]
  (when-let [editor (:editor @prefs/prefs)]
    (when-let [{:keys [file line]} (source-location/of-value x)]
      (let [command-line (-> editor
                             (string/replace "{file}" (str file))
                             (string/replace "{line}" (str line)))]
        (with-meta #(rio/run-command! command-line)
                   {:vlaaad.reveal.ui/ignore-action-result true})))))

(defaction ::text [x]
  (when (string? x)
    #(stream/as x (stream/raw-string x {:fill :util}))))

(defn execute [id x ann]
  (event/daemon-future
    (if-let [action (@*registry id)]
      (if-let [invoke (action x ann)]
        (invoke)
        (throw (ex-info "Action unavailable" {:action id :value x :annotation ann})))
      (throw (ex-info "Action does not exist" {:action id})))))