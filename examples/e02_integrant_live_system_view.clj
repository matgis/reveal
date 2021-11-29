(ns e02-integrant-live-system-view
  (:require [integrant.core :as ig]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as response]
            [clojure.main :as m]
            [vlaaad.reveal :as r])
  (:import [org.eclipse.jetty.server Server]))

;; Integrant is a micro-framework for building applications with data-driven
;; architecture. It manages application lifecycle: starting and stopping the system and
;; wiring dependencies.
;; In this example we create a monitor for our developed system that shows its current
;; state and controls to start and stop it.

;; this was taken directly from integrant readme: https://github.com/weavejester/integrant

(def config
  {:adapter/jetty {:port 8080, :handler (ig/ref :handler/greet)}
   :handler/greet {:name "Alice"}})

(defmethod ig/init-key :adapter/jetty [_ {:keys [handler] :as opts}]
  (jetty/run-jetty handler (-> opts (dissoc :handler) (assoc :join? false))))

(defmethod ig/init-key :handler/greet [_ {:keys [name]}]
  (fn [_] (response/response (str "Hello " name))))

(defmethod ig/halt-key! :adapter/jetty [_ ^Server server]
  (.stop server))

;; This is the system that we will start/stop during development
;; it's implemented as an agent instead of atom since start/stop is
;; side-effecting, hence not retriable

(def system
  (agent nil :error-handler #(-> %2 Throwable->map m/ex-triage m/ex-str println)))

(defn start! []
  (send system (fn [state]
                 (if state
                   (throw (ex-info "Already started" {:system state}))
                   (ig/init config))))
  (await system)
  @system)

(defn stop! []
  (send system (fn [state]
                 (if state
                   (do (ig/halt! state) nil)
                   (throw (ex-info "Already stopped" {:system state})))))
  (await system)
  @system)

;; REPL controls

(comment
  (start!)
  (slurp "http://localhost:8080")
  (stop!))

;; Reveal monitor and controls: eval it and execute "view" action

{:fx/type r/observable-view
 :ref system
 :fn (fn [state]
       {:fx/type :v-box
        :children [{:fx/type r/value-view
                    :v-box/vgrow :always
                    :value state}
                   {:fx/type :h-box
                    :children [{:fx/type :button
                                :disable (not state)
                                :text "Stop"
                                :on-action (fn [_] (stop!))}
                               {:fx/type :button
                                :disable (some? state)
                                :text "Start"
                                :on-action (fn [_] (start!))}]}]})}
