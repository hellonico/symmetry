(ns symmetry.run
  (:require
    [pyjama.io.export :refer :all]
    [pyjama.parallel :refer :all]
    [pyjama.state :refer :all]))

(defn -main []
  (let [app-state (atom {:url "http://localhost:11434" :tasks {} :processing true})]
    ;(println "Starting parallel tasks...")
    (parallel-generate
      app-state
      {:models   ["tinyllama"]
       :pre "Answer in three points the following sentence:\n %s"
       :prompts  ["Why is the sky blue" "who is indiana jones" "who are the ninja turtles"]}
      identity
      (fn [data]
        (swap! app-state assoc :processing false)
        (println "Finished")
        (export-tasks-results @app-state "pruns.csv")
        (export-tasks-results @app-state "pruns.xlsx")))
    (while (:processing @app-state)
      (Thread/sleep 500))
    (shutdown-agents)))