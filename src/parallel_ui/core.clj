(ns parallel-ui.core
  (:gen-class)
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.pprint]
            [pyjama.io.export]
            [pyjama.io.parallelmap]
            [pyjama.parallel]
            [pyjama.screens.core :as screens])
  (:import (javafx.scene.input DragEvent TransferMode)
           (javafx.stage DirectoryChooser)))

(def ollama-url (or (System/getenv "OLLAMA_URL") "http://localhost:11434"))

(def app-state (atom {
                      :selected-row nil
                      :url          ollama-url
                      :tasks        {}}))

(defn preprocess-data [tasks]
  (let [grouped-by-prompt
        (->> tasks
             (vals)                                         ; Get all task values
             (group-by (fn [task] (get-in task [:params :prompt]))) ; Group by prompt
             (map (fn [[prompt task-list]]
                    {:prompt  prompt
                     :results (reduce (fn [acc task]
                                        (assoc acc
                                          (get-in task [:params :model]) ; Model name
                                          (:result task)))  ; Result
                                      {}
                                      task-list)})))]
    grouped-by-prompt))

(defn run-with-file [file]
  (if (and file (.endsWith (.getName file) ".csv"))
    (let [excel-data (pyjama.io.parallelmap/load-data file)
          parallel-map (pyjama.io.parallelmap/construct-map excel-data)]
      (pyjama.parallel/parallel-generate app-state
                                         parallel-map
                                         identity
                                         (fn [results]
                                           )))))

(defn drag-drop-handler [event]
  (let [db (.getDragboard event)
        files (.getFiles db)
        file (first files)]
    (swap! app-state assoc :last-file file)
    (run-with-file (:last-file @app-state))))

(defn description-cell-factory []
  (fn [text]
    {:graphic {:fx/type        :text
               :text           text
               :wrapping-width 380}}))

(defn row-clicked-handler [row]
  (swap! app-state
         update-in
         [:selected-row]
         (fn [current-selection]
           (if (= (:prompt row) current-selection)
             nil                                            ;; If the same row is clicked again, deselect it
             (:prompt row)))))                              ;; Select the clicked row

(defn times-data [data]
  (concat
    [{:key "Total Time" :value (:total-time data)}
     {:key "Count Results" :value (:count-results data)}
     {:key "Avg Per Result" :value (:avg-per-result data)}]
    (for [[model time] (:avg-per-model data)]
      {:key (str "Avg Per Model: " model) :value time})
    (for [[url time] (:avg-per-url data)]
      {:key (str "Avg Per URL: " url) :value time})))

(defn times-view []
  {:fx/type              :table-view
   :items                (times-data (:result-times @app-state))
   :min-height           150
   :column-resize-policy :constrained
   :columns              [{:fx/type            :table-column
                           :text               "Key"
                           :cell-value-factory :key
                           :pref-width         1.0
                           :max-width          Double/MAX_VALUE}
                          {:fx/type            :table-column
                           :text               "Value"
                           :cell-value-factory :value
                           :pref-width         1.0
                           :max-width          Double/MAX_VALUE}]})

(defn table-view [tasks]
  (let [rows (preprocess-data tasks)
        models (->> tasks
                    (vals)
                    (map #(get-in % [:params :model]))
                    distinct)
        total (count tasks)                                 ; Total number of tasks
        progress (count (filter :result (vals tasks)))      ; Tasks with results
        progress-percentage (if (zero? total) 0 (/ progress total))
        selected-row-id (:selected-row @app-state)
        ]
    {:fx/type    :v-box
     :spacing    10
     :max-height Double/MAX_VALUE
     :children
     [
      {:fx/type   :v-box
       :alignment :center
       :spacing   5
       :children
       [{:fx/type    :progress-bar
         :progress   progress-percentage
         :pref-width Double/MAX_VALUE}                      ; Full width of the container
        {:fx/type :label
         :text    (str progress " / " total)
         :style   {:-fx-font-size   14
                   :-fx-font-weight "bold"}}
        (times-view)
        ]}
      {:fx/type         :table-view
       :column-resize-policy :constrained
       :max-height      Double/MAX_VALUE
       :pref-width      Double/MAX_VALUE
       :columns         (concat
                          [{:fx/type            :table-column
                            :text               "Prompts"
                            :cell-factory       {:fx/cell-type :table-cell
                                                 :describe     (description-cell-factory)}
                            :cell-value-factory (fn [row] (:prompt row))
                            :pref-width         300}]
                          (map (fn [model]
                                 {:fx/type            :table-column
                                  :text               model
                                  :cell-factory       {:fx/cell-type :table-cell
                                                       :describe     (description-cell-factory)}
                                  :cell-value-factory (fn [row] (get-in row [:results model]))
                                  :pref-width         400})
                               models))

       :items           (if selected-row-id
                          (filter #(= selected-row-id (:prompt %)) rows) ;; Show only the selected row
                          rows)                             ;; Show all rows if no row is selected
       :row-factory     {:fx/cell-type :table-row
                         :describe     (fn [row-data]
                                         {
                                          :on-mouse-clicked
                                          {:event/type :row-click
                                           :row        row-data}})}
       ;:pref-height Double/MAX_VALUE
       :min-height      600
       ;:column-resize-policy TableView/CONSTRAINED_RESIZE_POLICY ; NICE !
       :v-box/vgrow     :always
       :on-drag-over    (fn [^DragEvent event]
                          (let [db (.getDragboard event)]
                            (when (.hasFiles db)
                              (doto event
                                (.acceptTransferModes (into-array TransferMode [TransferMode/COPY]))))))
       :on-drag-dropped drag-drop-handler}                  ;; Set the drag-and-drop handler here
      ]}))

(defn select-export-directory []
  (let [chooser (DirectoryChooser.)]
    (.setTitle chooser "Select Results Export Folder")
    (let [selected-folder (.showDialog chooser nil)]
      (pyjama.io.export/export-tasks-results @app-state (str (.getAbsolutePath selected-folder) "/symmetry.csv")))))

(defn handle-event [event]
  (case (:event/type event)
    :row-click (row-clicked-handler (:row event))
    :url-updated (swap! app-state assoc :url (:fx/event event))
    ::screen-selection (let [{:keys [selected]} event] (swap! app-state assoc :screen selected))
    :screen-settings/close (swap! app-state assoc :screen :main)
    :menu-item/open (select-export-directory)
    :menu-item/settings (swap! app-state assoc :screen :settings)
    :menu-item/models (swap! app-state assoc :screen :models)
    :menu-item/sample (do
                        (pyjama.parallel/parallel-generate app-state
                                                           {:models ["tinyllama"] :prompts ["Which of the ninja turtles is the strongest?"]}
                                                           identity
                                                           (fn [results]))
                        )
    :menu-item/rerun (run-with-file (:last-file @app-state))
    :menu-item/reset (swap! app-state update-in [:selected-row] (fn [_] nil))
    :menu-item/exit (do (println "Exiting...") (System/exit 0))))

(def menu-bar
  {:fx/type :menu-bar
   :menus   [{:fx/type :menu
              :text    "File"
              :items   [{:fx/type   :menu-item
                         :text      "Save"
                         :on-action {:event/type :menu-item/open}}
                        {:fx/type   :menu-item
                         :text      "Settings"
                         :on-action {:event/type :menu-item/settings}}
                        {:fx/type   :menu-item
                         :text      "Models"
                         :on-action {:event/type :menu-item/models}}
                        {:fx/type   :menu-item
                         :text      "Reset Selection"
                         :on-action {:event/type :menu-item/reset}}
                        {:fx/type   :menu-item
                         :text      "Rerun"
                         :on-action {:event/type :menu-item/rerun}}
                        {:fx/type   :menu-item
                         :text      "Sample Data"
                         :on-action {:event/type :menu-item/sample}}
                        {:fx/type   :menu-item
                         :text      "Exit"
                         :on-action {:event/type :menu-item/exit}}]}
             ]})

(def menu-bar-2
  {:fx/type :menu-bar
   :menus   [{:fx/type :menu
              :text    "Screens"
              :items   [{:fx/type   :menu-item
                         :text      "Main"
                         :on-action {:event/type ::screen-selection :selected :main}}
                        {:fx/type   :menu-item
                         :text      "Models [ps]"
                         :on-action {:event/type ::screen-selection :selected :models}}
                        {:fx/type   :menu-item
                         :text      "Settings"
                         :on-action {:event/type ::screen-selection :selected :settings}}]}]}
  )


(defn main-screen [tasks]
  {:fx/type  :v-box
   :children [
              menu-bar
              (table-view tasks)]})

(defn models-screen [models]
  {:fx/type  :v-box
   :children [menu-bar-2
              (screens/models-ps-view models)]})

(defn settings-screen []
  {:fx/type  :v-box
   :children [menu-bar-2
              (screens/settings-screen app-state)]})

(defn root-view [{:keys [tasks] :as state}]
  {:fx/type :stage
   :title   "Parallel Real-time Pyjamas"
   :width   800
   :showing true
   :height  600
   :scene   {:fx/type     :scene
             :stylesheets #{(.toExternalForm (io/resource "terminal.css"))}
             :root        (condp = (:screen @app-state)
                            :settings (settings-screen)
                            :models (models-screen (pyjama.core/ollama (:url state) :ps))
                            (main-screen tasks)
                            )}})

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root-view)
    :opts {;:fx.opt/map-event-handler #(println "Unhandled event:" %)
           :fx.opt/map-event-handler handle-event
           :app-state                app-state}))

(defn -main []
  (fx/mount-renderer app-state renderer))