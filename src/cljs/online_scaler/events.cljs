(ns online-scaler.events
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require
   [cljs.reader :refer [read-string]]
   [clojure.string :refer [split]]
   [re-frame.core :as re-frame]
   [online-scaler.db :as db]))

;;; Init

(re-frame/reg-event-db
 ::initialize-db
  (fn [_ _]
    db/default-db))

;;; Upload

(re-frame/reg-event-db
 ::set-mv-ctx
  (fn [db [_ result]]
    ;; Save file as [["First" "line" "in" ".csv"] ["Second" "line"]]
    (assoc-in db [:mv-ctx :file] (map #(split % #",") 
                                      (split result #"\n")))))

(re-frame/reg-fx
  :read
  (fn [file]
    (let [reader (js/FileReader.)]
      (set! (.-onload reader) 
            #(re-frame/dispatch [::set-mv-ctx (-> % .-target .-result)]))
      (.readAsText reader file))))

(re-frame/reg-event-fx
 ::mv-ctx-file-change
  (fn [cofx [_ file]]
    {:read file
     :db (assoc-in (:db cofx) [:mv-ctx :name] (.-name file))}))

(re-frame/reg-event-db
 ::mv-ctx-header-change
  (fn [db [_ value]]
    (assoc-in db [:mv-ctx :header] value)))

;;; Selection

(re-frame/reg-event-fx
 ::initiate-selection
  (fn [cofx _]
    {:dispatch [::set-panel "select"]
     :db  (let [db                (:db cofx)
                first-line        (first (get-in db [:mv-ctx :file]))
                attribute-vector  (if (get-in db [:mv-ctx :header])
                                      ;; either use given names
                                      (mapv 
                                        #(vector % true) 
                                        first-line)
                                      ;; or generate generic ones
                                      (mapv
                                        #(vector (str "Attribute#" %) true)
                                        (range (count first-line))))]
            (assoc-in db [:mv-ctx :attributes] attribute-vector))}))

(re-frame/reg-event-db
 ::attribute-selection-swap
  (fn [db [_ attribute]]
    (let [index (.indexOf (get-in db [:mv-ctx :attributes]) attribute)]
      (update-in db [:mv-ctx :attributes] 
                    ;; Swap the Boolean for the received attribute in the db
                    #(assoc-in % [index 1] (not (second attribute)))))))

;;; Export

(re-frame/reg-event-db
 ::export-make-context
  (fn [db _]
    (let [blob (js/Blob. ["Mops"])
          url  (js/URL.createObjectURL blob)]
      (assoc-in db [:url] url))))

;;; Panel

(re-frame/reg-event-db
 ::set-panel
  (fn [db [_ panel]]
    (assoc-in db [:panel] panel)))
