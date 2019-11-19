(ns online-scaler.events
  (:require
   [cljs.reader :refer [read-string]]
   [clojure.string :refer [split]]
   [re-frame.core :as re-frame]
   [online-scaler.db :as db]))

;;;-Init-----------------------------------------------------------------------

(re-frame/reg-event-db
 ::initialize-db
  (fn [_ _]
    db/default-db))

;;;-Upload---------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-mv-ctx 
  (fn [db [_ result]]
    ;; Save file as [["First" "line" "in" ".csv"] ["Second" "line"]]
    (assoc-in db [:mv-ctx :file] (map #(split % #", |,") 
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
          (-> db
            (assoc-in [:mv-ctx :attributes] attribute-vector)
            (assoc-in [:scaling] (apply merge
                                    (map 
                                      #(hash-map %
                                         (hash-map :measure "Nominal")) 
                                      (map first attribute-vector))))))}))

;;;-Selection------------------------------------------------------------------

(re-frame/reg-event-fx
 ::attribute-selection-down
  (fn [cofx [_ attribute]]
    (let [db    (:db cofx) 
          index (.indexOf (get-in db [:mv-ctx :attributes]) attribute)]
      {:dispatch [::set-tmp index]})))

(re-frame/reg-event-db
 ::attribute-selection-up
  (fn [db [_ attribute]]
    (let [down-index (:tmp db) 
          up-index   (.indexOf (get-in db [:mv-ctx :attributes]) attribute)
          index-list (if (> down-index up-index)
                         (conj (range up-index down-index) down-index)
                         (conj (range down-index up-index) up-index))]
      (loop [loop-db          db
             loop-index-list  index-list]
        (if (empty? loop-index-list)
            loop-db
            (recur
              (update-in 
                loop-db 
                [:mv-ctx :attributes (first loop-index-list) 1]
                not)
              (drop 1 loop-index-list)))))))

(re-frame/reg-event-fx
 ::initiate-attributes
  (fn [cofx _]
    {:dispatch [::set-panel "scale"]
     :db (let [db             (:db cofx)
               attribute-list (mapv 
                                first
                                (filter
                                  #(= (second %) true)
                                  (get-in db [:mv-ctx :attributes])))]
           (-> db
             (assoc-in [:selection :attributes] attribute-list)
             (assoc-in 
               [:selection :current-attribute] 
               (first attribute-list))))}))

;;;-Scaling--------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-current-attribute
  (fn [db [_ attribute]]
    (assoc-in db [:selection :current-attribute] attribute)))

(re-frame/reg-event-db
 ::next-attribute
  (fn [db _]
    (let [current-attribute     (get-in db [:selection :current-attribute])
          attributes            (get-in db [:selection :attributes])
          next-position         (mod
                                 (+ (.indexOf attributes current-attribute) 1)
                                 (count attributes))
          next-attribute        (nth attributes next-position)]
      (assoc-in db [:selection :current-attribute] next-attribute))))

(re-frame/reg-event-db
 ::previous-attribute
  (fn [db _]
    (let [current-attribute     (get-in db [:selection :current-attribute])
          attributes            (get-in db [:selection :attributes])
          next-position         (mod
                                 (- (.indexOf attributes current-attribute) 1)
                                 (count attributes))
          next-attribute        (nth attributes next-position)]
      (assoc-in db [:selection :current-attribute] next-attribute))))

(re-frame/reg-event-db
 ::change-measure
  (fn [db [_ measure]]
    (let [attribute (get-in db [:selection :current-attribute])]
      (assoc-in db [:scaling attribute :measure] measure))))

;;;-Export---------------------------------------------------------------------

(re-frame/reg-event-fx
 ::export-make-context
  (fn [cofx _]
    {:dispatch [::set-panel "export"]
     :db (let [db (:db cofx)
               blob (js/Blob. [(.stringify js/JSON (clj->js db))])
               url  (js/URL.createObjectURL blob)]
               (assoc-in db [:url] url))}))

;;;-Panel----------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-panel
  (fn [db [_ panel]]
    (assoc-in db [:panel] panel)))

;;;-Util-----------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-tmp
  (fn [db [_ value]]
    (assoc-in db [:tmp] value)))
