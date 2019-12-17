(ns online-scaler.events
  (:require
   [cljs.reader :refer [read-string]]
   [clojure.string :refer [split]]
   [re-frame.core :as re-frame]
   [online-scaler.scaling :as scale]
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
  :read-mv-ctx
   (fn [file]
     (let [reader (js/FileReader.)]
       (set! (.-onload reader) 
             #(re-frame/dispatch [::set-mv-ctx (-> % .-target .-result)]))
       (.readAsText reader file))))

(re-frame/reg-event-fx 
 ::mv-ctx-file-change
  (fn [cofx [_ file]]
    {:read-mv-ctx file
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
                header            (get-in db [:mv-ctx :header])
                mv-ctx            (get-in db [:mv-ctx :file])
                attribute-vector  (if header
                                      ;; either use given names
                                      (mapv 
                                        #(vector (str %) true) 
                                        (first mv-ctx))
                                      ;; or generate generic ones
                                      (mapv
                                        #(vector (str "Attribute#" %) true)
                                        (range (count (first mv-ctx)))))
                nested-values     (apply map list 
                                    (if header (drop 1 mv-ctx) mv-ctx))]
          (-> db
            (assoc-in [:ctx :name] (get-in db [:mv-ctx :name]))
            (assoc-in [:ctx :attributes] attribute-vector)
            (assoc-in [:scaling] 
              ;; for each attribute build a map with all its distinct values
              (apply merge
                (map 
                  (fn [attribute values]
                    (hash-map 
                      (keyword attribute)
                      (hash-map :measure  "nominal"
                                :values   values
                                :distinct (distinct values))))
                  (map first attribute-vector)
                  nested-values)))
            (assoc-in [:mv-ctx] {:name   nil
                                 :file   nil
                                 :header false})))}))

;;;-Import---------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-import-db
  (fn [db [_ new-db]]
    (assoc-in db [:db :map] new-db)))

(re-frame/reg-fx
 :read-import
  (fn [file]
    (let [reader (js/FileReader.)]
      (set! (.-onload reader) 
            #(re-frame/dispatch [::set-import-db (js->clj
                                                   (->> % 
                                                        .-target 
                                                        .-result
                                                        (.parse js/JSON))
                                                   :keywordize-keys true)]))
      (.readAsText reader file))))

(re-frame/reg-event-fx 
 ::import-file-change
  (fn [cofx [_ file]]
    {:read-import file
     :db (assoc-in (:db cofx) [:db :name] (.-name file))}))

(re-frame/reg-event-db
 ::set-db 
  (fn [db _]
    (if (nil? (get-in db [:db :map]))
      db
      (get-in db [:db :map]))))

;;;-Selection------------------------------------------------------------------

(re-frame/reg-event-fx
 ::attribute-selection-down
  (fn [cofx [_ attribute]]
    (let [db    (:db cofx) 
          index (.indexOf (get-in db [:ctx :attributes]) attribute)]
      {:dispatch [::set-tmp index]})))

(re-frame/reg-event-db
 ::attribute-selection-up
  (fn [db [_ attribute]]
    (let [down-index (:tmp db) 
          up-index   (.indexOf (get-in db [:ctx :attributes]) attribute)
          index-list (if (> down-index up-index)
                         (conj (range up-index down-index) down-index)
                         (conj (range down-index up-index) up-index))]
      ;; go through all indices and swap if they are selected
      (loop [loop-db          (assoc-in db [:tmp] nil)
             loop-index-list  index-list]
        (if (empty? loop-index-list)
            ;; empty the tmp value
            (assoc-in loop-db [:tmp] nil)
            (recur
              (update-in 
                loop-db 
                [:ctx :attributes (first loop-index-list) 1]
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
                                  (get-in db [:ctx :attributes])))]
           (-> db
             (assoc-in [:selection :attributes] attribute-list)
             (assoc-in 
               [:selection :current-attribute] 
               (first attribute-list))))}))

;;;-Ordinal-Scaling-Drag-------------------------------------------------------

(re-frame/reg-event-db
 ::switch-to-context
  (fn [db _]
    (let [current-attribute (get-in db [:selection :current-attribute])]
      (assoc-in db 
                [:scaling (keyword current-attribute) :context-view]
                true))))

;;;-Ordinal-Scaling-Context----------------------------------------------------

(re-frame/reg-event-db
 ::add-new-attribute
  (fn [db [_ attribute]]
    (let [current-attribute (get-in db [:selection :current-attribute])]
      (update-in 
        db 
        [:scaling (keyword current-attribute) :attributes] 
        #(if (some #{attribute} %) % (conj % attribute))))))

(re-frame/reg-event-db
 ::remove-attribute
  (fn [db [_ attribute]]
    (let [current-attribute (get-in db [:selection :current-attribute])]
      (-> db
        (assoc-in [:scaling (keyword current-attribute) 
                   :incidence (keyword attribute)] 
                  nil)
        (update-in [:scaling (keyword current-attribute) :attributes] 
                   (fn [a] (remove #{attribute} a)))))))

(re-frame/reg-event-db
 ::swap-incidence
  (fn [db [_ [attribute object]]]
    (let [current-attribute (get-in db [:selection :current-attribute])]
      (update-in 
        db
        [:scaling (keyword current-attribute) 
         :incidence (keyword attribute) (keyword object)]
        #(if (nil? %) true (not %))))))

(re-frame/reg-event-db
 ::drop-row
  (fn [db [_ [attribute position]]]
    (let [current (get-in db [:selection :current-attribute])]
      (update-in 
        db 
        [:scaling (keyword current) :distinct]
        #(if (some #{attribute} %)
          (let [removed (filter (fn [a](not (= a attribute))) %)
                index   (.indexOf % position)]
                (concat (take index removed)
                        (list attribute)
                        (drop index removed)))
          %)))))

(re-frame/reg-event-db
 ::drop-column
  (fn [db [_ [attribute position]]]
    (let [current (get-in db [:selection :current-attribute])]
      (update-in 
        db 
        [:scaling (keyword current) :attributes]
        #(if (some #{attribute} %)
          (let [removed (filter (fn [a](not (= a attribute))) %)
                index   (.indexOf % position)]
                (concat (take index removed)
                        (list attribute)
                        (drop index removed)))
          %)))))

(re-frame/reg-event-db
 ::relation-name
  (fn [db [_ relation]]
    (let [current (get-in db [:selection :current-attribute])]
      (assoc-in
        db
        [:scaling (keyword current) :relation-name]
        relation))))

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
      (assoc-in db [:scaling (keyword attribute) :measure] measure))))

;;;-Export---------------------------------------------------------------------

(re-frame/reg-event-db
 ::make-context
  (fn [db _]
    (let [context-blob (js/Blob. [(scale/write db)])
          context-url  (js/URL.createObjectURL context-blob)]
      (-> db
        (assoc-in [:warning] false)
        (assoc-in [:context-url]  context-url)))))

(re-frame/reg-event-db
 ::make-config
  (fn [db _]
    (let [;; remove urls from db since the files are not included
          new-db       (-> db
                         (assoc-in [:warning] false)
                         (assoc-in [:panel] "select")
                         (assoc-in [:context-url] nil)
                         (assoc-in [:config-url] nil))
          config-blob  (js/Blob. [(.stringify js/JSON (clj->js new-db))])
          config-url   (js/URL.createObjectURL config-blob)]
      (-> db
        (assoc-in [:warning] false)
        (assoc-in [:config-url]  config-url)))))

(re-frame/reg-event-fx
 ::set-export-warning
  (fn [cofx _]
    {:dispatch [::set-panel "export"]
     :db       (assoc-in (:db cofx) [:warning] true)}))

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
