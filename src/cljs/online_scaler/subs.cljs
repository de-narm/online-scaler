(ns online-scaler.subs
  (:require 
   [re-frame.core :as re-frame]))

;;;-Upload---------------------------------------------------------------------

(re-frame/reg-sub
 ::mv-ctx-file-name
  (fn [db _]
    (get-in db [:mv-ctx :name])))

(re-frame/reg-sub
 ::mv-ctx-file
  (fn [db _]
    (get-in db [:mv-ctx :file])))

(re-frame/reg-sub
 ::checkbox
  (fn [db _]
    (get-in db [:mv-ctx :header])))

;;;-Import---------------------------------------------------------------------

(re-frame/reg-sub
 ::import-name
  (fn [db _]
    (get-in db [:db :name])))

(re-frame/reg-sub
 ::import-db
  (fn [db _]
    (get-in db [:db :map])))

;;;-Selection------------------------------------------------------------------

(re-frame/reg-sub
 ::ctx-attributes
  (fn [db _]
    (get-in db [:ctx :attributes])))

;;;-Ordinal-Scaling------------------------------------------------------------

(re-frame/reg-sub
 ::current-distinct
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :distinct])))

(re-frame/reg-sub
 ::current-attributes
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :attributes])))

;;;-Scaling--------------------------------------------------------------------

(re-frame/reg-sub
 ::current-attribute
  (fn [db _]
    (get-in db [:selection :current-attribute])))

(re-frame/reg-sub
 ::attribute-list
  (fn [db _]
    (get-in db [:selection :attributes])))

(re-frame/reg-sub
 ::attribute-measure
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :measure])))

;;;-Export---------------------------------------------------------------------

(re-frame/reg-sub
 ::context-url
  (fn [db _]
    (get-in db [:context-url])))

(re-frame/reg-sub
 ::config-url
  (fn [db _]
    (get-in db [:config-url])))

(re-frame/reg-sub
 ::warning
  (fn [db _]
    (get-in db [:warning])))

(re-frame/reg-sub
 ::ctx-file-name
  (fn [db _]
    (get-in db [:ctx :name])))

;;;-Panel----------------------------------------------------------------------

(re-frame/reg-sub
 ::panel
  (fn [db _]
    (get-in db [:panel])))

;;;-Util-----------------------------------------------------------------------

(re-frame/reg-sub
 ::get-tmp
  (fn [db _]
    (get-in db [:tmp])))
