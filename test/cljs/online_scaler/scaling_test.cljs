(ns online-scaler.scaling-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [online-scaler.scaling :as scale]))


(def db
  {:scaling {:norm {:measure   "nominal"
                    :numerical false
                    :values    ["1" "3" "2" "3"]
                    :distinct  ["1" "3" "2"]}
             :con11 {:distinct ["a" "b"]
                     :attributes ["a" "b"]
                     :orders   [{:relation "<"
                                 :elements ["a"]}]}
             :con12 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation "<"
                                   :elements ["b" "a"]}]}
             :con21 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation ">"
                                   :elements ["a"]}]}
             :con22 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation ">"
                                   :elements ["b" "a"]}]}
             :con31 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation "<="
                                   :elements ["a"]}]}
             :con32 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation "<="
                                   :elements ["b" "a"]}]}
             :con41 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation ">="
                                   :elements ["a"]}]}
             :con42 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation ">="
                                   :elements ["b" "a"]}]}
                                  }})

(deftest nominal
  (is (= (scale/nominal-scale (:norm (:scaling db)))
         {"1" ["X" "." "."], "3" ["." "X" "."], "2" ["." "." "X"]})))

(deftest ordinal-conversion
  (is (= (:incidence (scale/to-context (:con11 (:scaling db))))
         {:a nil :b nil}))
  (is (= (:incidence (scale/to-context (:con12 (:scaling db))))
         {:a {:b true} :b nil}))
  (is (= (:incidence (scale/to-context (:con21 (:scaling db))))
         {:a nil :b nil}))
  (is (= (:incidence (scale/to-context (:con22 (:scaling db))))
         {:b {:a true} :a nil}))
  (is (= (:incidence (scale/to-context (:con31 (:scaling db))))
         {:a {:a true} :b nil}))
  (is (= (:incidence (scale/to-context (:con32 (:scaling db))))
         {:a {:b true :a true} :b {:b true}}))
  (is (= (:incidence (scale/to-context (:con41 (:scaling db))))
         {:a {:a true} :b nil}))
  (is (= (:incidence (scale/to-context (:con42 (:scaling db))))
         {:b {:a true :b true} :a {:a true}})))

(deftest ordinal
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con11 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["." "."], "b" ["." "."]})) 
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con12 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["." "."], "b" ["X" "."]}))
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con21 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["." "."], "b" ["." "."]})) 
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con22 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["." "X"], "b" ["." "."]})) 
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con31 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["X" "."], "b" ["." "."]})) 
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con32 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["X" "."], "b" ["X" "X"]})) 
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con41 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["X" "."], "b" ["." "."]})) 
  (is (= (scale/ordinal-scale 
           (assoc-in (scale/to-context (:con42 (:scaling db)))
                     [:context-view]
                     true))
         {"a" ["X" "X"], "b" ["." "X"]})))

;conversion combinations
;context view
;numeric
;intervall gen
;alles unter numeric
