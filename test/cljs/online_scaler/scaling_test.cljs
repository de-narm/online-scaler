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
             :conc1 {:distinct   ["a" "b"]
                     :attributes ["a" "b"]
                     :orders     [{:relation ">"
                                   :elements ["b" "a"]}
                                  {:relation ">"
                                   :elements ["a" "b"]}]}
             :conc2 {:distinct   ["a" "b" "c" "d"]
                     :attributes ["a" "b" "c" "d"]
                     :orders     [{:relation ">"
                                   :elements ["a" "b" "c" "d"]}
                                  {:relation ">"
                                   :elements ["d" "b" "c" "a"]}]}
             :num1  {:distinct   ["0" "1" "2" "3"]
                     :selected   [{:name       "a"
                                   :intervals  [{:pos 0 :left "(" :start 0 
                                                        :end 2 :right ")"}]}]}
             :num2  {:distinct   ["0" "1" "2" "3"]
                     :selected   [{:name       "a"
                                   :intervals  [{:pos 0 :left "[" :start 0 
                                                        :end 2 :right "]"}]}]}
             :num3  {:distinct   ["0" "1" "2" "3"]
                     :selected   [{:name       "a"
                                   :intervals  [{:pos 0 :left "[" :start 0 
                                                        :end 2 :right ")"}]}]}
             :numc1 {:distinct   ["0" "1" "2" "3"]
                     :selected   [{:name       "a"
                                   :intervals  [{:pos 0 :left "[" :start 0 
                                                        :end 2 :right ")"}
                                                {:pos 1 :left "[" :start 2 
                                                        :end 3 :right "]"}]}]}
             :numc2 {:distinct   ["0" "1" "2" "3"]
                     :selected   [{:name       "a"
                                   :intervals  [{:pos 0 :left "[" :start 0 
                                                        :end 2 :right ")"}]}
                                  {:name       "b"
                                   :intervals  [{:pos 1 :left "[" :start 2 
                                                        :end 3 :right "]"}]}]}
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

(deftest ordinal-conversion-combination
  (is (= (:incidence (scale/to-context (:conc1 (:scaling db))))
         {:a nil :b nil}))
  (is (= (:incidence (scale/to-context (:conc2 (:scaling db))))
         {:a nil :b {:c true} :c nil :d nil})))

(deftest ordinal-context
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

(deftest ordinal-drag
  (is (= (scale/ordinal-scale (scale/to-context (:con11 (:scaling db))))
         {"a" [], "b" []})) 
  (is (= (scale/ordinal-scale (scale/to-context (:con12 (:scaling db))))
         {"a" ["."], "b" ["X"]}))
  (is (= (scale/ordinal-scale (scale/to-context (:con21 (:scaling db))))
         {"a" [], "b" []}))
  (is (= (scale/ordinal-scale (scale/to-context (:con22 (:scaling db))))
         {"a" ["X"], "b" ["."]}))
  (is (= (scale/ordinal-scale (scale/to-context (:con31 (:scaling db))))
         {"a" ["X"], "b" ["."]}))
  (is (= (scale/ordinal-scale (scale/to-context (:con41 (:scaling db))))
         {"a" ["X"], "b" ["."]})))

(deftest numeric
  (is (= (scale/numeric-scale (:num1 (:scaling db)))
         {"3" ["."], "1" ["X"], "0" ["."], "2" ["."]}))
  (is (= (scale/numeric-scale (:num2 (:scaling db)))
         {"3" ["."], "1" ["X"], "0" ["X"], "2" ["X"]}))
  (is (= (scale/numeric-scale (:num3 (:scaling db)))
         {"3" ["."], "1" ["X"], "0" ["X"], "2" ["."]})))

(deftest numeric-combination
  (is (= (scale/numeric-scale (:numc1 (:scaling db)))
         {"3" ["X"], "1" ["X"], "0" ["X"], "2" ["X"]}))
  (is (= (scale/numeric-scale (:numc2 (:scaling db)))
         {"3" ["." "X"], "1" ["X" "."], "0" ["X" "."], "2" ["." "X"]})))

;intervall gen
;write
