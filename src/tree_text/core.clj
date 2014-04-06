(ns tree-text.core
  (:use [hiccup.core]
        [clojure.test])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as instaparse]
            [hiccup.core :as hiccup]))


(def tree-text-parser
  (instaparse/parser "
<document> = <space>* ( tree | <space> )*
tree =  <'('> <space>* word (tree | word | <space> | literal )* <')'>
literal = <'(**'> <space> word <space> literal-content* <'**)'>
<literal-content> = !'**)' #'(.|\\n)'
<word> = #'[\\S&&[^()]]+'
<space> = <#'\\s'>

"))

(defn parse [text]
  (->> (tree-text-parser text)
       (instaparse/transform {:literal (fn [type & body] (vector :tree type (apply str body)))}))  )

(defn transform [tree transformer]
  (instaparse/transform {:tree (fn [& children]
                                 (transformer children))}
                        tree))

(def test-text
  "
(:post (** :clojure
         {:date {:year 2014 :month 4 :day 1}
          :tags [:first :lisp]
          :title \"This is a post\"
          :published false}
        **)

 (:p s fsdf sdfsd f fsf (:emphasize sdf fsfsfdsdsf sdf) sdf sf sdfs dfs)

 (** :code

 (def tree-text-parser
  (instaparse/parser
   \"trees = tree*
     word = (#'[\\S&&[^()]]+' | '/(' | '/)')+\"))

  **))
")

(def test-text-2
  "
(:meeting
 (2014 4 6)
 (:sari (Sari J)
  :jukka (Jukka V)))

(:meeting
 (2014 4 6)
 (:sari (Sari J)
  :jukka (Jukka V)))


")

(deftest test
  #_(is (= (instaparse/parse tree-text-parser test-text) nil))
  (is (= (transform (parse test-text)
                    (fn [children] children))
           nil)))

(run-tests)
