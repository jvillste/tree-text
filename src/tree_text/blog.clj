(ns tree-text.blog
  (:use [hiccup.core]
        [clojure.test])
  (:require [tree-text.core :as tree-text]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as instaparse]
            [hiccup.core :as hiccup]))

(defn page [contents]
  (str "<!DOCTYPE html>"
       (hiccup/html [:html {:lang "en"}
                     [:head
                      [:meta {:charset "utf-8"}]
                      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                      [:link {:href "css/bootstrap.min.css" :rel "stylesheet"}]]
                     [:body [:div {:class "container"}
                             contents]
                      [:script {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"}]
                      [:script {:src "js/bootstrap.min.js"}]]])))

(defn blog-to-html [all-children]
  (let [children (rest all-children)]
    (case (first all-children)
      ":p" (str "<p>" (apply str (interpose " " children) ) "</p>")
      ":post" (page (str (hiccup/html [:h1 (:title (first children))]) (apply str (rest children))))
      ":emphasize" (str "<span class=\"bold\">" (apply str (interpose " " children) ) "</span>")
      ":code" (str "<pre>" (apply str (interpose " " children) ) "</pre>")
      ":clojure" (read-string (first children))
      (str all-children))))


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

(spit "blog/test.html" (first (tree-text/transform (tree-text/parse test-text) blog-to-html)))

(run-tests)
