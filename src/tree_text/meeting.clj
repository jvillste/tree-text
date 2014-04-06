(ns tree-text.meeting
  (:use [clojure.test])
  (:require [clojure.string :as string]
            [hiccup.core :as hiccup]
            [clojure.edn :as edn]
            [tree-text.core :as tree-text]))

(defn page [contents]
  (str "<!DOCTYPE html>"
       (hiccup/html [:html {:lang "en"}
                     [:head
                      [:meta {:charset "utf-8"}]
                      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                      [:link {:href "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" :rel "stylesheet"}]
                      [:style (slurp "resources/meeting.css")]
                      [:link {:href "css/meeting.css" :rel "stylesheet"}]]
                     [:body [:div {:class "container"}
                             (hiccup/html contents)]
                      [:script {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"}]
                      [:script {:src "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"}]]])))

(defn words-to-string [words]
  (apply str (interpose " " words)))

(defn div [class contents]
  (vec (concat [:div {:class class}] contents)))

(defn div-with-text-and-children [class children]
  (div class (into [(words-to-string (take-while string? children))]
                   (drop-while string? children))))

(defn icon [text class]
  [:span {:class class} text])

(defn dialog-item [class icon-text children]
  (div class (into [(icon icon-text (str class "-icon"))
                    (words-to-string (take-while string? children))]
                   (drop-while string? children))))



(defn meeting-body-to-hiccup [all-children]
  (let [children (rest all-children)]
    (case (first all-children)
      ":meeting" (drop 1 children)
      "?" (dialog-item "question" "?" children)
      "-" (dialog-item "minus" "-" children)
      "+" (dialog-item "plus" "+" children)
      "!" (dialog-item "idea" "*" children)
      "!!" (dialog-item "conclusion" "Päätös:" children)
      ":ap" (dialog-item "action-point" "Tehtävä:" [(words-to-string (rest children))])
      ":edn" (edn/read-string (first children))

      [(words-to-string all-children)])))

(defn generate-html [source target]
  (let [tree (tree-text/parse (slurp source))]
    (spit target
          (page (first (tree-text/transform tree
                                            meeting-body-to-hiccup))))))

(generate-html "/Users/jukka/Dropbox/Public/Vaalit 2014/eurovaalityöryhmä_2014_04_06/eurovaalityöryhmä_2014_04_06.ttxt"
               "/Users/jukka/Dropbox/Public/Vaalit 2014/eurovaalityöryhmä_2014_04_06/index.html")

(run-tests)