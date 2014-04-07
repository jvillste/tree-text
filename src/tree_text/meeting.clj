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
                      [:link {:href "https://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" :rel "stylesheet"}]
                      [:style (slurp "resources/meeting.css")]]
                     [:body (into [:div {:class "container"}]
                                  contents)
                      [:script {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"}]
                      [:script {:src "https://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"}]]])))

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

(defn dialog-map-to-hiccup [all-children]
  (let [children (rest all-children)]
    (case (first all-children)
      "?" (dialog-item "question" "?" children)
      "-" (dialog-item "minus" "-" children)
      "+" (dialog-item "plus" "+" children)
      "!" (dialog-item "idea" "*" children)
      "!!" (dialog-item "conclusion" "Päätös:" children)
      ":ap" (dialog-item "action-point" "Tehtävä:" [(words-to-string (rest children))])

      [(words-to-string all-children)])))

(defn meeting-body-to-hiccup [all-children]
  (let [children (rest all-children)]
    (case (first all-children)
      ":meeting" (let [metadata (first children)
                       date (:date metadata)]
                   (into [:div [:h1 (str (:title metadata) " ") [:small (str (:day date) "." (:month date) "." (:year date))] ]]
                         (interpose [:hr] (drop 1 children))))
      ":edn" (edn/read-string (first children))

      (dialog-map-to-hiccup all-children))))

(defn exists-in-tree? [tree predicate]
  (and (vector? tree)
       (or (predicate tree)
           (some #(exists-in-tree? % predicate)  (drop 2 tree)))))

(deftest contains-action-point?-test
  (is (= (exists-in-tree? [:tree ":ap" "foo"] #(= (second %) ":ap"))
         true))
  (is (= (exists-in-tree? [:tree ":foo" [:tree ":apd" "foo"]] #(= (second %) ":ap"))
         nil)))

(defn prune-tree [tree predicate]
  (into [:tree (second tree)] (concat (take-while string? (drop 2 tree))
                                      (map #(prune-tree % predicate)
                                           (filter #(exists-in-tree? % predicate)
                                                   (drop 2 tree))))))
(deftest prune-tree-test
  (is (= (prune-tree [:tree ":ap" "foo"] #(= (second %) ":ap"))
         true))
  (is (= (prune-tree [:tree ":apz" [:tree ":aap" [:tree ":ap" "foo"]] [:tree ":apz" "bar"]] #(= (second %) ":ap"))
         true)))

(defn generate-html [source target]
  (let [tree (tree-text/parse (slurp source))
        metadata (-> tree
                     (first)
                     (nth 2)
                     (nth 2)
                     (edn/read-string))

        body (first (tree-text/transform tree
                                         meeting-body-to-hiccup))
        tasks (concat [[:h1 "Tehtävät"]]
                      (for [person (keys (:people metadata))]
                        (concat [[:h4 (get-in metadata [:people person])]]
                                (map #(tree-text/transform %
                                                           dialog-map-to-hiccup)
                                     (drop 2 (prune-tree (first tree)
                                                         (fn [[ _ type responsible & others]]
                                                           (and (= type ":ap")
                                                                (= responsible (str person))))))))))]
    (spit target
          (page (concat [body]
                        tasks)))))

(run-tests)
(generate-html "/Users/jukka/Dropbox/Public/Vaalit 2014/eurovaalityöryhmä_2014_04_06/eurovaalityöryhmä_2014_04_06.ttxt"
               "/Users/jukka/Dropbox/Public/Vaalit 2014/eurovaalityöryhmä_2014_04_06/index.html")
