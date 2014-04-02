(ns tree-text.core
  (:use [hiccup.core]
        [clojure.test])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as instaparse]))

(def as-and-bs
  (instaparse/parser
   "<blocks> = header*
    number = #'[0-9]+'
    header = <'h'> number <Whitespace+> text
    text = Word (Whitespace Word)* EOL
    <Line> = Linepre Word (Whitespace Word)* Linepost EOL
    <Linepre> = (Space (Space (Space)? )? )?
    <Linepost> = Space?
    <Whitespace> = #'(\\ | \\t)+'
    <Space> = ' '
    <Word> = #'\\S+'
    <EOL> = <'\\n'>"))

#_(deftest test
    (is (= (as-and-bs "h1 otsikko ykkönen\n") nil))
    (is (= (->> (as-and-bs "h1 otsikko ykkönen\n")
                (instaparse/transform {:text str
                                       :number read-string
                                       :header (fn [level text] {:type :header :level level :text text})}))
           nil)))


#_(def tree-text
    (instaparse/parser
     "trees = tree*
    tree = <'(:'> word <whitespace?> ( <whitespace> | text | tree )* <')'>
    <text> = word (<whitespace> | word | <'\\n'>)*?
    <whitespace> = #'\\s+'
    <word> = (#'[\\S&&[^()]]+' | '/(' | '/)')+"))

                                        ;#'[\\S&&[^()]]+'
(def tree-text
  (instaparse/parser "

tree =  <space> <'('> word (tree | word | <space> | literal )* <')'>
literal = <'(**'> <space> word <space> anything <space> <'**)'>
<anything> = #'(.|\\n)'+
<word> = #'[\\S&&[^()]]+'
<space> = <#'\\s+'>

"))

(deftest test
  (let [text
        "
(post (date 2014 04 01)
   (chapter (The first post)
-
   (** clojure

   (defn [x]
     (+ x 1))

    **)

   (p This is the first post.)))"]
    #_(is (= (tree-text text) nil))
    (is (= (->> (tree-text text)
                (instaparse/transform {:literal (fn [type & body] (vector :tree type (apply str body)))})
                (instaparse/transform {:tree (fn [& children]
                                               (case (first children)
                                                 "date" (let [[year month day] (rest children)]
                                                          {:year (read-string year)
                                                           :month (read-string month)
                                                           :day (read-string day)})

                                                 children))}))
           nil))))

(defn tree-text-to-html [& children]
  (case (first children)
    "date" (let [[year month day] (rest children)]
             {:year (read-string year)
              :month (read-string month)
              :day (read-string day)})

    (str children))
  )
#_(def tree-text
    (instaparse/parser
     "trees = #'[a-zA-Z]+'"))

#_(deftest test
    (is (= (tree-text "(:post (:date 2014 04 01) (:chapter The first \n post (:p This i the first post.)))(:post (:date 2014 04 01) (:chapter The first \n post (:p This i the first post.)))") nil))
    )

#_(deftest test
    (is (= (tree-text "(:ch This is text\n\n   (:p s fsdf sdfsd f fsf sdf fsfsfdsdsf sdf sdfs fdsf dsf sdf ds fsdf\n       sdf sdf sdfsd fds fdsf dsfds sdf sfd sfdsf (:em sdf sdf sdfsd\n       dsf sf) sdfsd fdsfdsf (:li helsingin sanomat\n       http:/sfdsf.sfsdfsd.fsdfsdfsdf/sfdsffsf?sfdsfds=sfsdf) sfds\n       fdsf dsfds fds fdsf sd fds fds fds fsd fsd fds fd sf ds dsf sf\n       fsdfs dsfdssdf)\n\n   (:p s fsdf sdfsd f fsf sdf fsfsfdsdsf sdf sdfs fdsf dsf sdf ds fsdf\n       sdf sdf sdfsd fds fdsf dsfds dsf dsf sf fsdfs dsfdssdf))") nil))
    )

(def as-and-bs
  (instaparse/parser
   "S = '(' AB* ')'
     AB = A B
     A = 'a'+
     B = 'b'+"))

#_(deftest test
    (is (= (as-and-bs "(ab)") nil))
    )

(run-tests)

(def parse-md
  (instaparse/parser
   "<Blocks> = (Paragraph | Header | List | Ordered | Code | Rule)+
    Header = Line Headerline Blankline+
    <Headerline> = h1 | h2
    h1 = '='+
    h2 = '-'+
    List = Listline+ Blankline+
    Listline = Listmarker Whitespace+ Word (Whitespace Word)* EOL
    <Listmarker> = <'+' | '*' | '-'>
    Ordered = Orderedline+ Blankline+
    Orderedline = Orderedmarker Whitespace* Word (Whitespace Word)* EOL
    <Orderedmarker> = <#'[0-9]+\\.'>
    Code = Codeline+ Blankline+
    Codeline = <Space Space Space Space> (Whitespace | Word)* EOL
    Rule = Ruleline Blankline+
    <Ruleline> = <'+'+ | '*'+ | '-'+>
    Paragraph = Line+ Blankline+
    <Blankline> = Whitespace* EOL
    <Line> = Linepre Word (Whitespace Word)* Linepost EOL
    <Linepre> = (Space (Space (Space)? )? )?
    <Linepost> = Space?
    <Whitespace> = #'(\\ | \\t)+'
    <Space> = ' '
    <Word> = #'\\S+'
    <EOL> = <'\\n'>"))

(def span-elems
  [[#"!\[(\S+)\]\((\S+)\)" (fn [[n href]] (html [:img {:src href :alt n}]))]
   [#"\[(\S+)\]\((\S+)\)"  (fn [[n href]] (html [:a {:href href} n]))]
   [#"`(\S+)`"             (fn [s] (html [:code s]))]
   [#"\*\*(\S+)\*\*"       (fn [s] (html [:strong s]))]
   [#"__(\S+)__"           (fn [s] (html [:strong s]))]
   [#"\*(\S+)\*"           (fn [s] (html [:em s]))]
   [#"_(\S+)_"             (fn [s] (html [:em s]))]])

(defn- parse-span [s]
  (let [res (first (filter (complement nil?)
                           (for [[regex func] span-elems]
                             (let [groups (re-matches regex s)]
                               (if groups (func (drop 1 groups)))))))]
    (if (nil? res) s res)))

(defn- output-html [blocks]
  (reduce str
          (for [b blocks]
            (case (first b)
              :List (html [:ul (for [li (drop 1 b)] [:li (apply str (map parse-span (drop 1 li)))])])
              :Ordered (html [:ol (for [li (drop 1 b)] [:li (apply str (map parse-span (drop 1 li)))])])
              :Header (html [(first (last b)) (apply str (map parse-span (take (- (count b) 2) (drop 1 b))))])
              :Code (html [:pre [:code (apply str (interpose "<br />" (for [line (drop 1 b)] (apply str (drop 1 line)))))]])
              :Rule (html [:hr])
              :Paragraph (html [:p (apply str (map parse-span (drop 1 b)))])))))

(def markdown-to-html (comp output-html parse-md))


#_(parse-md
   " s
==

* foo

")
