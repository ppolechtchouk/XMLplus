(defproject XMLplus "0.5.0"
  :description "An XML parser that returns similar map as clojure.xml
Main difference is that the comment nodes are in the resulting structure with :!-- tag
{:tag :!-- :attrs nil :content [\"comment text\"]}"
  :dependencies [[org.clojure/clojure "1.3.0"]])