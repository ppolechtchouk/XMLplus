(defproject XMLplus "1.1.1"
  :description "An XML parser that returns similar map as clojure.xml
Main difference is that the comment nodes are in the resulting structure with :!-- tag
{:tag :!-- :attrs nil :content [\"comment text\"]}
In addition, it can emit with tabulations
"
  :dependencies [[org.clojure/clojure "1.3.0"]])