(defproject XMLplus "1.1.6"
  :description "An XML parser that returns similar map as clojure.xml
Main difference is that the comment nodes are in the resulting structure with :!-- tag
{:tag :!-- :attrs nil :content [\"comment text\"]}
In addition, it can emit with tabulations
Changelog:
1.1.6 Modified writer/emit-element to ignore attributes with nil values
1.1.5 Corrected create-comment
1.1.4 Corrected writer/emit-element to use name on keyword values
1.1.3 Added node/create-comment
1.1.2 Fixed XMLplus.reader/whitespace?
1.1.1 Added default namespace support

"
  :dependencies [[org.clojure/clojure "1.3.0"]])