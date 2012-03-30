(ns XMLplus.writer
  (:require [XMLplus.node :as node]))

(def ^:dynamic *tab-step* "  ")

(defprotocol IXMLEmit
  (emit-element [e] [e tab] "Prints the XML. If tab is given the tab will be added to all child elements as well"))

(extend-type String
  IXMLEmit
  (emit-element
    ([this tab]
       (print (str \newline tab this))                   ; ignore tabulation for text?
       )
    ([this]
       (emit-element this nil))))

(extend-type clojure.lang.IPersistentMap
  IXMLEmit
  (emit-element
    ([this tab]
       (cond
        (node/virtual-root? this)
        (doseq [e (:content this)] (emit-element e tab))
        
        (node/comment? this)
        (print (str \newline tab "<!--" (first (:content this)) "-->"))

        (node/element? this)
        (do
          (print (str \newline tab "<" (name (:tag this))))
          (when (:attrs this)
            (doseq [attr (:attrs this)]
              (print (str " " (name (key attr)) "=\"" (if (keyword? (val attr)) (name (val attr)) (val attr)) "\""))))
          (if (:content this)
            (do
              (print ">")
              (doseq [e (:content this)]
                (emit-element e (when tab (str tab *tab-step*))))
              (print 
               (str \newline tab "</" (name (:tag this)) ">")))
            (print "/>")))))
    ([this]
       (emit-element this nil))))