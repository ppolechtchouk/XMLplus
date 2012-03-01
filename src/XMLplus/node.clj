(ns XMLplus.node)


(def ^:dynamic *tab-step* "  ")

(defn comment?
  "Returns true if n is a comment node"
  [n]
  (= :!-- (:tag n)))

(defn virtual-root?
  "Return true if the node is a virtual root"
  [n]
  (= :<root> (:tag n)))

(defn element?
  "Return true if n is an element node"
  [n]
  (and (:tag n) (not (or (comment? n) (virtual-root? n)))))

(defn text?
  "Returns true if n is a text node"
  [n]
  (instance? String n))

(defprotocol IXMLEmit
  (emit-element [e] [e tab] "Prints the XML. If tab is given the tab will be added to all child elements as well"))

(defrecord XMLNode [tag attrs content]
  IXMLEmit
  (emit-element
    [this tab]
    ;;TODO
    (cond
     (virtual-root? this)
     (doseq [e content] (emit-element e tab))
     
     (comment? this)
     (print (str \newline tab "<!--" (first content) "-->"))

     (element? this)
     (do
       (print (str \newline tab "<" (name tag)))
       (when attrs
         (doseq [attr attrs]
           (print (str " " (name (key attr)) "=\"" (val attr) "\""))))
       (if content
         (do
           (print ">")
           (doseq [e content]
             (emit-element e (when tab (str tab *tab-step*))))
           (print 
            (str (when-not (text? (last content)) \newline) ; no new line necessary if text node
                 tab "</" (name tag) ">")))
         (print "/>")))))
  (emit-element [this]
       (emit-element this nil)))

(extend-type String
  IXMLEmit
  (emit-element
    ([this tab]
       (print this) ; ignore tabulation for text?
       )
    ([this]
       (emit-element this nil))))


(defn create
  "Returns an XMLNode instance based on supplied parameters
Input params are either:
;;
m - a map with :tag :attrs :content keys
;;
tag, attrs, content - explicit params"
  ([m]
     ;; TODO - assoc other keyvals?
     (XMLNode. (:tag m) (:attrs m) (:content m)))
  ([tag attrs content]
     (XMLNode. tag attrs content)))

