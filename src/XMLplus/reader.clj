(ns XMLplus.reader
  (:require [XMLplus.node :as node])
  (:import [org.xml.sax InputSource Attributes SAXException] 
	   [java.io StringReader BufferedReader Reader File FileReader]
	   [javax.xml.parsers SAXParser SAXParserFactory])
  (:gen-class
   :extends org.xml.sax.helpers.DefaultHandler
   :implements [ org.xml.sax.ext.LexicalHandler]
   :init init
   :constructors {[] []}
   :state state))





(defprotocol INodeStack
  "Various stack operations for a mutable stack. Possibly not thread safe!"
  (current-node [_] "Returns the top element in the stack. The stack is not modified.")
  (pop-node [_] "Returns the top element in the stack. Top element is removed form the stack!!")
  (push-node [_ obj] "Add obj to the top of the stack"))

(defprotocol IContent
  "Functions for adding content to the parent node"
  (add-content [_ obj] "Adds to the :content vector of the node at the top of the stack"))

(defprotocol IChars
  "Function for adding text content"
  (append-chars [_ ch start length] "Appends a sub-vector of ch to the text buffer")
  (append-string [_ s] "Appends s to the text buffer")
  (push-chars [_] "Pushes the text buffer to the :content vector of the node at the at the top of the stack. Clears the text buffer"))

(deftype State [^:unsynchronized-mutable stack  ^:unsynchronized-mutable sb]
  INodeStack
  (current-node [_] (peek stack))
  (pop-node [_] (let [x (peek stack)]
             (set! stack (pop stack))
             x))
  (push-node [_ obj] (set! stack (conj stack obj))) ; add consistency check?
  IContent
  (add-content [this obj]
    (let [e (pop-node this)]
      (push-node this (assoc e :content (conj (or (:content e) []) obj)))))
  IChars
  (append-chars [_ ch start length]
    (when-not sb
      (set! sb (StringBuilder. (int length))))
    (.append sb (chars ch) (int start) (int length)))
  (append-string [_ s]
    (if-not sb
      (set! sb (StringBuilder. (str s)))
      (.append sb (str s))))
  (push-chars [this]
    (when sb
      (add-content this (str sb))
      (set! sb nil))))


(def ^:dynamic *parse-comments* false) ; add comments
(def ^:dynamic *use-virtual-root* false) ; use virutal root. This must be set to preserve comments outside of the root node
                                        ; or whitespace outside of the root node
(def ^{:dynamic true} *ignore-whitespace-only-text* true) ; whitespace-only text nodes will be dropped


(defn whitespace?
  "Returns true if all chars in the sub-array are whitespace or there are no chars to check"
  [^chars ch start length]
  (loop [idx start left length]
    (cond
     (not (Character/isWhitespace (aget ch idx)))
     false
     
     (pos? left) ; still chars to check
     (recur (inc idx) (dec left))

     :default ; no chars left to check and all previous ones were whitespace
     true)))

(defn map-atts
  "Creates a map out of attributes names and values. Returns attributes map or nil if none" 
  [^org.xml.sax.Attributes atts]
  (if (= (.getLength atts) 0)
    nil
    (reduce merge
	    (for [idx (range (.getLength atts))]
	      (assoc {} (keyword (.getQName atts idx)) (.getValue atts idx))))))


(defn -init [] ; class constructor
  [[] (State. nil nil)]) ; state will be used for building DOM


;; DefaultHandler methods
(defn -startDocument [this]

  ;; create a virtual root for the document
  ;; this will allow to place an comments that occur outside of the root XML tag
  ;; the virtual root will be removed at the end of parsing if *use-virtual-root* is set to false
  (push-node (.state this) (node/create :<root> nil nil))
  ;(println "doc started.")
  )

(defn -endDocument [this]
  ;(println "doc finished")
  )

(defn  -startElement [this,
		     uri,
		     localName,
		     qName,
		      ^org.xml.sax.Attributes attributes]
  ;(println (str "Start element " localName  " qn: " qName))
  ;; if there is any text, add it to the :content of the parent element
  (push-chars (.state this))
  ;; add a new element to the stack
  (push-node (.state this)
        (node/create (keyword qName) ; possibly intern as symbol to keep reusing the keywords?
                     (map-atts attributes)
                     nil)))

(defn  -endElement [this,
		     uri,
		     localName,
		     qName]
  ;(println (str "End element " localName  " qn: " qName))
  ;; if there is any text, add it to the :content of the current element
  (push-chars (.state this))
  ;; add current element to the :content of the parent
  (add-content (.state this) (pop-node (.state this))))

(defn -characters [ this,
		   ^chars ch,
		   start,
		   length]
  ;;comment out debug
  #_(println (str
            "Text: \""
            (-> (StringBuilder. length) 
                (.append ch start length)
                (.toString))
            "\""))
  
  (when-not (and *ignore-whitespace-only-text*
                 (whitespace? ch start length))
    (append-chars (.state this) ch start length)))


(defn -ignorableWhitespace [ this,
			    ^chars ch,
			    start,
			    length]
					; do nothing
;(println (str "IgWhitespace: " length " chars"))
  )

(defn -warning [this, e]
  ; TODO
  )

(defn -error [this, e]
  ; TODO
  )
(defn -fatalError [this, e]
  ; TODO
  )

;; LexicalHandler methods
(defn -comment [this,
		^chars ch,
		start,
		length]
  ;;comment out debug
  #_(println (str
            "Comment: \""
            (-> (StringBuilder. length) 
              (.append ch start length)
              (.toString))
            "\""))
  (when *parse-comments*
    (let [s (-> (StringBuilder. length)
              (.append ch start length)
              (.toString))]
      (add-content
       (.state this)
       (node/create :!-- nil [s]))))
   ;(println (str "Comment: " (comment-text node)))
  )

(defn -startEntity [ this, #^String name]
					; do nothing - will be automatically converted to text
  ;(println (str "start entity " name))
  )

(defn -endEntity [ this, #^String name]
					;do nothing
  ;(println (str "end entity " name))
)

(defn -startCDATA [this]
; do nothing
  )

(defn -endCDATA [this]
; do nothing
  )



(defn find-root-element
  "Given a root element returns the first non-comment node"
  [m]
  (some #(when-not (node/comment? %) %) (:content m)))

(defn parse [source]
  "Parses the XML source. The source should either be a File, InputSource, InputStream or a URI as a string"
  (let [parser-factory (. SAXParserFactory newInstance)
	handler (new XMLplus.reader)
	]    
    (.setNamespaceAware parser-factory true)
    (doto (.newSAXParser parser-factory)
      (.setProperty "http://xml.org/sax/properties/lexical-handler" handler)
      (.parse source handler))
    ;; return dom
    (if *use-virtual-root*
      (pop-node (.state handler))
      (find-root-element (pop-node (.state handler))))))

(defn parse-string
  "Parses an XML string. s is a string containing XML"
  [s]
  (parse (InputSource. (StringReader. s))))

(defn parse-file
  "Parses an XML file. f is the file name as a string or a java.io.File object"
  [f]
  (parse (InputSource. (FileReader. f))))

