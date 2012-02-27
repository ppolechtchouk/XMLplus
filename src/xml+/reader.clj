(ns xml+.reader
  (:import [org.xml.sax InputSource Attributes SAXException] 
	   [java.io StringReader BufferedReader Reader File FileReader]
	   [javax.xml.parsers SAXParser SAXParserFactory])
  (:gen-class
   :extends org.xml.sax.helpers.DefaultHandler
   :implements [ org.xml.sax.ext.LexicalHandler]
   :init init
   :constructors {[] []}
   :state state))

					; Utility fns

					; TODO pre-treat text that is being parsed to process the pre-formatted text
					; 1. replace the content of all <pre> tags by an id
					; 2. store the content in a map: id content
					; 3. parse the resulting XML
					; 4. in the DOM replace all child text nodes of <pre> tags by the content based on id

(def ^{:dynamic true} *ignore-whitespace-only-text* true) ; whitespace-only text nodes will be dropped

(defn map-atts
  "Creates a map out of attributes names and values. Returns attributes map or nil if none" 
  [#^org.xml.sax.Attributes atts]
  (if (= (.getLength atts) 0)
    nil
    (reduce merge
	    (for [idx (range (.getLength atts))]
	      (assoc {} (keyword (.getQName atts idx)) (.getValue atts idx))))))
 

(defn whitespace?
  [#^String s]
  "Returns true if s has only whitespace characters"
  (every? #(Character/isWhitespace %) s))

(defn -init [] ; class constructor
  [[] {:dom (ref (create-dom)) :open (ref ())}]) ; state will be used for building DOM


;; DefaultHandler methods
(defn -startDocument [this]
  (dosync
   (alter (:open (.state this))
	  conj
	  (:root (deref (:dom (.state this))))))
  
;  (println "doc started.")
  )

(defn -endDocument [this]

 ; (println "doc finished")
  )

(defn  -startElement [this,
		     uri,
		     localName,
		     qName,
		      #^org.xml.sax.Attributes attributes]

  (dosync
   (let [dom (:dom (.state this))
	 open (:open (.state this))
	 node (element-node localName (map-atts attributes))
	 ]
     (alter dom add-child (first @open) node)
     (alter open conj node)))
;  (println (str "Start element " localName  " qn: " qName))
  )

(defn  -endElement [this,
		     uri,
		     localName,
		     qName]
  (dosync
   (alter (:open (.state this)) next)) ; close currently open node
  ;(println (str "End element " localName  " qn: " qName))
  )

(defn -characters [ this,
		   ^chars ch,
		   start,
		   length]
  (dosync
   (let [dom-ref (:dom (.state this))
	 p (first @(:open (.state this))) 
	 s (new String ch start length)
	 ps (last-child @dom-ref p)]
     (if (and (node? ps) (text-node? ps)) ; first verifies that ps exists, then checks whether is is a TEXT node
       (alter dom-ref mutate-node ps (text-node (str (text ps) s))) ; join 2 text nodes
       (when-not (and *ignore-whitespace-only-text* (whitespace? s)) ; ignore the nodes that contain only whitespace
	 (alter dom-ref add-child p (text-node s))))
     
     ))
  ;(println (str "Text: " s))
  )


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
  (dosync
   (let [dom (:dom (.state this))
	 node (comment-node (new String ch start length))]
     (alter dom add-child (first @(:open (.state this))) node)
     ;(println (str "Comment: " (comment-text node)))
     )))

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




(defn parse [source]
  "Parses the XML source. The source should either be a File, InputSource, InputStream or a URI as a string"
  (let [parser-factory (. SAXParserFactory newInstance)
	handler (new xml+.reader)
	]    
    (.setNamespaceAware parser-factory true)
    (doto (.newSAXParser parser-factory)
      (.setProperty "http://xml.org/sax/properties/lexical-handler" handler)
      (.parse source handler))
    (let [dom (deref (:dom (.state handler)))]
      (export-dom dom (first (filter element-node? (child-nodes dom (:root dom)))))
      )	; return state
    ))

(defn parse-string
  "Parses an XML string. s is a string containing XML"
  [s]
  (parse (InputSource. (StringReader. s))))

(defn parse-file
  "Parses an XML file. f is the file name as a string or a java.io.File object"
  [f]
  (parse (InputSource. (FileReader. f))))

