;; compile all records and gen-class
(compile 'XMLplus.node)
(compile 'XMLplus.writer)
(compile 'XMLplus.reader)

;; start code
(ns XMLplus.core
  (:require [XMLplus.writer :as writer]
            [XMLplus.reader :as reader]))

(defn emit
  "Print the x as xml. If tabulation is supplied it will be used"
  ([x] (emit x nil))
  ([x tab]
     (print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
     (binding [writer/*tab-step* (when tab tab)]
       (writer/emit-element x tab))))

(defn emit->str
  "Same as emit, only returns a string rather than prints it"
  ([x] (emit->str x nil))
  ([x tab] (with-out-str (emit x tab))))


(defn parse
  "Parses an loads the source, and returns a tree of a XMLplus.node.XMLNode records. This are compatible with the return of xml/parse

source can be a File, InputStream or a String naming a URI if :string or :filename flags are not supplied. The parser can be controlled using flags, which are:
:string - source is an XML string
:filename - source is a string representing a file name

:comments - parse comment nodes
:whitespace - preserve the whitespace-only text nodes
:vroot - use a virtual root. This is useful if you need to preserve comments before or after the root XML tag

With no flags it acts exactly as xml/parse
"
  ([source] (parse source nil))
  ([source & flags]
     (let [flags (set flags)]
       (binding [reader/*parse-comments* (when (flags :comments) true)
                 reader/*use-virtual-root* (when (flags :vroot) true)
                 reader/*ignore-whitespace-only-text* (if (flags :whitespace) false true)]
        (cond
         (flags :string)
         (reader/parse-string source)

         (flags :filename)
         (reader/parse-file source)

         :default
         (reader/parse source))))))
