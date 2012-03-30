(ns XMLplus.node)

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

(defrecord XMLNode [tag attrs content])

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

(defn create-comment
  "Returns an XMLNode instance that represents the comment node with the given comment string"
  [s]
  (create :!-- nil [(str s)]))

