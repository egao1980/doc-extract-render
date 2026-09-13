(in-package #:doc-extract-render)

(defmethod render-document (doc (format (eql :markdown))
                            &key stream (profile :llm) &allow-other-keys)
  (doc-extract-protocol:dump-markup doc :profile profile :stream stream))

(defmethod parse-document (input (format (eql :markdown)) &key &allow-other-keys)
  (doc-extract-protocol:parse-markup (%coerce-text input)))
