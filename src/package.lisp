(defpackage #:doc-extract-render
  (:use #:cl)
  (:nicknames #:stack-doc-extract-render)
  (:export #:render-error
           #:render-error-message
           #:unknown-render-format
           #:unknown-render-format-format

           #:canonicalize-render-format
           #:render-document
           #:parse-document
           #:strip-render-attributes))

(in-package #:doc-extract-render)
