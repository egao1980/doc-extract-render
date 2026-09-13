(in-package #:doc-extract-render/tests)

(defun %roundtrip-strict (doc format)
  (let* ((text (doc-extract-render:render-document doc format :profile :archival))
         (round (doc-extract-render:parse-document text format)))
    (and (doc-extract-protocol:document-equivalent-p doc round :level :text)
         (doc-extract-protocol:document-equivalent-p doc round :level :structure)
         (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(defun %roundtrip-stripped-structure (doc format)
  (let* ((text (doc-extract-render:render-document doc format :profile :archival))
         (stripped (doc-extract-render:strip-render-attributes text format))
         (round (doc-extract-render:parse-document stripped format)))
    (doc-extract-protocol:document-equivalent-p doc round :level :structure)))

(defun %property-loop (format check-fn label)
  (let ((failed 0)
        (seeds '())
        (n *property-doc-count*))
    (dotimes (i n)
      (let* ((seed (+ 1000 (* i 17)))
             (doc (gen-document seed)))
        (handler-case
            (unless (funcall check-fn doc format)
              (incf failed)
              (push seed seeds))
          (error ()
            (incf failed)
            (push seed seeds)))))
    (ok (zerop failed)
        (format nil "~a: ~d/~d generated docs failed~@[ seeds ~a~]"
                label failed n (and seeds (subseq (nreverse seeds)
                                                  0 (min 8 (length seeds))))))))

(deftest markdown-generated-strict
  (%property-loop :markdown #'%roundtrip-strict
                  (format nil "markdown :strict ×~d" *property-doc-count*)))

(deftest html-generated-strict
  (%property-loop :html #'%roundtrip-strict
                  (format nil "html :strict ×~d" *property-doc-count*)))

(deftest markdown-generated-stripped-structure
  (%property-loop :markdown #'%roundtrip-stripped-structure
                  (format nil "markdown stripped :structure ×~d"
                          *property-doc-count*)))

(deftest html-generated-stripped-structure
  (%property-loop :html #'%roundtrip-stripped-structure
                  (format nil "html stripped :structure ×~d"
                          *property-doc-count*)))
