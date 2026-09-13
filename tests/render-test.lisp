(in-package #:doc-extract-render/tests)

(deftest markdown-sample-strict
  (let* ((doc (make-sample-doc :unicode-p t :title "Markup"))
         (md (doc-extract-render:render-document doc :markdown :profile :archival))
         (round (doc-extract-render:parse-document md :markdown)))
    (ok (search "<!--" md))
    (ok (search "café" md))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :text))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :structure))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(deftest html-sample-strict
  (let* ((doc (make-sample-doc :unicode-p t :title "Html"))
         (html (doc-extract-render:render-document doc :html :profile :archival))
         (round (doc-extract-render:parse-document html :html)))
    (ok (search "<section" html))
    (ok (search "data-demiurge-id" html))
    (ok (search "<table" html))
    (ok (search "hash:abc123" html))
    (ok (search "<em>" html))
    (ok (search "café" html))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :text))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :structure))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(deftest html-table-rowspan-colspan
  (let* ((table (doc-extract-protocol:make-table-block
                 :rows 2 :cols 2
                 :cells
                 (list (doc-extract-protocol:make-table-cell
                        :row 0 :col 0 :header-p t :col-span 2
                        :content (list (doc-extract-protocol:make-text-block
                                        :text "wide")))
                       (doc-extract-protocol:make-table-cell
                        :row 1 :col 0
                        :content (list (doc-extract-protocol:make-text-block
                                        :text "a")))
                       (doc-extract-protocol:make-table-cell
                        :row 1 :col 1
                        :content (list (doc-extract-protocol:make-text-block
                                        :text "b"))))))
         (doc (doc-extract-protocol:ensure-ids
               (doc-extract-protocol:make-extracted-document
                :blocks (list (doc-extract-protocol:make-section-block
                               :title "T" :level 1
                               :children (list table))))))
         (html (doc-extract-render:render-document doc :html :profile :archival))
         (round (doc-extract-render:parse-document html :html)))
    (ok (search "colspan=\"2\"" html))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict))))

(deftest strip-markdown-holds-structure
  (let* ((doc (make-sample-doc :title "StripMD"))
         (md (doc-extract-render:render-document doc :markdown :profile :archival))
         (stripped (doc-extract-render:strip-render-attributes md :markdown))
         (round (doc-extract-render:parse-document stripped :markdown)))
    (ng (search "{#" stripped))
    (ok (search "close=section" stripped))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :structure))))

(deftest strip-html-holds-structure
  (let* ((doc (make-sample-doc :title "StripHTML"))
         (html (doc-extract-render:render-document doc :html :profile :archival))
         (stripped (doc-extract-render:strip-render-attributes html :html))
         (round (doc-extract-render:parse-document stripped :html)))
    (ng (search "data-demiurge-" stripped))
    (ok (search "<section" stripped))
    (ok (doc-extract-protocol:document-equivalent-p doc round :level :structure))))

(deftest foreign-markdown-fixture
  (let* ((path (fixture-path "foreign.md"))
         (doc (doc-extract-render:parse-document path :markdown))
         (blocks (doc-extract-protocol:extracted-document-blocks doc)))
    (ok (probe-file path))
    (ok (plusp (length blocks)))
    (ok (find-if (lambda (b) (typep b 'doc-extract-protocol:section)) blocks))
    (let ((has-list nil) (has-table nil) (has-image nil))
      (labels ((walk (b)
                 (when (typep b 'doc-extract-protocol:list-block) (setf has-list t))
                 (when (typep b 'doc-extract-protocol:table-block) (setf has-table t))
                 (when (typep b 'doc-extract-protocol:image-block) (setf has-image t))
                 (mapc #'walk (doc-extract-protocol:block-children b))))
        (mapc #'walk blocks))
      (ok has-list)
      (ok has-table)
      (ok has-image))))

(deftest foreign-html-fixture
  (let* ((path (fixture-path "foreign.html"))
         (doc (doc-extract-render:parse-document path :html))
         (blocks (doc-extract-protocol:extracted-document-blocks doc)))
    (ok (probe-file path))
    (ok (plusp (length blocks)))
    (ok (find-if (lambda (b) (typep b 'doc-extract-protocol:section)) blocks))
    (let ((has-list nil) (has-table nil) (has-quote nil))
      (labels ((walk (b)
                 (when (typep b 'doc-extract-protocol:list-block) (setf has-list t))
                 (when (typep b 'doc-extract-protocol:table-block) (setf has-table t))
                 (when (and (typep b 'doc-extract-protocol:text-block)
                            (eq (doc-extract-protocol:text-block-kind b) :quote))
                   (setf has-quote t))
                 (mapc #'walk (doc-extract-protocol:block-children b))))
        (mapc #'walk blocks))
      (ok has-list)
      (ok has-table)
      (ok has-quote))))

(deftest unknown-format-signals
  (let ((doc (make-sample-doc :title "X")))
    (ok (signals (doc-extract-render:render-document doc :rtf)
                 'doc-extract-render:unknown-render-format))
    (ok (signals (doc-extract-render:parse-document "x" :rtf)
                 'doc-extract-render:unknown-render-format))))

(deftest unknown-format-use-value
  (let ((doc (make-sample-doc :title "X")))
    (ok (string= "ok"
                 (handler-bind
                     ((doc-extract-render:unknown-render-format
                       (lambda (c) (use-value "ok" c))))
                   (doc-extract-render:render-document doc :rtf))))))

(deftest render-to-stream
  (let* ((doc (make-sample-doc :title "Stream"))
         (s (make-string-output-stream)))
    (doc-extract-render:render-document doc :markdown :stream s :profile :llm)
    (ok (search "#" (get-output-stream-string s)))))
