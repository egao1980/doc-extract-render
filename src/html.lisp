(in-package #:doc-extract-render)

;;; Semantic HTML5 writer. Block id/provenance/layer/attrs live on
;;; data-demiurge-* attributes. :llm omits archival fields (same split as
;;; dump-markup). Serialize via html-protocol (no backend required).

(defun %bound (object slot)
  "SLOT may be a symbol; look it up on the protocol class (package-safe)."
  (let ((name (find-symbol (string slot) :doc-extract-protocol)))
    (and object name
         (slot-exists-p object name)
         (slot-boundp object name)
         (slot-value object name))))

(defun %kw (s &optional default)
  (cond
    ((null s) default)
    ((keywordp s) s)
    ((symbolp s) (intern (symbol-name s) :keyword))
    ((and (stringp s) (plusp (length s)))
     (intern (string-upcase s) :keyword))
    (t default)))

(defun %comment-quote (s)
  (let ((s (if (keywordp s)
               (string-downcase (symbol-name s))
               (if (stringp s) s (princ-to-string s)))))
    (if (or (zerop (length s))
            (find-if (lambda (c)
                       (or (char= c #\Space) (char= c #\")
                           (char= c #\=) (char= c #\;)
                           (char= c #\|) (char= c #\>))
                       ) s))
        (with-output-to-string (o)
          (write-char #\" o)
          (loop for ch across s
                do (when (or (char= ch #\\) (char= ch #\"))
                     (write-char #\\ o))
                   (write-char ch o))
          (write-char #\" o))
        s)))

(defun %read-quoted (s start)
  (let ((out (make-string-output-stream))
        (i (1+ start)))
    (loop while (< i (length s))
          for ch = (char s i)
          do (cond
               ((char= ch #\\)
                (when (< (1+ i) (length s))
                  (write-char (char s (1+ i)) out)
                  (incf i 2)))
               ((char= ch #\")
                (return (values (get-output-stream-string out) (1+ i))))
               (t
                (write-char ch out)
                (incf i))))
    (values (get-output-stream-string out) i)))

(defun %read-token (s start &optional (stops '(#\; #\|)))
  (let ((end start))
    (loop while (and (< end (length s))
                     (not (member (char s end) stops)))
          do (incf end))
    (values (subseq s start end) end)))

(defun %parse-num-list (s)
  (when (and s (plusp (length s)))
    (loop for start = 0 then (1+ comma)
          for comma = (position #\, s :start start)
          collect (let ((*read-eval* nil))
                    (read-from-string s t nil :start start
                                      :end (or comma (length s))))
          while comma)))

(defun %encode-attrs (plist)
  (when (and plist (listp plist))
    (format nil "~{~a~^;~}"
            (loop for (k v) on plist by #'cddr
                  collect (format nil "~a=~a"
                                  (string-downcase (symbol-name k))
                                  (%comment-quote v))))))

(defun %decode-attrs (s)
  (when (and s (plusp (length s)))
    (let ((acc '())
          (i 0))
      (loop while (< i (length s))
            do (if (char= (char s i) #\;)
                   (incf i)
                   (let ((eq (position #\= s :start i)))
                     (unless eq (return))
                     (let ((key (intern (string-upcase (subseq s i eq)) :keyword)))
                       (incf eq)
                       (multiple-value-bind (val j)
                           (if (and (< eq (length s)) (char= (char s eq) #\"))
                               (%read-quoted s eq)
                               (%read-token s eq '(#\;)))
                         (setf acc (list* key (%kw val val) acc)
                               i j))))))
      acc)))

(defun %encode-prov (entries)
  (when entries
    (format nil "~{~a~^|~}"
            (mapcar
             (lambda (p)
               (let ((parts '()))
                 (when (%bound p 'page)
                   (push (format nil "page=~d"
                                 (doc-extract-protocol:provenance-entry-page p))
                         parts))
                 (when (%bound p 'bbox)
                   (push (format nil "bbox=~{~a~^,~}"
                                 (doc-extract-protocol:provenance-entry-bbox p))
                         parts))
                 (when (%bound p 'charspan)
                   (push (format nil "span=~{~a~^,~}"
                                 (doc-extract-protocol:provenance-entry-charspan p))
                         parts))
                 (let ((cs (%bound p 'coord-system)))
                   (when cs
                     (push (format nil "units=~a"
                                   (string-downcase
                                    (symbol-name
                                     (or (%bound cs 'units) :pixels))))
                           parts)
                     (push (format nil "origin=~a"
                                   (string-downcase
                                    (symbol-name
                                     (or (%bound cs 'origin) :top-left))))
                           parts)
                     (when (%bound cs 'layout-width)
                       (push (format nil "lw=~a"
                                     (doc-extract-protocol:coord-system-layout-width cs))
                             parts))
                     (when (%bound cs 'layout-height)
                       (push (format nil "lh=~a"
                                     (doc-extract-protocol:coord-system-layout-height cs))
                             parts))))
                 (format nil "~{~a~^;~}" (nreverse parts))))
             entries))))

(defun %decode-one-prov (s)
  (let ((page nil) (bbox nil) (span nil)
        (units nil) (origin nil) (lw nil) (lh nil)
        (i 0))
    (loop while (< i (length s))
          do (if (char= (char s i) #\;)
                 (incf i)
                 (let ((eq (position #\= s :start i)))
                   (unless eq (return))
                   (let ((key (intern (string-upcase (subseq s i eq)) :keyword)))
                     (incf eq)
                     (multiple-value-bind (val j)
                         (%read-token s eq '(#\;))
                       (setf i j)
                       (case key
                         (:page (setf page (parse-integer val :junk-allowed t)))
                         (:bbox (setf bbox (%parse-num-list val)))
                         (:span (setf span (%parse-num-list val)))
                         (:units (setf units (%kw val :pixels)))
                         (:origin (setf origin (%kw val :top-left)))
                         (:lw (setf lw (let ((*read-eval* nil))
                                         (read-from-string val))))
                         (:lh (setf lh (let ((*read-eval* nil))
                                         (read-from-string val))))))))))
    (when (or page bbox span units origin lw lh)
      (doc-extract-protocol:make-provenance-entry
       :page page :bbox bbox :charspan span
       :coord-system (when (or units origin lw lh)
                       (doc-extract-protocol:make-coord-system
                        :origin (or origin :top-left)
                        :units (or units :pixels)
                        :layout-width lw
                        :layout-height lh))))))

(defun %decode-prov (s)
  (when (and s (plusp (length s)))
    (loop for start = 0 then (1+ bar)
          for bar = (position #\| s :start start)
          for part = (subseq s start (or bar (length s)))
          for entry = (%decode-one-prov part)
          when entry collect entry
          while bar)))

(defun %encode-anns (anns)
  (when anns
    (format nil "~{~a~^,~}"
            (mapcar (lambda (ann)
                      (format nil "~d-~d:~a~@[:~a~]"
                              (doc-extract-protocol:annotation-span-start ann)
                              (doc-extract-protocol:annotation-span-end ann)
                              (string-downcase
                               (symbol-name
                                (doc-extract-protocol:annotation-span-kind ann)))
                              (%bound ann 'target)))
                    anns))))

(defun %decode-anns (s)
  (when (and s (plusp (length s)))
    (mapcar
     (lambda (raw)
       (let* ((colon (position #\: raw))
              (range (subseq raw 0 colon))
              (rest (and colon (subseq raw (1+ colon))))
              (dash (position #\- range))
              (start (parse-integer range :end dash))
              (end (parse-integer range :start (1+ dash)))
              (colon2 (and rest (position #\: rest)))
              (kind (%kw (if colon2 (subseq rest 0 colon2) rest)))
              (target (and colon2 (subseq rest (1+ colon2)))))
         (doc-extract-protocol:make-annotation-span
          :start start :end end :kind kind
          :target (and target (plusp (length target)) target))))
     (loop for start = 0 then (1+ comma)
           for comma = (position #\, s :start start)
           collect (subseq s start (or comma (length s)))
           while comma))))

(defun %html-text (s)
  (make-instance 'html-protocol:html-text :data (or s "")))

(defun %el (name &key attributes children)
  (html-protocol:make-html-element name
                                   :attributes attributes
                                   :children children))

(defun %push-attr (attrs name value)
  (if (and value (or (not (stringp value)) (plusp (length value))))
      (cons (cons name (if (stringp value) value (princ-to-string value)))
            attrs)
      attrs))

(defun %block-data-attrs (block profile &key extra)
  (let ((attrs extra)
        (archival (eq profile :archival)))
    (setf attrs (%push-attr attrs "data-demiurge-type"
                            (string-downcase
                             (symbol-name
                              (doc-extract-protocol:block-node-type block)))))
    (when (%bound block 'id)
      (setf attrs (%push-attr attrs "data-demiurge-id"
                              (doc-extract-protocol:block-id block))))
    (let ((layer (or (doc-extract-protocol:block-layer block) :body)))
      (unless (eq layer :body)
        (setf attrs (%push-attr attrs "data-demiurge-layer"
                                (string-downcase (symbol-name layer))))))
    (when (typep block 'doc-extract-protocol:text-block)
      (setf attrs (%push-attr attrs "data-demiurge-kind"
                              (string-downcase
                               (symbol-name
                                (or (doc-extract-protocol:text-block-kind block)
                                    :para))))))
    (when (and (typep block 'doc-extract-protocol:section)
               (doc-extract-protocol:section-level block))
      (setf attrs (%push-attr attrs "data-demiurge-level"
                              (princ-to-string
                               (doc-extract-protocol:section-level block)))))
    (when (and (typep block 'doc-extract-protocol:list-block)
               (doc-extract-protocol:list-block-ordered-p block))
      (setf attrs (%push-attr attrs "data-demiurge-ordered" "true")))
    (when (and (typep block 'doc-extract-protocol:code-block)
               (%bound block 'language))
      (setf attrs (%push-attr attrs "data-demiurge-language"
                              (doc-extract-protocol:code-block-language block))))
    (when (typep block 'doc-extract-protocol:image-block)
      (when (%bound block 'alt)
        (setf attrs (%push-attr attrs "alt"
                                (doc-extract-protocol:image-block-alt block))))
      (when (%bound block 'content-hash)
        (let ((hash (doc-extract-protocol:image-block-content-hash block)))
          (setf attrs (%push-attr attrs "data-demiurge-hash" hash))
          (setf attrs (%push-attr attrs "src" (format nil "hash:~a" hash))))))
    (when (typep block 'doc-extract-protocol:table-block)
      (setf attrs (%push-attr attrs "data-demiurge-rows"
                              (princ-to-string
                               (or (doc-extract-protocol:table-block-rows block) 0))))
      (setf attrs (%push-attr attrs "data-demiurge-cols"
                              (princ-to-string
                               (or (doc-extract-protocol:table-block-cols block) 0)))))
    (when archival
      (when (%bound block 'confidence)
        (setf attrs (%push-attr attrs "data-demiurge-confidence"
                                (princ-to-string
                                 (doc-extract-protocol:block-confidence block)))))
      (let ((encoded (%encode-attrs (doc-extract-protocol:block-attrs block))))
        (when encoded
          (setf attrs (%push-attr attrs "data-demiurge-attrs" encoded))))
      (let ((encoded (%encode-prov (doc-extract-protocol:block-provenance block))))
        (when encoded
          (setf attrs (%push-attr attrs "data-demiurge-provenance" encoded))))
      (when (typep block 'doc-extract-protocol:text-block)
        (let ((encoded (%encode-anns
                        (doc-extract-protocol:text-block-annotations block))))
          (when encoded
            (setf attrs (%push-attr attrs "data-demiurge-ann" encoded))))))
    attrs))

(defun %ann-element (ann text)
  (let* ((kind (or (doc-extract-protocol:annotation-span-kind ann) :emphasis))
         (target (%bound ann 'target))
         (kids (list (%html-text text))))
    (case kind
      (:emphasis (%el "em" :children kids))
      (t
       (let ((attrs (list (cons "data-demiurge-ref"
                                (string-downcase (symbol-name kind))))))
         (when target
           (push (cons "href" target) attrs))
         (%el "a" :attributes attrs :children kids))))))

(defun %wrap-text (text annotations)
  (let* ((text (or text ""))
         (n (length text))
         (anns (sort (copy-list (or annotations nil)) #'<
                     :key #'doc-extract-protocol:annotation-span-start)))
    (unless anns
      (return-from %wrap-text (list (%html-text text))))
    (let ((nodes '())
          (pos 0))
      (dolist (ann anns)
        (let ((s (max 0 (min n (or (doc-extract-protocol:annotation-span-start ann) 0))))
              (e (max 0 (min n (or (doc-extract-protocol:annotation-span-end ann) 0)))))
          (when (> s pos)
            (push (%html-text (subseq text pos s)) nodes))
          (when (> e s)
            (push (%ann-element ann (subseq text s e)) nodes)
            (setf pos (max pos e)))))
      (when (< pos n)
        (push (%html-text (subseq text pos)) nodes))
      (nreverse nodes))))

(defun %heading-name (level)
  (format nil "h~d" (max 1 (min 6 (or level 1)))))

(defun %block->element (block profile)
  (let ((attrs (%block-data-attrs block profile)))
    (etypecase block
      (doc-extract-protocol:section
       (let ((depth (max 1 (min 6 (or (doc-extract-protocol:section-level block) 1)))))
         (%el "section"
              :attributes attrs
              :children
              (cons (%el (%heading-name depth)
                         :children
                         (list (%html-text
                                (or (doc-extract-protocol:section-title block) ""))))
                    (mapcar (lambda (c) (%block->element c profile))
                            (or (doc-extract-protocol:section-children block) nil))))))
      (doc-extract-protocol:text-block
       (let ((kind (or (doc-extract-protocol:text-block-kind block) :para))
             (kids (%wrap-text (doc-extract-protocol:text-block-text block)
                               (when (eq profile :archival)
                                 (doc-extract-protocol:text-block-annotations block)))))
         (if (eq kind :quote)
             (%el "blockquote" :attributes attrs :children kids)
             (%el "p" :attributes attrs :children kids))))
      (doc-extract-protocol:code-block
       (let* ((lang (%bound block 'language))
              (code-attrs (when lang
                            (list (cons "class" (format nil "language-~a" lang))))))
         (%el "pre"
              :attributes attrs
              :children
              (list (%el "code"
                         :attributes code-attrs
                         :children
                         (list (%html-text
                                (or (doc-extract-protocol:code-block-text block)
                                    ""))))))))
      (doc-extract-protocol:list-block
       (%el (if (doc-extract-protocol:list-block-ordered-p block) "ol" "ul")
            :attributes attrs
            :children
            (mapcar (lambda (item)
                      (%el "li"
                           :children (list (%block->element item profile))))
                    (or (doc-extract-protocol:list-block-items block) nil))))
      (doc-extract-protocol:table-block
       (%el "table"
            :attributes attrs
            :children (list (%table-body block profile))))
      (doc-extract-protocol:image-block
       (%el "img" :attributes attrs))
      (doc-extract-protocol:key-value-block
       (%el "dl"
            :attributes attrs
            :children
            (list (%el "dt" :children
                       (list (%html-text
                              (or (doc-extract-protocol:key-value-block-key block)
                                  ""))))
                  (%el "dd" :children
                       (list (%html-text
                              (or (doc-extract-protocol:key-value-block-value block)
                                  ""))))))))))

(defun %table-body (table profile)
  (let* ((rows (or (doc-extract-protocol:table-block-rows table) 0))
         (cols (or (doc-extract-protocol:table-block-cols table) 0))
         (cells (or (doc-extract-protocol:table-block-cells table) nil))
         (starts (make-hash-table :test #'equal))
         (covered (make-hash-table :test #'equal)))
    (dolist (cell cells)
      (let ((r (doc-extract-protocol:table-cell-row cell))
            (c (doc-extract-protocol:table-cell-col cell))
            (rs (or (doc-extract-protocol:table-cell-row-span cell) 1))
            (cs (or (doc-extract-protocol:table-cell-col-span cell) 1)))
        (setf (gethash (cons r c) starts) cell)
        (loop for i from r below (+ r rs)
              do (loop for j from c below (+ c cs)
                       unless (and (= i r) (= j c))
                         do (setf (gethash (cons i j) covered) t)))))
    (%el "tbody"
         :children
         (loop for r from 0 below (max rows 0)
               collect
               (%el "tr"
                    :children
                    (loop for c from 0 below (max cols 0)
                          unless (gethash (cons r c) covered)
                            collect
                            (let ((cell (gethash (cons r c) starts)))
                              (%td-element cell r c profile))))))))

(defun %td-element (cell row col profile)
  (let* ((header (and cell (doc-extract-protocol:table-cell-header-p cell)))
         (rs (if cell (or (doc-extract-protocol:table-cell-row-span cell) 1) 1))
         (cs (if cell (or (doc-extract-protocol:table-cell-col-span cell) 1) 1))
         (attrs (list (cons "data-demiurge-row" (princ-to-string row))
                      (cons "data-demiurge-col" (princ-to-string col))))
         (content (and cell (doc-extract-protocol:table-cell-content cell))))
    (when (and rs (/= rs 1))
      (push (cons "rowspan" (princ-to-string rs)) attrs))
    (when (and cs (/= cs 1))
      (push (cons "colspan" (princ-to-string cs)) attrs))
    (%el (if header "th" "td")
         :attributes attrs
         :children
         (if content
             (mapcar (lambda (b) (%block->element b profile)) content)
             (list (%html-text ""))))))

(defun %document-title (doc)
  (let ((md (doc-extract-protocol:extracted-document-metadata doc)))
    (or (and md (%bound md 'title)
             (doc-extract-protocol:document-metadata-title md))
        "")))

(defun render-html (doc &key (profile :llm) stream)
  (doc-extract-protocol:ensure-ids doc)
  (let* ((title (%document-title doc))
         (html
           (html-protocol:make-html-document
            :children
            (list (make-instance 'html-protocol:html-doctype :name "html")
                  (%el "html"
                       :children
                       (list (%el "head"
                                  :children
                                  (list (%el "meta"
                                             :attributes
                                             (list (cons "charset" "utf-8")))
                                        (%el "title"
                                             :children (list (%html-text title)))))
                             (%el "body"
                                  :children
                                  (list (%el "article"
                                             :children
                                             (mapcar (lambda (b)
                                                       (%block->element b profile))
                                                     (or (doc-extract-protocol:extracted-document-blocks doc)
                                                         nil)))))))))))
    (%emit-text (html-protocol:serialize html) stream)))

(defmethod render-document (doc (format (eql :html)) &key stream (profile :llm)
                            &allow-other-keys)
  (render-html doc :profile profile :stream stream))
