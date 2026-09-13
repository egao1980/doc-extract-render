(in-package #:doc-extract-render)

;;; Walk html-protocol DOM. Restore blocks from data-demiurge-* when
;;; present; otherwise infer structure (fresh ids via ensure-ids).

(defun %el-name (node)
  (and (html-protocol:html-element-p node)
       (html-protocol:html-element-name node)))

(defun %el-attr (node name)
  (and (html-protocol:html-element-p node)
       (html-protocol:element-attr node name)))

(defun %data (node key)
  (%el-attr node (format nil "data-demiurge-~a" key)))

(defun %typed-p (node)
  (and (html-protocol:html-element-p node)
       (%data node "type")))

(defun %heading-depth (name)
  (when (and name (= (length name) 2)
             (char-equal (char name 0) #\h)
             (digit-char-p (char name 1))
             (char<= (char name 1) #\6)
             (char>= (char name 1) #\1))
    (- (char-code (char-downcase (char name 1))) (char-code #\0))))

(defun %element-children (node)
  (cond
    ((html-protocol:html-document-p node)
     (html-protocol:html-document-children node))
    ((html-protocol:html-element-p node)
     (html-protocol:html-element-children node))
    (t nil)))

(defun %element-kids (node)
  (remove-if-not #'html-protocol:html-element-p (%element-children node)))

(defun %ws-text-p (node)
  (and (html-protocol:html-text-p node)
       (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
              (html-protocol:html-text-data node))))

(defun %parse-int (s)
  (and s (parse-integer s :junk-allowed t)))

(defun %apply-common (block node)
  (let ((id (%data node "id"))
        (layer (%data node "layer"))
        (conf (%data node "confidence"))
        (attrs (%decode-attrs (%data node "attrs")))
        (prov (%decode-prov (%data node "provenance"))))
    (when id
      (setf (doc-extract-protocol:block-id block) id))
    (when layer
      (setf (doc-extract-protocol:block-layer block) (%kw layer :body)))
    (when conf
      (setf (doc-extract-protocol:block-confidence block)
            (let ((*read-eval* nil))
              (read-from-string conf))))
    (when attrs
      (setf (doc-extract-protocol:block-attrs block) attrs))
    (when prov
      (setf (doc-extract-protocol:block-provenance block) prov))
    (when (typep block 'doc-extract-protocol:text-block)
      (let ((anns (%decode-anns (%data node "ann"))))
        (when anns
          (setf (doc-extract-protocol:text-block-annotations block) anns))))
    block))

(defun %own-text (node)
  "Document-order descendant text. html-protocol:element-text nreverses an
   already-ordered walk, which swaps annotation fragments."
  (labels ((walk (n acc)
             (cond
               ((html-protocol:html-text-p n)
                (cons (html-protocol:html-text-data n) acc))
               ((html-protocol:html-element-p n)
                (reduce #'walk (html-protocol:html-element-children n)
                        :initial-value acc :from-end t))
               ((html-protocol:html-document-p n)
                (reduce #'walk (html-protocol:html-document-children n)
                        :initial-value acc :from-end t))
               (t acc))))
    (apply #'concatenate 'string (walk node nil))))

(defun %text-of (node)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (%own-text node)))

(defun %raw-text-of (node)
  (%own-text node))

(defun %restore-typed (node)
  (let* ((type (%kw (%data node "type") :text))
         (blk (ecase type
                (:section
                 (let ((title "")
                       (kids '())
                       (level (or (%parse-int (%data node "level")) 1)))
                   (dolist (child (%element-children node))
                     (cond
                       ((and (html-protocol:html-element-p child)
                             (%heading-depth (%el-name child)))
                        (setf title (%own-text child)))
                       ((html-protocol:html-element-p child)
                        (let ((b (%node->block child)))
                          (when b (push b kids))))
                       (t nil)))
                   (doc-extract-protocol:make-section-block
                    :title title :level level :children kids)))
                (:text
                 (doc-extract-protocol:make-text-block
                  :kind (%kw (%data node "kind")
                             (if (string-equal (%el-name node) "blockquote")
                                 :quote
                                 :para))
                  :text (%raw-text-of node)))
                (:list
                 (doc-extract-protocol:make-list-block
                  :ordered-p (or (equal (%data node "ordered") "true")
                                 (string-equal (%el-name node) "ol"))
                  :items (mapcar #'%li-item (%element-kids node))))
                (:code
                 (doc-extract-protocol:make-code-block
                  :language (or (%data node "language")
                                (%code-language node))
                  :text (string-right-trim '(#\Newline) (%raw-text-of node))))
                (:table (%restore-table node))
                (:image
                 (let ((hash (or (%data node "hash")
                                 (%hash-from-src (%el-attr node "src")))))
                   (doc-extract-protocol:make-image-block
                    :alt (or (%el-attr node "alt") (%data node "alt"))
                    :content-hash hash)))
                (:key-value
                 (let ((dt (find-if (lambda (e) (string-equal (%el-name e) "dt"))
                                    (%element-kids node)))
                       (dd (find-if (lambda (e) (string-equal (%el-name e) "dd"))
                                    (%element-kids node))))
                   (doc-extract-protocol:make-key-value-block
                    :key (if dt (%text-of dt) "")
                    :value (if dd (%text-of dd) "")))))))
    (%apply-common blk node)
    blk))

(defun %code-language (node)
  (let* ((code (find-if (lambda (e) (string-equal (%el-name e) "code"))
                        (%element-kids node)))
         (class (and code (%el-attr code "class"))))
    (when (and class (>= (length class) 9)
               (string-equal (subseq class 0 9) "language-"))
      (subseq class 9))))

(defun %hash-from-src (src)
  (when (and src (>= (length src) 5)
             (string-equal (subseq src 0 5) "hash:"))
    (subseq src 5)))

(defun %li-item (li)
  (let ((typed (find-if #'%typed-p (%element-kids li))))
    (cond
      ((%typed-p li) (%restore-typed li))
      (typed (%node->block typed))
      ((= 1 (length (%element-kids li)))
       (%node->block (first (%element-kids li))))
      (t
       (doc-extract-protocol:make-text-block :text (%text-of li))))))

(defun %restore-table (node)
  (let* ((rows-attr (%parse-int (%data node "rows")))
         (cols-attr (%parse-int (%data node "cols")))
         (trs '())
         (cells '())
         (max-col 0)
         (r 0))
    (labels ((walk (n)
               (dolist (c (%element-kids n))
                 (if (string-equal (%el-name c) "tr")
                     (push c trs)
                     (walk c)))))
      (walk node))
    (setf trs (nreverse trs))
    (dolist (tr trs)
      (let ((c 0))
        (dolist (td (%element-kids tr))
          (when (member (%el-name td) '("td" "th") :test #'string-equal)
            (let* ((row (or (%parse-int (%el-attr td "data-demiurge-row")) r))
                   (col (or (%parse-int (%el-attr td "data-demiurge-col")) c))
                   (rs (or (%parse-int (%el-attr td "rowspan")) 1))
                   (cs (or (%parse-int (%el-attr td "colspan")) 1))
                   (header (or (string-equal (%el-name td) "th")
                               (equal (%el-attr td "data-demiurge-header")
                                      "true")))
                   (kids (remove-if #'%ws-text-p (%element-children td)))
                   (content (if (find-if #'html-protocol:html-element-p kids)
                                (loop for k in kids
                                      when (html-protocol:html-element-p k)
                                        collect (%node->block k))
                                (list (doc-extract-protocol:make-text-block
                                       :text (%text-of td))))))
              (push (doc-extract-protocol:make-table-cell
                     :row row :col col
                     :row-span rs :col-span cs
                     :header-p header
                     :content (remove nil content))
                    cells)
              (setf max-col (max max-col (+ col cs))
                    c (+ col cs))))))
      (incf r))
    (doc-extract-protocol:make-table-block
     :rows (or rows-attr r)
     :cols (or cols-attr max-col)
     :cells (nreverse cells))))

(defun %infer-leaf (node)
  (let ((name (%el-name node)))
    (cond
      ((null name) nil)
      ((string-equal name "section")
       (let ((title "")
             (level 1)
             (kids '()))
         (dolist (child (%element-children node))
           (cond
             ((and (html-protocol:html-element-p child)
                   (%heading-depth (%el-name child)))
              (setf title (%own-text child)
                    level (%heading-depth (%el-name child))))
             ((html-protocol:html-element-p child)
              (let ((b (%node->block child)))
                (when b (push b kids))))))
         (doc-extract-protocol:make-section-block
          :title title :level level :children kids)))
      ((%heading-depth name)
       (doc-extract-protocol:make-section-block
        :title (%own-text node)
        :level (%heading-depth name)))
      ((string-equal name "p")
       (doc-extract-protocol:make-text-block :kind :para :text (%raw-text-of node)))
      ((string-equal name "blockquote")
       (doc-extract-protocol:make-text-block :kind :quote :text (%raw-text-of node)))
      ((or (string-equal name "pre") (string-equal name "code"))
       (doc-extract-protocol:make-code-block
        :language (%code-language node)
        :text (string-right-trim '(#\Newline) (%raw-text-of node))))
      ((or (string-equal name "ul") (string-equal name "ol"))
       (doc-extract-protocol:make-list-block
        :ordered-p (string-equal name "ol")
        :items (mapcar #'%li-item
                       (remove-if-not
                        (lambda (e) (string-equal (%el-name e) "li"))
                        (%element-kids node)))))
      ((string-equal name "table")
       (%restore-table node))
      ((string-equal name "img")
       (doc-extract-protocol:make-image-block
        :alt (%el-attr node "alt")
        :content-hash (%hash-from-src (%el-attr node "src"))))
      ((string-equal name "dl")
       (let ((dt (find-if (lambda (e) (string-equal (%el-name e) "dt"))
                          (%element-kids node)))
             (dd (find-if (lambda (e) (string-equal (%el-name e) "dd"))
                          (%element-kids node))))
         (doc-extract-protocol:make-key-value-block
          :key (if dt (%text-of dt) "")
          :value (if dd (%text-of dd) ""))))
      ((member name '("article" "main" "div" "body" "html") :test #'string-equal)
       nil)
      ((member name '("header" "footer" "aside" "nav") :test #'string-equal)
       (let ((text (%text-of node)))
         (when (plusp (length text))
           (doc-extract-protocol:make-text-block
            :text text :layer :furniture))))
      (t
       (let ((text (%text-of node)))
         (when (plusp (length text))
           (doc-extract-protocol:make-text-block :text text)))))))

(defun %node->block (node)
  (cond
    ((not (html-protocol:html-element-p node)) nil)
    ((%typed-p node) (%restore-typed node))
    (t (%infer-leaf node))))

(defun %infer-stream (nodes)
  "Heading-driven nesting for foreign HTML without section wrappers."
  (let ((roots '())
        (stack '()))
    (labels ((add (b)
               (if stack
                   (push b (doc-extract-protocol:section-children
                            (cdr (first stack))))
                   (push b roots)))
             (fix (b)
               (when (typep b 'doc-extract-protocol:section)
                 (setf (doc-extract-protocol:section-children b)
                       (nreverse (doc-extract-protocol:section-children b)))
                 (mapc #'fix (doc-extract-protocol:section-children b)))))
      (dolist (node nodes)
        (unless (or (%ws-text-p node)
                    (html-protocol:html-comment-p node)
                    (html-protocol:html-doctype-p node))
          (let* ((name (%el-name node))
                 (depth (and name (%heading-depth name)))
                 (typed (%typed-p node)))
            (cond
              ((and depth (not typed)
                    (not (string-equal name "section")))
               (let ((sec (doc-extract-protocol:make-section-block
                           :title (%own-text node)
                           :level depth)))
                 (loop while (and stack (>= (car (first stack)) depth))
                       do (pop stack))
                 (if stack
                     (push sec (doc-extract-protocol:section-children
                                (cdr (first stack))))
                     (push sec roots))
                 (push (cons depth sec) stack)))
              (t
               (let ((b (%node->block node)))
                 (when b
                   (if (typep b 'doc-extract-protocol:section)
                       (let ((depth (or (doc-extract-protocol:section-level b) 1)))
                         (loop while (and stack (>= (car (first stack)) depth))
                               do (pop stack))
                         (if stack
                             (push b (doc-extract-protocol:section-children
                                      (cdr (first stack))))
                             (push b roots))
                         (push (cons depth b) stack))
                       (add b)))))))))
      (mapc #'fix roots)
      (nreverse roots))))

(defun %body-root (doc)
  (or (first (html-protocol:select doc "article"))
      (first (html-protocol:select doc "body"))
      (html-protocol:document-root doc)
      doc))

(defun %strip-html-data-attributes (html)
  (let ((doc (html-protocol:parse html)))
    (labels ((walk (node)
               (when (html-protocol:html-element-p node)
                 (setf (html-protocol:html-element-attributes node)
                       (remove-if
                        (lambda (pair)
                          (let ((name (car pair)))
                            (and (stringp name)
                                 (>= (length name) 14)
                                 (string-equal (subseq name 0 14)
                                               "data-demiurge-"))))
                        (html-protocol:html-element-attributes node)))
                 (mapc #'walk (html-protocol:html-element-children node)))
               (when (html-protocol:html-document-p node)
                 (mapc #'walk (html-protocol:html-document-children node)))))
      (walk doc)
      (html-protocol:serialize doc))))

(defun parse-html (input)
  (unless (doc-extract-protocol:html-backend-bound-p)
    (error 'render-error
           :format :html
           :message "*html-backend* is unbound — load html-backend-plump"))
  (let* ((text (%coerce-text input))
         (dom (html-protocol:parse text))
         (root (%body-root dom))
         (nodes (if (html-protocol:html-element-p root)
                    (%element-children root)
                    (html-protocol:html-document-children dom)))
         (blocks (%infer-stream nodes)))
    (doc-extract-protocol:ensure-ids
     (doc-extract-protocol:make-extracted-document :blocks blocks))))

(defmethod parse-document (input (format (eql :html)) &key &allow-other-keys)
  (parse-html input))
