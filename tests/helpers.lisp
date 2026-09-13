(in-package #:doc-extract-render/tests)

;;; Copied/adapted from doc-extract-protocol/tests/helpers.lisp — do not
;;; import that test package.

(defparameter *property-doc-count* 200
  "Generated documents per markdown/html property suite.")

(defun rng (state)
  (mod (+ (* state 1103515245) 12345) 2147483648))

(defun rng-pick (state items)
  (let* ((n (rng state))
         (item (nth (mod n (length items)) items)))
    (values item n)))

(defun make-sample-doc (&key (title "Sample") unicode-p)
  (let* ((para (doc-extract-protocol:make-text-block
                :text (if unicode-p
                          "café 日本語 naïve"
                          "Hello world.")
                :annotations
                (list (doc-extract-protocol:make-annotation-span
                       :start 0 :end 5 :kind :emphasis))
                :provenance
                (list (doc-extract-protocol:make-provenance-entry
                       :page 1 :bbox '(0 0 100 20)
                       :coord-system (doc-extract-protocol:make-coord-system
                                      :units :pixels :origin :top-left)
                       :charspan '(0 5)))
                :attrs '(:role :title)))
         (quote (doc-extract-protocol:make-text-block
                 :kind :quote :text "Quoted line"))
         (code (doc-extract-protocol:make-code-block
                :language "lisp" :text "(+ 1 2)"))
         (items (list (doc-extract-protocol:make-text-block :text "one")
                      (doc-extract-protocol:make-text-block :text "two")))
         (lst (doc-extract-protocol:make-list-block :items items))
         (table (doc-extract-protocol:make-table-block
                 :rows 2 :cols 2
                 :cells
                 (list (doc-extract-protocol:make-table-cell
                        :row 0 :col 0 :header-p t :row-span 1 :col-span 1
                        :content (list (doc-extract-protocol:make-text-block :text "H1")))
                       (doc-extract-protocol:make-table-cell
                        :row 0 :col 1 :header-p t
                        :content (list (doc-extract-protocol:make-text-block :text "H2")))
                       (doc-extract-protocol:make-table-cell
                        :row 1 :col 0
                        :content (list (doc-extract-protocol:make-text-block :text "a")))
                       (doc-extract-protocol:make-table-cell
                        :row 1 :col 1
                        :content (list (doc-extract-protocol:make-text-block :text "b"))))))
         (img (doc-extract-protocol:make-image-block
               :alt "fig" :content-hash "abc123"))
         (kv (doc-extract-protocol:make-key-value-block
              :key "Name" :value "Ada"))
         (foot (doc-extract-protocol:make-text-block
                :text "Page 1" :layer :furniture
                :attrs '(:role :page-footer)))
         (sub (doc-extract-protocol:make-section-block
               :title "Subsection" :level 2
               :children (list quote code)))
         (top (doc-extract-protocol:make-section-block
               :title title :level 1
               :children (list para sub lst table img kv foot))))
    (doc-extract-protocol:ensure-ids
     (doc-extract-protocol:make-extracted-document
      :metadata (doc-extract-protocol:make-document-metadata
                 :title title :language "en" :mimetype "text/html")
      :pages (list (doc-extract-protocol:make-page-info :number 1
                                                       :width 612 :height 792))
      :blocks (list top)))))

(defun gen-document (seed)
  "Deterministic pseudo-random document (adapted from protocol helpers)."
  (let ((state seed)
        (blocks '()))
    (flet ((next ()
             (setf state (rng state))
             state)
           (pick (items)
             (multiple-value-bind (item n) (rng-pick state items)
               (setf state n)
               item)))
      (let* ((n-sec (1+ (mod (next) 3)))
             (words '("alpha" "beta" "gamma" "delta" "ελληνικά" "café")))
        (dotimes (s n-sec)
          (let* ((title (format nil "S~d-~a" s (pick words)))
                 (kids '())
                 (n-kids (1+ (mod (next) 4))))
            (dotimes (k n-kids)
              (let ((kind (pick '(:para :quote :code :list :table :image :kv :furn))))
                (push
                 (ecase kind
                   (:para
                    (doc-extract-protocol:make-text-block
                     :text (format nil "~a ~a ~d" (pick words) (pick words)
                                   (mod (next) 100))
                     :annotations
                     (when (zerop (mod (next) 2))
                       (list (doc-extract-protocol:make-annotation-span
                              :start 0 :end 3 :kind :emphasis)))
                     :provenance
                     (when (zerop (mod (next) 2))
                       (list (doc-extract-protocol:make-provenance-entry
                              :page (1+ (mod (next) 3))
                              :bbox (list 0 0 10 10))))
                     :attrs (when (zerop (mod (next) 3))
                              '(:role :section-heading))))
                   (:quote
                    (doc-extract-protocol:make-text-block
                     :kind :quote :text (pick words)))
                   (:code
                    (doc-extract-protocol:make-code-block
                     :language (pick '("lisp" "text"))
                     :text (format nil "(~a)" (pick words))))
                   (:list
                    (doc-extract-protocol:make-list-block
                     :ordered-p (zerop (mod (next) 2))
                     :items (list (doc-extract-protocol:make-text-block
                                   :text (pick words))
                                  (doc-extract-protocol:make-text-block
                                   :text (pick words)))))
                   (:table
                    (doc-extract-protocol:make-table-block
                     :rows 2 :cols 2
                     :cells
                     (loop for r from 0 below 2
                           nconc
                           (loop for c from 0 below 2
                                 collect
                                 (doc-extract-protocol:make-table-cell
                                  :row r :col c
                                  :header-p (zerop r)
                                  :content
                                  (list (doc-extract-protocol:make-text-block
                                         :text (pick words))))))))
                   (:image
                    (doc-extract-protocol:make-image-block
                     :alt (pick words)
                     :content-hash (format nil "h~d" (mod (next) 9999))))
                   (:kv
                    (doc-extract-protocol:make-key-value-block
                     :key (pick words) :value (pick words)))
                   (:furn
                    (doc-extract-protocol:make-text-block
                     :text (format nil "footer ~d" s)
                     :layer :furniture
                     :attrs '(:role :page-footer))))
                 kids)))
            (push (doc-extract-protocol:make-section-block
                   :title title :level 1
                   :children (nreverse kids))
                  blocks)))
        (doc-extract-protocol:ensure-ids
         (doc-extract-protocol:make-extracted-document
          :metadata (doc-extract-protocol:make-document-metadata
                     :title (format nil "gen-~d" seed))
          :blocks (nreverse blocks)))))))

(defun fixture-path (name)
  (asdf:system-relative-pathname "doc-extract-render"
                                 (format nil "tests/fixtures/~a" name)))
