(in-package #:doc-extract-render)

(defun canonicalize-render-format (format)
  "Keyword for a render/parse format. :md → :markdown, :htm → :html."
  (let ((fmt (doc-extract-protocol:canonicalize-format format)))
    (case fmt
      ((:md :markdown) :markdown)
      ((:htm :html) :html)
      (t fmt))))

(defun %coerce-text (input)
  (etypecase input
    (string input)
    (pathname
     (with-open-file (s input :direction :input)
       (let ((buf (make-string (file-length s))))
         (read-sequence buf s)
         buf)))
    ((vector (unsigned-byte 8))
     (%utf8-decode input))))

(defun %utf8-decode (octets)
  (let ((out (make-array (length octets)
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0))
        (n (length octets))
        (i 0))
    (loop while (< i n)
          for b = (aref octets i)
          do (cond
               ((< b #x80)
                (vector-push-extend (code-char b) out)
                (incf i))
               ((< b #xE0)
                (vector-push-extend
                 (code-char (logior (ash (logand b #x1F) 6)
                                    (logand (aref octets (1+ i)) #x3F)))
                 out)
                (incf i 2))
               ((< b #xF0)
                (vector-push-extend
                 (code-char (logior (ash (logand b #x0F) 12)
                                    (ash (logand (aref octets (1+ i)) #x3F) 6)
                                    (logand (aref octets (+ i 2)) #x3F)))
                 out)
                (incf i 3))
               (t
                (vector-push-extend
                 (code-char (logior (ash (logand b #x07) 18)
                                    (ash (logand (aref octets (1+ i)) #x3F) 12)
                                    (ash (logand (aref octets (+ i 2)) #x3F) 6)
                                    (logand (aref octets (+ i 3)) #x3F)))
                 out)
                (incf i 4))))
    (coerce out 'string)))

(defun %emit-text (text stream)
  (if stream
      (progn (write-string text stream) stream)
      text))

(defgeneric render-document (doc format &key stream profile &allow-other-keys)
  (:documentation "Serialize DOC as FORMAT (:markdown :html :pdf).
   PROFILE :llm omits provenance; :archival keeps ids, provenance, attrs."))

(defgeneric parse-document (input format &key &allow-other-keys)
  (:documentation "Parse INPUT (string, pathname, or UTF-8 octets) as FORMAT."))

(defmethod render-document :around (doc format &rest args
                                    &key stream profile &allow-other-keys)
  (declare (ignore stream profile))
  (check-type doc doc-extract-protocol:extracted-document)
  (let ((fmt (canonicalize-render-format format)))
    (if (eq fmt format)
        (call-next-method)
        (apply #'render-document doc fmt args))))

(defmethod parse-document :around (input format &rest args &key &allow-other-keys)
  (let ((fmt (canonicalize-render-format format)))
    (if (eq fmt format)
        (call-next-method)
        (apply #'parse-document input fmt args))))

(defmethod render-document (doc format &key stream profile &allow-other-keys)
  (declare (ignore stream profile))
  (restart-case
      (error 'unknown-render-format
             :format format
             :message (format nil "no renderer for ~s" format))
    (use-value (value)
      :report "Use a supplied render result"
      value)))

(defmethod parse-document (input format &key &allow-other-keys)
  (declare (ignore input))
  (restart-case
      (error 'unknown-render-format
             :format format
             :message (format nil "no parser for ~s" format))
    (use-value (value)
      :report "Use a supplied parsed document"
      value)))

(defun %html-comment-line-p (line)
  "True for attribute comments. {close=section} is structure, not metadata."
  (let ((s (string-trim '(#\Space #\Tab #\Return) line)))
    (and (>= (length s) 7)
         (string= (subseq s 0 4) "<!--")
         (search "-->" s)
         (not (search "close=section" s)))))

(defun %strip-fence-language (line)
  "```lisp → ``` so parse-markup will not touch an unbound language slot."
  (let ((s (string-right-trim '(#\Space #\Tab #\Return) line)))
    (cond
      ((and (>= (length s) 3) (string= (subseq s 0 3) "```")
            (> (length s) 3))
       "```")
      ((and (>= (length s) 4) (string= (subseq s 0 4) "~~~~")
            (> (length s) 4))
       "~~~~")
      (t line))))

(defun %strip-markdown-comments (text)
  (with-output-to-string (out)
    (let ((start 0)
          (n (length text)))
      (loop for i from 0 below n
            when (char= (char text i) #\Newline)
              do (let ((line (subseq text start i)))
                   (unless (%html-comment-line-p line)
                     (write-string (%strip-fence-language line) out)
                     (write-char #\Newline out))
                   (setf start (1+ i))))
      (let ((line (subseq text start)))
        (unless (%html-comment-line-p line)
          (write-string (%strip-fence-language line) out))))))

(defun strip-render-attributes (input &optional (format :html))
  "Drop demiurge metadata so a re-parse can only recover :structure.
   :html removes data-demiurge-* attributes; :markdown removes attribute comments."
  (ecase (canonicalize-render-format format)
    (:markdown (%strip-markdown-comments (%coerce-text input)))
    (:html (%strip-html-data-attributes (%coerce-text input)))))
