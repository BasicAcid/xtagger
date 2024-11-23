(defpackage :xattr-lister
            (:use :cl)
            (:export :list-files-with-xattrs
                     :find-files-with-tag
                     :main
                     :save-executable))

(in-package :xattr-lister)

(ql:quickload '(:cffi :cl-fad :babel :cl-ppcre))
(use-package :cffi)

(define-foreign-library libc
                        (:unix (:or "libc.so.6" "libc.so")))

(load-foreign-library 'libc)

(defcfun ("llistxattr" %llistxattr) :long
         (path :string)
         (list :pointer)
         (size :size))

(defcfun ("lgetxattr" %lgetxattr) :long
         (path :string)
         (name :string)
         (value :pointer)
         (size :size))

;; Get list of xattr names for a file.
(defun get-xattr-names (path)
  (with-foreign-pointer (buf 1024) ; Adjust buffer size if needed.
                        (let ((len (%llistxattr path buf 1024)))
                          (when (plusp len)
                            (let ((names (make-string-output-stream)))
                              (loop for i from 0 below len
                                    for char = (code-char (mem-ref buf :unsigned-char i))
                                    unless (zerop (char-code char))
                                    do (write-char char names)
                                    else collect (get-output-stream-string names)
                                    and do (get-output-stream-string names)))))))

;; Get value of a specific xattr.
(defun get-xattr-value (path name)
  (with-foreign-pointer (buf 1024)
                        (let ((len (%lgetxattr path name buf 1024)))
                          (when (plusp len)
                            (let ((value (make-array len :element-type '(unsigned-byte 8))))
                              (loop for i from 0 below len
                                    do (setf (aref value i) (mem-ref buf :unsigned-char i)))
                              value)))))

;; Convert byte array to string
(defun bytes-to-string (bytes)
  (when bytes
    (babel:octets-to-string bytes)))

;; Default tag attribute used by KDE Dolphin.
(defparameter *default-tag-attr* "user.xdg.tags")

;; Parse tags from KDE Dolphin format.
(defun parse-tags (tag-string)
  (when tag-string
    (cl-ppcre:split "," tag-string)))

;; Check if a file has a specific tag.
(defun file-has-tag-p (path tag-name)
  (let* ((tags-value (bytes-to-string (get-xattr-value path *default-tag-attr*)))
         (file-tags (parse-tags tags-value)))
    (member tag-name file-tags :test #'string=)))

;; Process a single file for listing.
(defun process-file (path)
  (let ((names (get-xattr-names path)))
    (when names
      (format t "~&File: ~A~%" path)
      (dolist (name names)
        (let ((value (get-xattr-value path name)))
          (when value
            (format t "  ~A = ~A~%" name (bytes-to-string value))))))))

;; Recursive directory traversal.
(defun walk-directory (directory fn)
  (labels ((walk (name)
                 (cond
                  ((not (cl-fad:directory-exists-p name))
                   (funcall fn name))
                  (t
                   (dolist (x (directory (merge-pathnames "*.*" name)))
                     (walk (namestring x)))))))
    (walk (pathname directory))))

;; List files with xattrs.
(defun list-files-with-xattrs (directory)
  (walk-directory directory #'process-file))

;; Find files with specific tag.
(defun find-files-with-tag (directory tag-name)
  "Find all files in DIRECTORY that have the specified TAG-NAME."
  (let ((matching-files nil))
    (flet ((check-file (path)
                       (when (file-has-tag-p path tag-name)
                         (push path matching-files))))
      (walk-directory directory #'check-file))
    (nreverse matching-files)))

;; Get default search path from environment variable or fallback to "."
(defun get-default-path ()
  (or (uiop:getenv "TAGGER_PATH") "."))

;; Updated CLI interface.
(defun main (&optional args)
  (let ((args (or args (uiop:command-line-arguments)))
        (default-path (get-default-path)))
    (cond
     ;; Print usage if no arguments.
     ((null args)
      (format t "Usage:~%")
      (format t "  Find files by tag: xtagger tag <tagname> [directory]~%")
      (format t "  List all tags:     xtagger list [directory]~%")
      (format t "~%")
      (format t "If directory is not specified, TAGGER_PATH environment variable will be used.~%")
      (format t "Current search path: ~A~%" default-path))

     ;; List all tagged files.
     ((string= (first args) "list")
      (list-files-with-xattrs (or (second args) default-path)))

     ;; Find files with specific tag.
     ((string= (first args) "tag")
      (if (second args)
          (dolist (file (find-files-with-tag (or (third args) default-path) (second args)))
            (format t "~A~%" file))
        (format t "Error: Please specify a tag name~%")))

     ;; Unknown command.
     (t (format t "Unknown command: ~A~%" (first args))))))

;; Make it easier to run from SBCL.
(defun save-executable ()
  (sb-ext:save-lisp-and-die
   "xtagger"
   :toplevel #'main
   :executable t))
