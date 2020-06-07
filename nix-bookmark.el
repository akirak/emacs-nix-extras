(require 'eieio)
(require 'eieio-base)
(require 'ts)
(require 'f)

(defcustom nix-bookmark-persist-directory
  (expand-file-name "nix-bookmark/store" user-emacs-directory)
  "Directory in which objects are stored."
  :type 'directory)

;;;; Classes
;;;;; nix-bookmark-store class

(defvar nix-bookmark-store-instances nil
  "Track instances of `nix-bookmark-store'.")

(defclass nix-bookmark-store (eieio-instance-tracker
                              eieio-instance-inheritor
                              eieio-persistent)
  ((tracking-symbol :initform nix-bookmark-store-instances)
   ;; An empty file name is set to prevent an error on
   ;; `eieio-persistent'. An actual value should be set by implementing
   ;; `nix-bookmark-store-filename' method.
   (file :initform "")
   (creation-time :initform (ts-now))
   (description :initarg :description :type (or null string)
                :initform nil)
   (store-path :initarg :store-path
               :type (or null string))))

;;;;;; Generic methods

(defgeneric nix-bookmark-store-filename ((obj nix-bookmark-store))
  "Return a filename without directory for persisting OBJ.

Actually, it is a filename suffix.  The timestamp of the object is
prepended to the filename to ensure uniqueness.")

(defgeneric nix-bookmark-format-source ((obj nix-bookmark-store))
  "Return a string to refer to OBJ for displaying.")

;;;;;; Functions

(defun nix-bookmark-save-store (obj)
  "Persist OBJ in `nix-bookmark-persist-directory'."
  (cl-check-type obj nix-bookmark-store)
  (make-directory nix-bookmark-persist-directory t)
  (eieio-persistent-save obj (nix-bookmark--store-filepath obj)))

(defun nix-bookmark-delete (obj)
  "Delete OBJ from the instance list."
  (cl-check-type obj nix-bookmark-store)
  (let ((filepath (nix-bookmark--store-filepath obj)))
    (delete-instance obj)
    (when (f-exists-p filepath)
      (delete-file filepath))))

(defun nix-bookmark--store-filepath (obj)
  "Return a fullpath for persisting OBJ."
  (f-join nix-bookmark-persist-directory
          (concat (ts-format "%s" (oref obj creation-time))
                  "-"
                  (nix-bookmark-store-filename obj))))

(defun nix-bookmark--format-store (obj)
  "Format OBJ for displaying in `completing-read'."
  (let ((reltime (nix-bookmark--pretty-time (oref obj creation-time)))
        (title (nix-bookmark-format-store obj))
        (description (oref obj description)))
    (format "%-12s %-20s  %s"
            reltime
            title
            (if description
                (propertize description 'face 'font-lock-comment-face)
              ""))))

(defun nix-bookmark--sort-by-creation-time (list)
  "Sort a LIST by creation time."
  (cl-sort list
           (lambda (a b)
             (> (ts-diff (oref a creation-time)
                         (oref b creation-time))
                0))))

(defun nix-bookmark-find-source (&optional dir)
  (let* ((root (nix-bookmark--project-root-dir (or dir default-directory))))
    (if root
        (eieio-instance-tracker-find (string-remove-suffix "/" root)
                                     'store-path
                                     'nix-bookmark-store-instances)
      (user-error "Not in /nix/store"))))

;;;;; Subclasses of the store class

;; You have to implement the following methods of each subclass:
;;
;; * `nix-bookmark-store-filename'
;; * `nix-bookmark-format-source'

;;;;;; nix-bookmark-store-git-url

(defclass nix-bookmark-git-url-store (nix-bookmark-store)
  ((git-url :initarg :git-url :type string)))

(defmethod nix-bookmark-store-filename ((obj nix-bookmark-git-url-store))
  (nix-bookmark--filename-escape-url (oref obj git-url)))

(defmethod nix-bookmark-format-store ((obj nix-bookmark-git-url-store))
  (oref obj git-url))

;;;; Commands
(defun nix-bookmark-load-stores ()
  "Restore store information from `nix-bookmark-persist-directory'."
  (interactive)
  (mapc #'eieio-persistent-read
        ;; Load nothing if the directory does not exist
        (ignore-errors
          (directory-files nix-bookmark-persist-directory
                           'full))))

(cl-defun nix-bookmark-browse-stores (&key (prompt "Nix stores: ")
                                           (action #'dired))
  (interactive)
  (let* ((candidates (-map (lambda (store)
                             (propertize (nix-bookmark--format-store store)
                                         'nix-bookmark-store store))
                           (nix-bookmark--sort-by-creation-time
                            nix-bookmark-store-instances)))
         (input (completing-read prompt candidates nil 'match))
         (obj (get-char-property 0 'nix-bookmark-store input))
         (store-path (oref obj store-path)))
    (cond
     ((and (stringp store-path) (f-directory-p store-path))
      (funcall action store-path))
     ((not (stringp store-path))
      (error "Object returned a non-string store path %s from %s" store-path
             (substring-no-properties input)))
     ((not (f-directory-p store-path))
      (error "Non-existing directory %s" store-path))
     (t
      (error "Unexpected situation")))))

;;;; Utility function

(defun nix-bookmark--filename-escape-url (url)
  "Convert URL into a filename-safe string."
  (->> url
       ;; Eliminate scheme
       (replace-regexp-in-string (rx bol (+ (any alpha)) ":" (* "/")) "")
       ;; Eliminate fragment
       (replace-regexp-in-string (rx "#" (* anything) eol) "")
       ;; Eliminate query
       (replace-regexp-in-string (rx "?" (* anything) eol) "")
       ;; Eliminate file extensions in the last segment for avoiding confusion
       (replace-regexp-in-string (rx (+ "." (+ (not (any "/")))) eol) "")
       ;; Replace slashes with two underscores
       (replace-regexp-in-string "/" "__")
       ;; Replace strings other than alnums, hyphens, and multibyte
       ;; characters with underscores
       (replace-regexp-in-string (rx (not (any "-" alnum nonascii))) "_")))

(defun nix-bookmark--pretty-time (ts)
  (let ((seconds (ts-diff (ts-now) ts)))
    (cond
     ((< seconds 60)
      "just now")
     ;; less than 2 hours
     ((< seconds 7200)
      (format "%d minutes" (/ seconds 60)))
     ;; less than 24 hours
     ((< seconds 86400)
      (format "%d hours" (/ seconds 3600)))
     ;; less than 30 days
     ((< seconds 2592000)
      (format "%d days" (/ seconds 86400)))
     (t
      (ts-format "%F" ts)))))

(defun nix-bookmark--project-root-dir (directory)
  (save-match-data
    (when (string-match (rx bol "/nix/store/" (+ (not (any "/")))
                            (optional "/"))
                        directory)
      (match-string 0 directory))))

;;;; Integration with project.el

(defun nix-bookmark-project-root (directory)
  (-some-> (nix-bookmark--project-root-dir directory)
    (cons 'nix-store)))

(defcustom nix-bookmark-enable-project t
  "FIXME"
  :type 'boolean
  :set (lambda (sym value)
         (set sym value)
         (when (require 'project nil t)
           (cond
            (value
             (add-to-list 'project-find-functions
                          #'nix-bookmark-project-root
                          'append))
            (t
             (cl-delete #'nix-bookmark-project-root project-find-functions))))))
