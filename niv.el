(require 'dash)
(require 'f)
(require 'json)

(defgroup niv nil
  "FIXME")

(defconst niv-github-url-template
  "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz")

(defcustom niv-path
  '(magit-list-repos)
  "List of directories in which this package looks for
  \"sources.json\"."
  :type '(repeat (or string function)))

(defcustom niv-format-entry-function #'niv--format-entry
  "FIXME"
  :type 'function)

(cl-defstruct niv-source-entry name context attrs)

(cl-defstruct niv-source-cache time sha1 data)

(defvar niv-sources-cache (make-hash-table :size 500))

(defun niv--project-root ()
  (or (vc-root-dir)
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "nix")))

(defun niv--expand-path ()
  (->> niv-path
       (-map (lambda (item)
               (cl-etypecase item
                 (string item)
                 (function (when (fboundp item) (funcall item))))))
       (-flatten-n 1)
       (-map #'file-truename)
       (-uniq)))

(defun niv--locate-source-file (root)
  (let ((file (f-join root "nix" "sources.json")))
    (when (f-exists-p file)
      file)))

(cl-defun niv--find-source-files (&key exclude-current)
  (let ((other-projects (niv--expand-path)))
    (->> (if exclude-current
             other-projects
           (cons (or (niv--project-root) default-directory)
                 other-projects))
         (-map #'niv--locate-source-file)
         (delq nil))))

(defun niv--read-sources (file)
  (let* ((json-object-type 'alist)
         (items (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (json-read))))
    (cl-loop for (name . alist) in items
             collect (make-niv-source-entry :name name
                                            :context file
                                            :attrs alist))))

(defun niv--sha1-file (file)
  "Retrieve sha1sum of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (sha1 (current-buffer))))

(defun niv--read-sources-with-cache (file)
  (let* ((cached (gethash file niv-sources-cache))
         (attrs (file-attributes file))
         (mtime (float-time (file-attribute-modification-time attrs)))
         ;; sha1 hash of the file. Retrieved when necessary
         sha1)
    (if (and cached
             (or (< mtime (niv-source-cache-time cached))
                 (equal (niv-source-cache-sha1 cached)
                        (setq sha1 (niv--sha1-file file)))))
        (niv-source-cache-data cached)
      (let ((data (niv--read-sources file)))
        (make-niv-source-cache :time (float-time)
                               :sha1 (or sha1 (niv--sha1-file file))
                               :data data)
        data))))

(defun niv--project-dependencies ()
  (-some->> (niv--project-root)
    (niv--locate-source-file)
    (niv--read-sources-with-cache)))

(defun niv--path-entries ()
  (->> (niv--find-source-files)
       (-map #'niv--read-sources-with-cache)
       (-flatten-n 1)))

(defun niv--format-entry (entry)
  "Format `niv-source-entry'."
  (cl-check-type entry niv-source-entry)
  (let* ((name (niv-source-entry-name entry))
         (attrs (niv-source-entry-attrs entry))
         (owner (alist-get 'owner attrs))
         (repo (alist-get 'repo attrs))
         (description (alist-get 'description attrs)))
    (concat (if (equal niv-github-url-template
                       (alist-get 'url_template attrs))
                (format "%s/%s" owner repo)
              name)
            ": "
            description)))

(defun niv-same-repo-1 (s1 s2)
  "Two sources point to the repository, excluding branches and revisions."
  (let ((a1 (niv-source-entry-attrs s1))
        (a2 (niv-source-entry-attrs s2)))
    (and (equal (alist-get 'repo a1) (alist-get 'repo a2))
         (equal (alist-get 'owner a1) (alist-get 'owner a2))
         (equal (alist-get 'url_template a1) (alist-get 'url_template a2)))))

(cl-defun niv-completing-read-repo (prompt &key action
                                           (scope t)
                                           sort-fn
                                           (equal-fn #'niv-same-repo-1))
  (let* ((candidates (--> (cl-ecase scope
                            (t (niv--path-entries))
                            (nil (niv--project-dependencies)))
                          (cl-remove-duplicates it :test equal-fn)
                          (if sort-fn
                              (cl-sort it sort-fn)
                            it)
                          (-map (lambda (entry)
                                  (propertize (funcall niv-format-entry-function entry)
                                              'niv-entry entry))
                                it)))
         (selection (completing-read prompt candidates))
         (entry (and selection
                     (not (string-empty-p selection))
                     (get-char-property 0 'niv-entry selection))))
    (when entry
      (funcall (or action #'identity) entry))))

(provide 'niv)
