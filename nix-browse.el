;; (defconst nix-browse-error-buffer "*nix-browse errors*")

(defun nix-browse-git (git-url)
  (interactive "sGit URL: ")
  (nix-browse--check-http-url git-url)
  (nix-browse--expr (format "builtins.fetchGit %s" git-url)
                    'nix-bookmark-git-url-store
                    :git-url git-url))

(defun nix-browse-tarball (url)
  (interactive "sURL: ")
  (nix-browse--check-http-url url)
  (nix-browse--expr (format "builtins.fetchTarball %s" url)))

(defun nix-browse--expr (expr class &rest args)
  "Instantiate an expression and open it."
  (let* ((store-path (nix-browse--instantiate expr))
         (store-obj (apply #'make-instance class :store-path store-path args)))
    (nix-bookmark-save-store store-obj)
    (find-file store-path)))

(defun nix-browse--instantiate (expr)
  "Evaluate EXPR using \"nix-instantiate\" command."
  (nix-browse--read-json-from-string
   (nix-browse--read-process "nix-instantiate"
                             "--eval"
                             "--json"
                             "--expr" expr)))

(defun nix-browse--check-http-url (url)
  "Check if URL is a valid one that returns 200 OK."
  (let* ((url-http-method "HEAD")
         (buffer (url-retrieve-synchronously url 'silent 0 200))
         (status (when buffer
                   (unwind-protect
                       (with-current-buffer buffer
                         (when (url-http-parse-response)
                           ;; TODO: Handle redirect
                           url-http-response-status))
                     (kill-buffer buffer)))))
    (unless (and status (eq status 200))
      (error "URL status %d from %s" status url))))

(defun nix-browse--read-json-from-string (str)
  (let ((json-object-type 'alist)
        (json-object-type 'list))
    (json-read-from-string str)))

(defun nix-browse--read-process (cmd &rest args)
  (with-temp-buffer
    (let ((status (apply #'call-process cmd
                         ;; Discard stderr.
                         nil (list (current-buffer) nil)
                         nil
                         args)))
      (if (eq status 0)
          (buffer-substring-no-properties (point-min) (point-max))
        (error "Non-zero exit from %s"
               (mapconcat #'shell-quote-argument (cons cmd args) " "))))))
