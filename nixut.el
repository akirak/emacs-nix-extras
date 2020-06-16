;;; nixut.el --- A collection of utilities mainly related to Nix -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: processes
;; URL: https://github.com/akirak/nixut.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a collection of utilities for JSON,
;; processes, and Nix.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'json)

;;;; Functions for internal use
(defsubst nixut-remove-prop (prop plist)
  "Destructively delete a pair of PROP from PLIST."
  (if (cl-find prop plist)
      (append (nreverse (cdr (member prop (nreverse (copy-sequence plist)))))
              (cddr (member prop plist)))
    plist))

;;;; Macros
(defmacro nixut-with-json-types (&rest progn)
  "Evaluate PROGN with json types set.

Variables are set as follows:

* `json-object-type' to alist
* `json-array-type' to list

This macro is commonly used to parse JSON in this library."
  `(let ((json-object-type 'alist)
         (json-array-type 'list))
     ,@progn))

;;;; General support for JSON
(defun nixut-json-read-from-string (str)
  "Parse STR as JSON."
  (nixut-with-json-types
   (json-read-from-string str)))

(defun nixut-json-read-buffer ()
  "Read the buffer content as JSON."
  (nixut-with-json-types
   (goto-char (point-min))
   (json-read)))

(defun nixut-json-read-file (file)
  "Read JSON FILE."
  (nixut-with-json-types
   (json-read-file file)))

;;;; General support for processes
(defsubst nixut-format-command-line (cmd &optional args)
  "Format a command line for displaying.

This returns a string which consists of CMD and ARGS, which is a
list of strings.

If an argument contains a space, it is quoted by
`shell-quote-argument'."
  (mapconcat #'shell-quote-argument (cons cmd args) " "))

(cl-defun nixut-sync-process (cmd &rest args
                                  &key destination
                                  &allow-other-keys)
  "Run a command synchronously.

This runs CMD and optionally pipes its output to DESTINATION,
which is a buffer.

You can also pass ARGS to the command.

If the process does not exit with zero, it throws an error."
  (let* ((args (nixut-remove-prop :destination args))
         (result (apply #'call-process cmd nil destination nil args)))
    (unless (= 0 result)
      (error "Abnormal exit %s from \"%s\""
             result
             (nixut-format-command-line cmd args)))))

(defun nixut-read-process (cmd &rest args)
  "Read the output from a command.

This functions runs CMD with ARGS synchronously and returns its
standard output, discarding the standard error.

It throws an error with non-zero exit."
  (with-temp-buffer
    (let ((result (apply #'call-process cmd
                         nil
                         ;; Discard stderr.
                         (list (current-buffer) nil)
                         nil
                         args)))
      (if (= 0 result)
          (buffer-substring-no-properties (point-min) (point-max))
        (error "Abnormal exit %s from \"%s\""
               result
               (nixut-format-command-line cmd args))))))

(defun nixut-read-json-process (cmd &rest args)
  "Read the output from a command and parse it as JSON.

This is a JSON variant of `nixut-read-process', which takes
CMD and ARGS, which see."
  (nixut-json-read-from-string (apply #'nixut-read-process cmd args)))

;;;; Other trivial utilities
(defun nixut-sha1-file (file)
  "Retrieve the sha1 sum of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (sha1 (current-buffer))))

;;;; Nix

(defun nixut-nix-installed-p (&optional required-version)
  "Check if Nix is installed on the system.

If the system type is unsupported by Nix, it throws an error.

If REQUIRED-VERSION is given, it checks the version of Nix.
Unless the version is satisfied, an error is thrown."
  (cl-case system-type
    ((gnu/linux darwin)
     (and (executable-find "nix")
          (or (not required-version)
              (let* ((cmd-output (nixut-read-process "nix" "--version"))
                     (version-str (save-match-data
                                    (and (string-match (rx bol "nix (Nix) "
                                                           (group (+ (any "." digit)))
                                                           eol)
                                                       cmd-output)
                                         (match-string 1 cmd-output)))))
                (cond
                 ((not version-str)
                  (error "Did not match a version output format: %s" version-str))
                 ((version< version-str required-version)
                  (error "Nix version is too low: %s (requires %s)"
                         version-str required-version))
                 (t version-str))))))
    (otherwise
     (user-error "This package does not work on your system, since it depends on Nix, which works only on gnu/linux and darwin now.  Your system: %s" system-type))))

(defun nixut-nix-build-expr (expr)
  "Instantiate EXPR and realise its result."
  (let ((path (nixut-nix-eval expr)))
    (string-trim-right
     (nixut-read-process "nix-store" "-r" path))))

(defun nixut-nix-eval (expr)
  "Evaluate EXPR using \"nix-instantiate\" command."
  (nixut-read-json-process "nix-instantiate"
                           "--eval" "--json" "--expr" expr))

;;;; Niv

(defun nixut-niv-sources (root)
  "Read nix/sources.nix maintained by niv.

You can specify the repository root as ROOT."
  (let ((file (expand-file-name "nix/sources.json" root)))
    (when (file-exists-p file)
      (nixut-json-read-file file))))

(provide 'nixut)
;;; nixut.el ends here

