;;; nemo.el --- Nix-based package manager -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (dash "2.10") (epkg "0"))
;; Keywords: tools lisp maint
;; URL: https://github.com/akirak/emacs-nix-extras

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

;; FIXME

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'epkg)

(defun nemo-parse-use-package-decls (file)
  "Get a list of packages in `use-package' forms in FILE."
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let (packages)
          (while (re-search-forward (rx "(use-package" space) nil t)
            (beginning-of-defun-raw)
            (let* ((exp (read (current-buffer)))
                   (name (nth 1 exp))
                   (disabled (-some->> (member :disabled exp)
                               (nth 1)))
                   (if-expr (-some->> (member :if exp)
                              (nth 1))))
              (when (and (not disabled)
                         (not (and if-expr
                                   (not (eval if-expr)))))
                (push name packages)))
            (end-of-defun))
          packages)))))

(defun nemo-map-installable-packages (packages)
  "Get a list of packages to be installed to activate PACKAGES."
  (cl-labels
      ((query (pkg)
              (let* ((name (symbol-name pkg))
                     (obj (epkg name)))
                (cond
                 ((and obj (epkg-builtin-package-p obj))
                  nil)
                 (obj
                  name)
                 (t
                  (epkg-provided-by pkg))))))
    (->> packages
         (-map #'query)
         (delq nil)
         (-uniq))))

(cl-defun nemo-write-packages-from-use-package (srcfile outfile
                                                        &key
                                                        (open "")
                                                        (close "")
                                                        (separator "\n"))
  "Write a list of installed packages from an init file.

SRCFILE should be an Emacs Lisp file containing `use-package' forms.
This function reads a list of package names contained in the file and
transforms it into a list of installed packages needed for those
packages, and saves the result to OUTFILE.

To customize the output format, you can set an OPEN bracket, a CLOSE
bracket, and a SEPARATOR for the generated list."
  (let ((packages (->> (nemo-parse-use-package-decls srcfile)
                       (nemo-map-installable-packages))))
    (with-temp-buffer
      (insert (concat open (string-join packages separator) close))
      (write-file outfile))))

(provide 'nemo)
;;; nemo.el ends here
