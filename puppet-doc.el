;;; puppet-doc.el --- Browse documentation for Puppet

;; Copyright 2011-2015 KURASHIKI Satoru

;; Author: KURASHIKI Satoru <lurdan@gmail.com>
;; Keywords: puppet

;; This file is not part of GNU Emacs, but distributed under the same
;; conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package makes it convenient to read puppet references from
;; within Emacs.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'thingatpt)
(require 'url-cache)

(defvar puppet-doc-root
  "http://docs.puppetlabs.com/references/latest"
  "The root of the puppet reference URL.
If you copy the puppet reference to your local system, set this variable
to something like \"file:/usr/local/doc/puppet/\".")

;; TODO: implement the way to refer local documentation.
(defvar puppet-doc-root-local "/etc/puppet/doc")

(defvar puppet-doc-index-file "~/.emacs.d/var/puppet-doc-index.el")

(defvar puppet-doc-cache-dir "~/.emacs.d/var/puppet")

;; variables for internal use

(defvar puppet-doc-history nil
  "History of symbols looked up.")

(defvar puppet-doc-symbols (make-vector 400 0))

(defvar puppet-doc-index nil)

;; functions for internal use

(defun puppet-doc-init ()
  "Initialize internal settings."
  (with-temp-buffer
    (insert-file-contents puppet-doc-index-file)
    (setq puppet-doc-index (read (buffer-string))))
  (mapcar (lambda (entry)
            (let ((symbol (intern (car entry) puppet-doc-symbols)))
              (if (boundp symbol)
                  (push (cadr entry) (symbol-value symbol))
                (set symbol (cdr entry))))) puppet-doc-index))

(defun puppet-doc-cache (url)
  "Convert given `URL' to its cache."
  (let* ((split-list (split-string url "#"))
         (url (nth 0 split-list))
         (anchor (nth 1 split-list))
         (cache-file (concat (url-cache-create-filename url) ".html")))
    (unless (file-exists-p cache-file)
      (url-cache-prepare cache-file)
      (url-copy-file url cache-file t t))
    (concat "file://" cache-file "#" anchor)
    ))

;;;###autoload
(defun puppet-doc (keyword)
  "View the documentation on `KEYWORD' from the Puppet documentation.
If `KEYWORD' has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.

Puppet documentaion is provided by Puppetlabs. If you copy Puppet
documentation to another location, customize the
variable `puppet-doc-root' to point to that location."
  (interactive (list (let* ((symbol-at-point (thing-at-point 'symbol))
                            (candidate (if symbol-at-point
                                           (downcase symbol-at-point) nil)))
                       (unless puppet-doc-index
                         (puppet-doc-init))
                       (if (and candidate
				(intern-soft candidate
                                             puppet-doc-symbols))
                           candidate
                         (completing-read
                          "Look up KEYWORD of puppet: "
                          puppet-doc-symbols #'boundp
                          t candidate
                          'puppet-doc-history)))))
  (cl-maplist (lambda (entry)
                (browse-url (puppet-doc-cache (concat puppet-doc-root "/" (car entry)))
                            browse-url-new-window-flag)
                (if (cdr entry)
                    (sleep-for 1.5)))
              (let ((symbol (intern-soft (downcase keyword)
                                         puppet-doc-symbols)))
                (if (and symbol (boundp symbol))
                    (symbol-value symbol)
                  (error "The KEYWORD  `%s' is not defined in puppet reference"
                         keyword)))))

(provide 'puppet-doc)

;;; puppet-doc.el ends here
