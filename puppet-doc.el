;;; puppet-doc.el --- Browse documentation for Puppet

;; Copyright 2011 KURASHIKI Satoru

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

(require 'cl)
(require 'browse-url)
(require 'thingatpt)

(defvar puppet-doc-root
    "http://docs.puppetlabs.com/references/latest/"
  "The root of the puppet reference URL.
If you copy the puppet reference to your local system, set this variable
to something like \"file:/usr/local/doc/puppet/\".")

(defvar puppet-doc-root-local "/etc/puppet/doc")

(defvar puppet-doc-history nil
  "History of symbols looked up.")

;;if only we had had packages or hash tables..., but let's fake it.

(defvar puppet-doc-symbols (make-vector 67 0))

(defvar puppet-doc-index-file "~/.emacs.d/tmp/puppet-doc-index.el")

(defun puppet-doc (symbol-name)
  "View the documentation on KEYWORD from the Puppet documentation.
If KEYWORD has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.

Puppet documentaion is provided by Puppetlabs. If you copy Puppet
documentation to another location, customize the
variable `puppet-doc-root' to point to that location."
  (interactive (list (let ((symbol-at-point (thing-at-point 'symbol)))
		       (if (and symbol-at-point
				(intern-soft (downcase symbol-at-point)
					     puppet-doc-symbols))
                           symbol-at-point
			 (completing-read
			  "Look up KEYWORD of puppet: "
			  puppet-doc-symbols #'boundp
			  t symbol-at-point
			  'puppet-doc-history)))))
  (maplist (lambda (entry)
             (browse-url (concat puppet-doc-root "/" (car entry))
                         browse-url-new-window-flag)
             (if (cdr entry)
                 (sleep-for 1.5)))
           (let ((symbol (intern-soft (downcase symbol-name)
                                      puppet-doc-symbols)))
             (if (and symbol (boundp symbol))
                 (symbol-value symbol)
               (error "The KEYWORD `%s' is not defined in puppet reference."
                      symbol-name)))))

(with-temp-buffer
 (insert-file-contents puppet-doc-index-file)
 (setq puppet-doc-index (read (buffer-string))))

(mapcar (lambda (entry)
	  (let ((symbol (intern (car entry) puppet-doc-symbols)))
	    (if (boundp symbol)
	      (push (cadr entry) (symbol-value symbol))
	      (set symbol (cdr entry))))) puppet-doc-index)

(provide 'puppet-doc)

;;; puppet-doc.el ends here

