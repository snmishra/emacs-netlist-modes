;;; ntlst-section.el --- section library
;;; $Id: ntlst-section.el,v 1.3 2007/01/03 17:00:36 vdplasg Exp $

;; Emacs Lisp Archive Entry
;; Author: Geert A. M. Van der Plas <geert_vanderplas@email.com> 2002 
;; Keywords: section handling
;; Filename: ntlst-section.el
;; Version: 0.5.0
;; Maintainer: Geert A. M. Van der Plas <geert_vanderplas@email.com>
;; Last-Updated: 26 September 2002
;; Description: library for handling sections in files
;; URL: http://www.esat.kuleuven.ac.be/~vdplas/emacs/
;; Compatibility: Emacs21

;; Please send suggestions and bug reports to
;; mailto:Geert_VanderPlas@email.com

;; Copyright (C) 2002 Geert A. M. Van der Plas
;; Copyright (C) 2002 Emmanuel Rouat

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; TODO:

;; INSTALL:
;; ========

;; byte compile ntlst-section.el to ntlst-section.elc (see `byte-compile-file')
;; put these two files in an arbitrary, but accesible directory
;; for example: $HOME/emacs, /usr/lib/emacs/site-lisp/ or 
;; /usr/local/lib/emacs/site-lisp/

;; If you chose a non-standard place to put the files add the following
;; line to your Emacs start-up file (`.emacs') or custom `site-start.el'
;; file (replace <directory-name> by the directory where you placed 
;; ntlst-section.el and ntlst-section.elc):
;; (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Howto use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auxiliary functions and compatibility:
(require 'ntlst-aux)

(defvar ntlst-section-defaults nil
  "Defaults for all ntlst-section settings:

Defaults should be of the form:

 (SECTION-ALIST SECTION-START-REGEXP DOC-STRING FIND-CHANGELOG INSERT-COMMENT-BAR ...)

These item elements are used by ntlst-section to set the variables
`ntlst-section-alist', `ntlst-section-regexp-start',
`ntlst-section-doc-char', `ntlst-section-find-changelog-point' and
`ntlst-section-insert-comment-bar', respectively.

Use `ntlst-section-set-defaults' to initialize the section code, you can
then use local variable `ntlst-section-headings-regexp' to search for
section headings, and function `ntlst-section-menu-alist' to get two submenu's
(goto and add section).

The header Changelog is added automatically. It can then be used with
`ntlst-section-add-changelog-entry' to add a Changelog entry in-file. 
Changelog section is inserted if it does not exist.  A good position 
is sought with `ntlst-section-find-changelog-point' if the Changelog 
section can not be found.

")
;;;###autoload
(make-variable-buffer-local 'ntlst-section-defaults)


(defvar ntlst-section-alist nil
  "local var holding section list")
(make-variable-buffer-local 'ntlst-section-alist)

(defvar ntlst-section-regexp-start nil
  "local var holding section start regexp. Make sure this regexp only
contains shy groups (?:)!")
(make-variable-buffer-local 'ntlst-section-regexp-start)

(defvar ntlst-section-doc-char nil
  "local var holding section start regexp")
(make-variable-buffer-local 'ntlst-section-doc-char)

(defvar ntlst-section-find-changelog-point nil
  "local var keeping function for finding changelog point")
(make-variable-buffer-local 'ntlst-section-find-changelog-point)

(defvar ntlst-section-insert-comment-bar nil
  "local var keeping function for finding changelog point")
(make-variable-buffer-local 'ntlst-section-insert-comment-bar)

(defvar ntlst-section-headings nil
  "List of section headings.")
(make-variable-buffer-local 'ntlst-section-headings)

(defvar ntlst-section-headings-regexp nil
  "Regexp for section headings.")
(make-variable-buffer-local 'ntlst-section-headings-regexp)


(defun ntlst-section-eval-alist (alist)
  "Evalulate ALIST if a function (funcall) or variable (eval) name."
  (if (listp alist)
      alist
    (ntlst-section-eval-alist (if (fboundp alist)
				 (funcall alist)
			       (eval alist)))))

;;------------------------------------------------------------
;; Changelog and sections support (taken from eldo- & spice-mode, trying
;;  to be compatible :)
;;------------------------------------------------------------

(defun ntlst-section-add-changelog-entry (changelog-entry)
  "Find Changelog section (create it if not found) and add an entry for today."
  (interactive "sChangelog entry: ")
  (let (pos)
    (save-excursion 
      (goto-char (point-min))
      (setq pos (re-search-forward 
		 (concat ntlst-section-regexp-start "Changelog") nil t)))
    (if pos (goto-char pos)
      (ntlst-section-add-section "Changelog" 
				 (if (fboundp ntlst-section-find-changelog-point)
				     (funcall ntlst-section-find-changelog-point)))))
  (ntlst-section-goto-section "Changelog")
  ;; insert changelog now:
  (let ((string (concat ntlst-section-doc-char " "
			(substring (current-time-string) 0 11)
			(substring (current-time-string) -4) " "
			(user-full-name) " <" user-mail-address ">")))
    (if (not (search-forward string nil t))
	(insert "\n" string "\n\n")
      (forward-line 2))
    (insert ntlst-section-doc-char "    - " changelog-entry "\n")))


(defun ntlst-section-goto-section (section)
  "Move point to the beginning of the specified section; If the
section is not found, leave point at previous location."
  (interactive "ssection: ")
  (let ((pos (point)))
    (goto-char (point-min))
    (if (not (re-search-forward 
	      (concat ntlst-section-regexp-start section "\\b") nil t))
	(progn (message "Couldn't find section %s" section)
	       (goto-char pos)) 
      (progn
	(forward-line 2)
	(recenter))))) ;; added recenter


(defun ntlst-section-comment-bar (&optional aligned)
  "Insert solid comment bar from column zero to end of line. If optional
argument is provided, bar will be added from current column."
  (interactive)
  (if (not aligned) (beginning-of-line))
  (insert ntlst-section-doc-char)
  (insert-char ?- (- (1- fill-column) (current-column)))
  (insert "\n"))


(defun ntlst-section-add-section (section &optional pos)
  "Add a section in buffer at (optional) point pos"
  (interactive "ssection: ")
  (if pos (goto-char pos))
  (when ntlst-section-insert-comment-bar
    (if (fboundp ntlst-section-insert-comment-bar)
	(funcall ntlst-section-insert-comment-bar)
      (ntlst-section-comment-bar)))
  (insert (concat ntlst-section-doc-char "\t" section " \n"))
  (when ntlst-section-insert-comment-bar
    (if (fboundp ntlst-section-insert-comment-bar)
	(funcall ntlst-section-insert-comment-bar)
      (ntlst-section-comment-bar)))
  )



(defun ntlst-section-set-defaults ()
  "Initialize ntlst sections local vars based on `ntlst-section-defaults'"
  ;; set buffer local vars...
  ;; ...
  (setq ntlst-section-alist (ntlst-section-eval-alist 
			     (nth 0 ntlst-section-defaults))
	ntlst-section-regexp-start (nth 1 ntlst-section-defaults)
	ntlst-section-doc-char (nth 2 ntlst-section-defaults)
	ntlst-section-find-changelog-point (nth 3 ntlst-section-defaults)
	ntlst-section-insert-comment-bar (nth 4 ntlst-section-defaults)
	)
  (setq ntlst-section-headings (list "Changelog")) ; Changelog is special case
  (let ((section-alist ntlst-section-alist) heading)
	(while section-alist
	  (setq heading (downcase (car (cdr (car section-alist)))))
	  (setq ntlst-section-headings (append ntlst-section-headings
					       (list heading)))
	  (setq section-alist (cdr section-alist))))
  (setq ntlst-section-headings-regexp
	(concat ntlst-section-regexp-start "\\("
		(regexp-opt ntlst-section-headings) "\\)\\(.*\\)$"))
  )

(defun ntlst-section-unset-defaults ()
  "Clean up ntlst sections local vars"
  (setq ntlst-section-alist nil
	ntlst-section-regexp-start nil
	ntlst-section-doc-char nil
	ntlst-section-find-changelog-point nil
	ntlst-section-insert-comment-bar nil
	ntlst-section-headings nil
	ntlst-section-headings-regexp nil))

;; caching sections in local var, for rapid menu build-up

(defvar ntlst-section-cache-section-alist nil)
(make-variable-buffer-local 'ntlst-section-cache-section-alist)

(defun ntlst-section-cache-section-p (section)
  "checks for all sections in file and remembers if they were present or not"
  (save-excursion
    ;; (message "updating cache...")
    (setq ntlst-section-cache-section-alist nil)
    (goto-char (point-min))
    (while (re-search-forward ntlst-section-headings-regexp nil t)
      (setq ntlst-section-cache-section-alist 
	    (cons (cons (downcase (ntlst-aux-match-string-no-properties 1)) t)
		  ntlst-section-cache-section-alist)))
    (ntlst-section-p section)))

(defun ntlst-section-p (section)
  "checks if named section is in file, returns t if found, nil otherwise, 
uses cache generated with the `ntlst-cache-section-p' function."
  (assoc section ntlst-section-cache-section-alist))

(defun ntlst-section-menu-alist ()
  "return a list with 2 submenu's: Goto Section and Add Section Header"
  (list
   (append 
    '("Goto Section")
    (let ((section-alist ntlst-section-alist) menu-alist name str)
      (setq menu-alist 
	    (cons "--"
		  (cons '["Changelog"
			  (ntlst-section-goto-section "changelog") 
					; (setq menu-sec (current-time))
			  (ntlst-section-cache-section-p "changelog")
			  ] menu-alist)))
      (while section-alist
	(setq name (car (car section-alist)))
	(setq str (downcase (car (cdr (car section-alist)))))
	(setq menu-alist (cons (vector name 
				       (list 'ntlst-section-goto-section str)
				       (list 'ntlst-section-p str)
				       )
			       menu-alist))
	(setq section-alist (cdr section-alist)))
     (setq menu-alist 
	   (cons '["Specify..."
		   ntlst-section-goto-section t]
		 (cons "--" menu-alist)))
     (nreverse menu-alist))
    )
   (append 
    '("Add Section Header")
    (let ((section-alist ntlst-section-alist) menu-alist name str)
      (setq menu-alist 
	    (cons "--"
		  (cons '["Changelog"
			  (ntlst-section-add-section "Changelog") 
			  (not (ntlst-section-p "changelog"))] menu-alist)))
      (while section-alist
	(setq name (car (car section-alist)))
	(setq str (car (cdr (car section-alist))))
	(setq menu-alist (cons (vector name 
				       (list 'ntlst-section-add-section str)
				       (list 'not (list 'ntlst-section-p (downcase str)))
				       )
			      menu-alist))
	(setq section-alist (cdr section-alist)))
      (setq menu-alist 
	    (cons '["Specify..."
		    ntlst-section-add-section t]
		  (cons "--" menu-alist)))
      (nreverse menu-alist))
    )
   "--"
   )
  )


(provide 'ntlst-section)

;;; ntlst-section.el ends here

;;; Local Variables:
;;; mode:Emacs-lisp
;;; End:
