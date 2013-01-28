;;; ntlst-aux.el --- section library
;;; $Id: ntlst-aux.el,v 1.4 2007/01/03 17:00:08 vdplasg Exp $

;; Emacs Lisp Archive Entry
;; Author: Geert A. M. Van der Plas <geert_vanderplas@email.com> 2002 
;; Keywords: section handling
;; Filename: ntlst-aux.el
;; Version: 0.5.0
;; Maintainer: Geert A. M. Van der Plas <geert_vanderplas@email.com>
;; Last-Updated: 26 September 2002
;; Description: library with aux routines for netlist like modes
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

;; byte compile ntlst-aux.el to ntlst-aux.elc (see `byte-compile-file')
;; put these two files in an arbitrary, but accesible directory
;; for example: $HOME/emacs, /usr/lib/emacs/site-lisp/ or 
;; /usr/local/lib/emacs/site-lisp/

;; If you chose a non-standard place to put the files add the following
;; line to your Emacs start-up file (`.emacs') or custom `site-start.el'
;; file (replace <directory-name> by the directory where you placed 
;; ntlst-aux.el and ntlst-aux.elc):
;; (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Howto use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mainly auxiliary functions used in various netlist modes

;; compatibility code:
(defvar ntlst-aux-running-xemacs (string-match "XEmacs" emacs-version)
  "A variable that tells us whether we're in Xemacs or not")

;; xemacs - emacs compatibility wrapper for match-string-no-properties
(defun ntlst-aux-match-string-no-properties (num)
  "wrapper for no properties string matcher"
  (if ntlst-aux-running-xemacs (match-string num)
    (match-string-no-properties num)))

;; ======================================================================
;; msb fix (from cperl-mode.el)
(defvar ntlst-aux-msb-fixed nil) ;; global variable keeping track of addition

(defun ntlst-aux-msb-fix (mode str)
  "Adds entry in msb menu, assumes that msb is already loaded"
  (unless (or (member mode ntlst-aux-msb-fixed)
	      (not (boundp 'msb-menu-cond)))
    (let* ((l (length msb-menu-cond))
	   (last (nth (1- l) msb-menu-cond))
	   (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	   (handle (1- (nth 1 last))))
      (setq ntlst-aux-msb-fixed (append (list mode) ntlst-aux-msb-fixed))
      (setcdr precdr (list
		      (list
		       (list 'eq 'major-mode mode)
                     handle
                     str)
		      last)))))
;; use :
;;(require 'ntlst-aux)
;;(ntlst-aux-msb-fix (quote 'layla-tech-mode) "Layla tech files (%d)")
;;=======================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comments (taken from eldo-mode.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncomment function, should work for every case now:
(defun ntlst-aux-uncomment-region (beg end)
  "Uncomment selected region."
  (interactive "*r")
  (comment-region beg end '(2))) ; 2 is arbitrary, can be any value

;;;
;;; speedbar stuff
;;;
(condition-case ()
    (require 'speedbar)
  (error nil))

(defvar ntlst-aux-speedbar-initialized nil) ;; global variable keeping track of addition

(defun ntlst-aux-speedbar-initialize ()
  "Initialize speedbar."
  ;; general settings
  ;; (set (make-local-variable 'speedbar-tag-hierarchy-method) nil)
  (unless (or (member major-mode ntlst-aux-speedbar-initialized)
	      (not (fboundp 'speedbar-add-supported-extension)))
    (let ((mode-alist auto-mode-alist))
      (setq ntlst-aux-speedbar-initialized 
	    (append (list major-mode) ntlst-aux-speedbar-initialized))
      (while mode-alist
	(when (eq (cdr (car mode-alist)) major-mode)
	  (speedbar-add-supported-extension (car (car mode-alist))))
	(setq mode-alist (cdr mode-alist))))))

(defun ntlst-aux-speedbar-menu-entry ()
  "Menu entry (toggle button) for speedbar open/close"
  (if (featurep 'speedbar)
      (vector "Speedbar" 'speedbar-frame-mode :style 'toggle 
	      :selected '(and (boundp 'speedbar-frame)
			      (frame-live-p speedbar-frame)
			      (frame-visible-p speedbar-frame)))
    (vector "Speedbar not available" 'speedbar-frame-mode nil)))

;;; speedbar end


(provide 'ntlst-aux)

;;; ntlst-aux.el ends here

;;; Local Variables:
;;; mode:Emacs-lisp
;;; End:
