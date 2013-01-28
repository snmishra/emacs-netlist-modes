;;; active-file.el --- active file link library
;;; $Id: active-file.el,v 1.12 2007/01/03 15:59:54 vdplasg Exp $

;; Emacs Lisp Archive Entry
;; Author: Geert A. M. Van der Plas <geert_vanderplas@email.com> 2002 
;; Keywords: link, file, url
;; Filename: active-file.el
;; Version: 0.5.5
;; Maintainer: Geert A. M. Van der Plas <geert_vanderplas@email.com>
;; Last-Updated: 27 September 2002
;; Description: library for inserting active links in various modes
;; URL: http://www.esat.kuleuven.ac.be/~vdplas/emacs/
;; Compatibility: Emacs2[01]

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

;; - add matcher function as in font-lock mode instead of regexps for matching
;; - active-file is a wrong name...
;; - clean up extent - overlay mess. 
;; - Idea: make active file a minor mode comparable to the font-lock minor 
;;         mode use predefined tables with major mode keys and active regexps.
;;         could have global activation of this minor mode.
;;         major modes can add regexps. (font-lock-add-keywords concept)
;;         unknown modes can initialize with their own regexps (font-lock-defaults, font-lock-set-defaults concept) 
;; 
;; this mode resembles tiny-url.el somewhat. Idea is the same, implementation
;; is entirely different.

;; INSTALL:
;; ========

;; byte compile active-file.el to active-file.elc (see `byte-compile-file')
;; put these two files in an arbitrary, but accesible directory
;; for example: $HOME/emacs, /usr/lib/emacs/site-lisp/ or 
;; /usr/local/lib/emacs/site-lisp/

;; If you chose a non-standard place to put the files add the following
;; line to your Emacs start-up file (`.emacs') or custom `site-start.el'
;; file (replace <directory-name> by the directory where you placed 
;; active-file.el and active-file.elc):
;; (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Howto use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example: include file handling in spectre-mode 
;;
;;  (condition-case ()
;;      (require 'active-file)
;;    (error nil))
;;  (when (featurep 'active-file)
;;    (defconst active-file-spectre-alist
;;      (list
;;       (list
;;	;;"^\\s-*\\(?:ahdl_\\)?include\\s-+\"\\([^\"\n]+\\)\""
;;	(concat spectre-library-regexp "\"\\([^\"\n]+\\)\"")
;;	nil
;;	2
;;	nil ;; function
;;	"mouse-2, RET: load include file"
;;	nil ;; highlighting face
;;	t))
;;      "List of active file links in spectre mode"
;;      )
;;    
;;    (defun active-file-spectre-init ()
;;      (active-file-init active-file-spectre-alist))
;;    
;;    (add-hook 'spectre-mode-hook 'active-file-spectre-init)
;;    )
;;
;; Example: subcircuit search in spice-mode
;;
;; helper function to find link:
;; 
;; (defun active-file-spice-subcircuit (filename)
;;   (let ((current (current-buffer)))
;;     (spice-search-subckt (spice-guess-subckt-name))
;;     (message "going back to buffer %s" current)
;;     (pop-to-buffer current t)))
;;
;; define a const containing a list of active links (must not be file links !)
;;
;; (defconst active-file-spice-alist
;;   (list  ;; list of lists !
;;    (list ;; one type of file links
;;     "^\\s-*\\([xX][a-zA-Z0-9:$_]+\\)" ;; regexp matching include file
;;     nil  ;; second regexp, for multiple occurences, if specified, first regexp looks for position (match-anchored from font-lock)
;;     1    ;; match-data number for include file, in first or second regexp
;;     #'active-file-spice-subcircuit   ;; function processing file name
;;     "mouse-2, RET: find subcircuit definition"  ;; info shown
;;     'fringe ;; face used for highlighting when crossing the link
;;     nil)) ;; should file be loaded with active-file-load-included-files ?
;;   "List of active links in spice mode" 
;;   )
;; 
;; intialize active links in buffers through mode hook:
;;
;; (defun active-file-spice-init ()
;;   (active-file-init active-file-spice-alist))
;; 
;; (add-hook 'spice-mode-hook 'active-file-spice-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Active file links in various modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compatibility code:
(defvar active-file-running-xemacs (string-match "XEmacs" emacs-version)
  "A variable that tells us whether we're in Xemacs or not")

;; xemacs - emacs compatibility wrappers
(defun active-file-match-string-no-properties (num)
  "wrapper for no properties string matcher"
  (if active-file-running-xemacs (match-string num)
    (match-string-no-properties num)))

;; overlay - extent compatibility 
(if (fboundp 'make-overlay) ;; is this a good test for overlay - extent choice ?
    (progn
      (defalias 'active-file-make-overlay 'make-overlay)
      (defalias 'active-file-overlays-in 'overlays-in)
      (defalias 'active-file-overlays-at 'overlays-at)
      (defalias 'active-file-overlay-get 'overlay-get)
      (defalias 'active-file-overlay-put 'overlay-put)
      (defalias 'active-file-delete-overlay 'delete-overlay)
      (defalias 'active-file-overlay-start 'overlay-start)
      (defalias 'active-file-overlay-end 'overlay-end)
      (defun active-file-set-overlay-keymap (overlay keymap)
	"fallback version of set-extent-keymap (for emacs 2[01])"
	(overlay-put overlay 'local-map keymap)))
  (defalias 'active-file-make-overlay 'make-extent)
  (defun active-file-overlays-in (beg end) 
    "fallback version of overlays-in (for xemacs21)"
    (extent-list (current-buffer) beg end))
  (defun active-file-overlays-at (pos)
    "fallback version of overlays-at (for xemacs21)"
    (active-file-overlays-in pos pos))
  (defalias 'active-file-overlay-get 'extent-property)
  (defalias 'active-file-overlay-put 'set-extent-property)
  (defalias 'active-file-delete-overlay 'delete-extent)
  (defalias 'active-file-overlay-start 'extent-start-position)
  (defalias 'active-file-overlay-end 'extent-end-position)
  (defalias 'active-file-set-overlay-keymap 'set-extent-keymap))

;; buffer local vars:
(defvar active-file-mode-mouse-map nil
  "Map containing mouse bindings for active-file-mode.")
(make-variable-buffer-local 'active-file-mode-mouse-map)

(defvar active-file-local-alist nil
  "List of active file link types in this buffer.")
(make-variable-buffer-local 'active-file-local-alist)

(defvar active-file-point nil
  "Point where user selected a link in a buffer, buffer local variable.")
(make-variable-buffer-local 'active-file-point)

;; keymap for overlays
(defun active-file-create-keymap ()
  "Creates keymap with active file entries."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    ;; mouse button bindings
    (define-key map "\r" 'active-file-load-file-at-point)
    (if active-file-running-xemacs
	(define-key map 'button2 'active-file-load-file-at-mouse) ;; ffap-at-mouse ?
      (define-key map [mouse-2] 'active-file-load-file-at-mouse))
    (if active-file-running-xemacs
	(define-key map 'Sh-button2 'mouse-yank)
      (define-key map [S-mouse-2] 'mouse-yank-at-click))
    map))


(defun active-file-delete-overlays-in (beg end) 
  "Deletes all active file overlays in current buffer region BEG END,
doesn't modify point"
  (let ((overlays (active-file-overlays-in beg end))
	(start beg)
	(stop end))
    (while overlays
      (when (and
	     (active-file-overlay-get (car overlays) 'detachable)
	     (active-file-overlay-get (car overlays) 'active-file-link))
	(setq start (min start (active-file-overlay-start (car overlays)))
	      stop (max stop (active-file-overlay-end (car overlays))))
	(active-file-delete-overlay (car overlays)))
      (setq overlays (cdr overlays))) ; while
    (list start stop)) ; let
  )


(defun active-file-create-link (beg end function &optional help-echo face)
  "Creates an overlay from BEG to END and attaches properties"
  (when (and beg end)
    (let ((overlay (active-file-make-overlay beg end)))
      (active-file-overlay-put overlay 'start-closed 't)
      (active-file-overlay-put overlay 'end-closed 't)
      (active-file-overlay-put overlay 'detachable 't)
      (active-file-overlay-put overlay 'active-file-link 't)
      (active-file-overlay-put overlay 'active-file-function function)
      (active-file-overlay-put overlay 'mouse-face (if face face 'highlight))
      (active-file-overlay-put overlay 'help-echo help-echo)
      (active-file-set-overlay-keymap overlay active-file-mode-mouse-map))))


(defun active-file-update-file-links (beg end old-len &optional store)
  "This function updates file links in modified region BEG END."
  (save-excursion
    (save-match-data
      (let (end-point (active-files active-file-local-alist))
	(goto-char end)
	(end-of-line)
	(setq end-point (point))
	(goto-char beg)
	(beginning-of-line)  ; scan entire line !
	;; delete overlays existing on the modified lines
	(active-file-delete-overlays-in (point) end-point)
	;; make new ones, could reuse deleted one ?
	(while (car active-files)
	  (while (if (stringp (car (car active-files)))
		     (search-forward-regexp (car (car active-files)) end-point t)
		   (if (functionp (car (car active-files)))
		       (funcall (car (car active-files)) end-point)))
	    (let ((once t)
		  (second (car (cdr (car active-files)))))
	      ;; (setq start-lib (point))
	      (while (if (stringp second) (looking-at second) once)
		;; (message "'%s'" (match-string 0))
		(setq once nil)
		(goto-char (match-end 0))
		(or nil ;; (extent-at (point) (current-buffer) 'mouse-face) ;; not yet extended
		    (active-file-create-link 
		     (match-beginning (nth 2 (car active-files))) 
		     (match-end (nth 2 (car active-files))) 
		     (nth 3 (car active-files)) 
		     (nth 4 (car active-files)) 
		     (nth 5 (car active-files))))
		(when (and store (nth 6 (car active-files)))
		  (active-file-store-file 
		   (active-file-match-string-no-properties (nth 2 (car active-files))) 
		   (nth 3 (car active-files)))))))
	  (goto-char beg)
	  (beginning-of-line)  ; scan entire line !
	  (setq active-files (cdr active-files)))))))


(defun active-file-update-file-links-buffer ()
  (interactive)
  ;; delete overlays
  (active-file-delete-overlays-in (point-min) (point-max))
  ;; update overlays
  (active-file-update-file-links (point-min) (point-max) nil))

(defun active-file-change-major-mode ()
  "function to clean up active file when a major mode changes."
  ;; (message "active file is removing active file links")
  (active-file-delete-overlays-in (point-min) (point-max)))

;; auxiliary function to load file
(defun active-file-derive-file (filename function)
  "expands filename"
  ;; (message "attempting to load %s" filename)
  (let ((expanded-name nil))
    (if (functionp function)
	(setq expanded-name (funcall function filename))
      (setq expanded-name (substitute-in-file-name filename)))
    ;; (message "trying to load '%s'" expanded-name)
    expanded-name
    )
  )

(defun active-file-store-file (filename function)
  "stores expanded file name in list for loading"
  (let (expanded-name)
    (setq expanded-name 
	  (active-file-derive-file filename function))
    (when expanded-name
      (setq expanded-name (expand-file-name expanded-name))
      (when (not (member expanded-name active-file-buffer-list))
	(setq active-file-buffer-list 
	      (append active-file-buffer-list (list expanded-name))))
      ))
  )


(defun active-file-load-file (pos)
  "loads file at pos based on overlay"
  (let ((overlays (if pos (active-file-overlays-at pos) nil)))
    (while overlays
      (when (and
	     (active-file-overlay-get (car overlays) 'detachable)
	     (active-file-overlay-get (car overlays) 'active-file-link))
	(let ((filename (buffer-substring (active-file-overlay-start (car overlays))
					  (active-file-overlay-end (car overlays))))
	      (function (active-file-overlay-get (car overlays) 'active-file-function))
	      name)
	  (setq name (active-file-derive-file filename function))
	  (if (stringp name)
	      (if (file-readable-p name) (find-file name)
		(message "File '%s' is not readable, use Shift Mouse2 to paste" filename)))
	  )
	)
      (setq overlays (cdr overlays))) ; while
    ))
 
;; loads whatever is under mouse click
(defun active-file-load-file-at-mouse (event)
  "loads file under button 2 click. sets point and transfers execution to
load file at point"
  (interactive "@e")
  (let ((pos nil))
    (save-excursion ;; 
      (mouse-set-point event)
      (if (or (looking-at "\\'")
	      (looking-at "^."))
	  (mouse-yank-at-click event nil)
	(setq active-file-point (point)))) ; 
    (active-file-load-file active-file-point)
    ))

;; loads whatever is at point
(defun active-file-load-file-at-point ()
  "loads file at point"
  (interactive)
  (if (looking-at "\\'")
      (newline) ;; assumes \r is bound to load file...
    (setq active-file-point (point))
    (active-file-load-file (point))
    )
  )


(defvar active-file-buffer-list nil
  "list of buffer still to be scanned recursively for include files"
  )

(defun active-file-load-include-files (&optional recursive)
  "function called to load included files"
  (interactive)
  (save-excursion
    (setq active-file-buffer-list nil)
    (active-file-update-file-links (point-min) (point-max) nil t)
    (let ((count 0) (buffer (current-buffer)))
      (while (nth count active-file-buffer-list)
	(if (file-readable-p (nth count active-file-buffer-list))
	    (progn
	      (set-buffer (find-file-noselect (nth count active-file-buffer-list)))
	      (when recursive
		(active-file-update-file-links (point-min) (point-max) nil t)))
	  (message "Couldn't load include file '%s'" (nth count active-file-buffer-list))
	  (sit-for 1)
	  )
;;	(incf count))
	(setq count (1+ count)))
      )
    )
  )

(defun active-file-load-include-files-recursively ()
  "function called to load included files recursively"
  (interactive)
  (active-file-load-include-files t)
  )

(defun active-file-unload-other-files ()
  "function called to unload all other files with current mode"
  (interactive)
  (save-excursion
    (let ((current (current-buffer))
	  (mode major-mode))
      (mapcar
       (lambda (buffer)
	 (set-buffer buffer)
	 (if (and (eq major-mode mode)
		  (not (eq current buffer)))
	     (progn
;;	       (message "Killing %s" buffer)
	       (kill-buffer buffer))))
       (buffer-list)))))

(defun active-file-init (list)
  "Sets up link types in current bufffer. Typically called from a mode hook.
Argument LIST contains all information for identifying and handling links.
See `active-file-latex-alist' for an example for Auctex's LaTeX mode."
  (setq active-file-local-alist list
	active-file-mode-mouse-map (active-file-create-keymap))
  (active-file-update-file-links-buffer)
  (add-hook 'change-major-mode-hook 'active-file-change-major-mode)
  (add-hook 'after-change-functions 'active-file-update-file-links t t))

(defun active-file-search-directories (filename list)
  "Searches LIST of directories for FILENAME"
  (interactive)
  (let (expanded-file)
    (while (car list)
      (setq expanded-file (expand-file-name filename (car list)))
      ;; (message "Looking for file %s" expanded-file)
      (setq list (cdr list))
      (if (file-readable-p expanded-file) 
	  (setq list nil)
	(setq expanded-file nil))
      (message "File %s" expanded-file))
    expanded-file))

(provide 'active-file)

;;; active-file.el ends here

;;; Local Variables:
;;; mode:Emacs-lisp
;;; End:
