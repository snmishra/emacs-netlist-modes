;; aplac-mode.el --- major mode for Aplac netlist files
;;
;; Author:  Brennan Sharp <brennanzl@xtra.co.nz>
;; Version: 0.30a
;; URL: not yet
;; Keywords: aplac, syntax, simulator, netlist
;; Compatibility: emacs21. Not tested but ought to work on other *emacs*
;;
;; Copyright © 2002 Brennan Sharp <brennan.sharp@xtra.co.nz>
;;
;; APLAC is a registered trademark of APLAC Solutions Corporation, Finland.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMENTARY:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This major mode for emacs provides support for editing APLAC   
;; netlist files. It might work on other emacs. I'm testing on emacs21.
;; While not complete by any stretch of thu imagination, I've been using this
;; regularly at work without any real problems.
;;
;; You may wish to add something like the following to your ~/.emacs file:
;;
;; (setq load-path (cons (expand-file-name "~/.emacs-modes") load-path))
;; (autoload 'aplac-mode "aplac-mode" "Major mode for editing APLAC input" t)
;; (setq auto-mode-alist (append (list (cons "\\.i$"    'aplac-mode)
;;				    (cons "\\.lib"   'aplac-mode)
;;				    auto-mode-alist))
;;
;; Put this file in the emacs load-path so emacs can find it (check the
;; manual). (I subdivide my ~/.emacs-modes directory for each mode because many
;; modes come with other files (e.g. README) I put symbolic links in the 
;; ~/.emacs-modes directory to each mode file.
;;
;; This mode will activate automatically on files which end by .i or .lib
;; This mode requires font-lock, easymenu, and optionally func-menu or imenu..
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREDIT:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This code is based entirely on eldo-mode v1.0
;; (Emmanuel Rouat and Geert A. M. Van der Plas)
;; ( see http://www.esat.kuleuven.ac.be/~vdplas/emacs/ )
;;
;; Thanks to them for putting together something I can tinker with.
;;
;; To claim I understand how half of this code works yet would be a lie.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; - maybe re-instate the component menu appropriate to aplac
;; - tie #libdir and #library together for opening a file.
;; - add aplac variables to the models menu
;; - remove leading double quote form "inclusions" model menu item
;; - add the mouse click to open file trick to filenames follow tho LOAD
;;   parameter.
;; - add key bindings for aplac help - user needs to specify pdf viewer
;; - there are many features commented out from the original eldo-mode code.
;; - As time permits, I'll look at their relevance to aplac-mode and maybe
;;   they'll live again.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KNOWN BUGS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; - that neat file-open by mouse button click doesn't always work. I must have
;;   damaged it.
;; - This comment:
;;   $ Inductors in directory "toko-fh" capacitors in directory "Murata-grm39".
;;   is not handled correctly -- the comment stops being a comment after the
;;   first string. However, that comment ought to be a doc-comment and use a *
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;------------------------------------------------------------
;; Misc variables/constants
;;------------------------------------------------------------


(defconst aplac-mode-version "0.30a May 2002"
  "Current version of aplac mode.")

(defconst aplac-mode-developer   
  "Brennan Sharp <brennanzl@xtra.co.nz>"
  "Current developer/maintainer of aplac-mode.")

(defvar aplac-mode-hook nil
  "Function or functions to run on entry to aplac-mode.")

(defvar aplac-default-header nil
  "Default header for Aplac netlists")

(defvar aplac-end-comment-column (1- fill-column)
  "*Last column in Aplac  comment (before line wraps)")

(defvar aplac-running-xemacs (string-match "XEmacs" emacs-version)
  "A variable that tells us whether we're in Xemacs or not" )


;;------------------------------------------------------------
;; Customization support
;;------------------------------------------------------------


(defgroup aplac-mode nil
  "Customizations for Aplac Mode."
  :prefix "aplac-mode-"
  :group 'languages)

(defgroup aplac-faces nil
  "Customizations for highlighting."
  :group 'aplac-mode)

(defcustom aplac-use-imenu t
  "*Non-nil means use imenu to create an index menu in the menubar.
This index will list subcircuits, bipolar, mosfet and diode models."
  :type 'boolean
  :group 'aplac-mode)

(defcustom aplac-use-func-menu nil
  "*Non-nil means use func-menu to create an index menu in the menubar.
This index will list subcircuits, bipolar, mosfet and diode models.
Note that func-menu is usually not available in Emacs."
  :type 'boolean
  :group 'aplac-mode)

(defcustom aplac-simulator-switches " -ntw"
  "Aplac command switches, used when simulating buffer with `compile-mode'"
  :type  'string
  :group 'aplac-mode)

(defgroup aplac-section nil
  "Customizations for sections."
  :group 'aplac-mode)

;; sections (entirely different implementation but   
;; sections idea has been taken from aplac-mode.el)
(defcustom aplac-section-alist
  '(
    ;; Libraries
    ("Libraries"     "LIBRARIES"              nil) ;   
    ;; Options	       
    ("Options"       "SIMULATION OPTIONS"     nil) ;
    ;; Supplies	       
    ("Supplies"      "SUPPLIES/REFERENCES"    nil) ;
    ;; Input Signals
    ("Input Signals" "INPUT SIGNALS"          nil) ;
    ;; DC Analysis
    ("DC Analysis"   "DC ANALYSIS"            nil) ;
    ;; AC Analysis
    ("AC Analysis"   "AC ANALYSIS"            nil) ;
    ;; Transient Analysis
    ("Transient Analysis" "TRANSIENT ANALYSIS" nil) ;
    ;;; Add your local spice sections here:
    ;;   
    )
  "*List of valid sections in an Aplac file and their options.
Each list entry specifies the following items for a section:
Section:
  Section Name     : name used in to select/create find section, make this   
                     name short and descriptive.
  Section String   : string used in file to start section (usually all   
                     uppercase variant of name.   
  Extra switches   : extra switches for a section, unspecified for now."
  :type '(repeat (list :tag Section :indent 2
		       (string :tag "Section Name        ")
		       (string :tag "Section String      ")
		       (string :tag "Extra Switches (nil)")))
  :group 'aplac-section)


;;------------------------------------------------------------
;; Required packages
;;------------------------------------------------------------

(require 'font-lock)
(require 'compile)
(require 'tempo)
(require 'imenu)
(require 'speedbar)
(require 'easymenu)
(require 'skeleton)


;;------------------------------------------------------------
;; Key bindings   
;;------------------------------------------------------------


(defvar aplac-mode-map ()
  "Keymap containing aplac commands.")

(if aplac-mode-map   
    ()
  (setq aplac-mode-map (make-sparse-keymap))

  ;; key bindings (start with Control-C)   
  ;; inspired by vhdl-mode for some compatibility

  (define-key aplac-mode-map "\C-c\C-s" 'compile)
  (define-key aplac-mode-map "\C-c\C-k" 'kill-compilation)
  (define-key aplac-mode-map "\C-c\C-b" 'aplac-reformat-netlist)
  (define-key aplac-mode-map "\C-c\C-f" 'font-lock-fontify-buffer)
  (define-key aplac-mode-map "\C-cc"    'comment-region)
  (define-key aplac-mode-map "\C-cu"    'aplac-uncomment-region)
  (define-key aplac-mode-map "\C-c\C-c" 'aplac-comment-uncomment-region)

  ;; mouse bindings
  (define-key aplac-mode-map [(shift mouse-2)] 'ffap-at-mouse)

  )



;;------------------------------------------------------------
;; Aplac-mode menu (using `easy-menu.el')
;;------------------------------------------------------------

(defun aplac-create-mode-menu ()
  "Create APLAC Mode menu."
  (list
   "Aplac"
    "------"
    ["Comment Region"		comment-region (mark)]
    ["Uncomment Region"		aplac-uncomment-region (mark)]
;    ["(Un)comment Region"	aplac-comment-uncomment-region (mark)]
    ["Fontify..."		font-lock-fontify-buffer t]
;    ["Abbrevs"			abbrev-mode :style toggle :selected abbrev-mode]
    ["Add Changelog Entry"	aplac-add-changelog-entry t]
    "------"
;    '("Voltage Sources"
;     ["Voltage controlled"	tempo-template-aplac-vcvs t]
;     ["Current controlled"	tempo-template-aplac-ccvs t]
;     )
;    '("Current Sources"
;     ["Voltage controlled"	tempo-template-aplac-vccs t]
;     ["Current controlled"	tempo-template-aplac-cccs t]
;     )
;    '("Waveforms"
;     ["PWL"			(aplac-pwl) t]
;     ["pulse"			tempo-template-aplac-pulse t]
;     ["ac"			tempo-template-aplac-ac t]
;     ["sin"			tempo-template-aplac-sine t]
;     ["sffm"			tempo-template-aplac-sffm t]
;     ["exp"			tempo-template-aplac-exp t]
;     ["noise"			tempo-template-aplac-noise t]
;     ["pattern"			tempo-template-aplac-pattern t]
;     )
;    '("Macromodels"
;     ["SO Comparator"		tempo-template-aplac-comp t]
;     ["DO Comparator"		tempo-template-aplac-compd t]
;     ["SO Linear Opamp"		tempo-template-aplac-linear-opa0 t]
;     ["DO Linear Opamp"		tempo-template-aplac-linear-opa0d t]
;     ["SO Linear 1-pole Opamp"	tempo-template-aplac-linear-opa1 t]
;     ["DO Linear 1-pole Opamp"	tempo-template-aplac-linear-opa1d t]
;     ["SO Linear 2-pole Opamp"	tempo-template-aplac-linear-opa2 t]
;     ["DO Linear 2-pole Opamp"	tempo-template-aplac-linear-opa2d t]
;     ["Delay"			tempo-template-aplac-delay t]
;     ["Saturating Resistor"	tempo-template-aplac-satr t]
;     ["Voltage Limiter"		tempo-template-aplac-satv t]
;     ["Voltage cont. switch"	tempo-template-aplac-vswitch t]
;     ["Current cont. switch"	tempo-template-aplac-cswitch t]
;     ["Sample&Hold"		tempo-template-aplac-saho t]
;     ["Track&Hold"		tempo-template-aplac-trho t]
;     ["Peak Detector"		tempo-template-aplac-peakd t]
;     ["SO Level Detector"	tempo-template-aplac-levdso t]
;     ["DO Level Detector"	tempo-template-aplac-levddo t]
;     )
;    '("Extracts"
;     ["Phase margin"		tempo-template-aplac-phmag t]
;     ["PM min"			tempo-template-aplac-pmmin t]
;     ["Gain margin"		tempo-template-aplac-gmag t]
;     ["Cut freq."		tempo-template-aplac-fc t]
;     ["Unity gain freq."	tempo-template-aplac-ugfc t]
;     ["Period of signal"	tempo-template-aplac-period t]
;     )      
;    '("Macros"
;     ["Period of signal"	tempo-template-aplac-period-macro t]
;     ["Duty cycle of signal"	tempo-template-aplac-duty-macro t]
;     )      
    '("Misc"
;     ["defmodel"		tempo-template-aplac-defmodel t]
     ["header"			tempo-template-aplac-circuit-header t]
     ["comment bar"		(aplac-comment-bar 't) t ]
     )
    "--"
   (append   
    '("Goto Section")
    (let ((section-alist aplac-section-alist) menu-alist name str)
      (while section-alist
	(setq name (car (car section-alist)))
	(setq str (car (cdr (car section-alist))))
	(setq menu-alist (cons (vector name   
				       (list 'aplac-goto-section str)
				       (list 'aplac-section-p str)
				       )
			       menu-alist))
	(setq section-alist (cdr section-alist)))
      (setq menu-alist   
	    (cons '["Changelog"
		    (aplac-goto-section "Changelog")   
		    (aplac-section-p "Changelog")]
		  (cons "--" menu-alist)))
      (setq menu-alist   
	    (cons '["Specify..."
		    aplac-goto-section t]
		  (cons "--" menu-alist)))
      (nreverse menu-alist))
    )
   (append   
    '("Add Section Header")
    (let ((section-alist aplac-section-alist) menu-alist name str)
      (while section-alist
	(setq name (car (car section-alist)))
	(setq str (car (cdr (car section-alist))))
	(setq menu-alist (cons (vector name   
				       (list 'aplac-add-section str)
				       (list 'not (list 'aplac-section-p str))
				       )
			       menu-alist))
	(setq section-alist (cdr section-alist)))
      (setq menu-alist   
	    (cons '["Changelog"
		    (aplac-add-section "Changelog")   
		    (not (aplac-section-p "Changelog"))]
		  (cons "--" menu-alist)))
      (setq menu-alist   
	    (cons '["Specify..."
		    aplac-add-section t]
		  (cons "--" menu-alist)))
      (nreverse menu-alist))
    )
    "------"
    ["Run Aplac"			compile  t]
    ["Quit simulation"		kill-compilation  t]
    ["Customize..."		aplac-customize t]
    ["Open/Close Speedbar"      aplac-speedbar t]
    ["Reformat netlist"		aplac-reformat-netlist t]
    "------"
    ["About Aplac-Mode"		aplac-about t])
  )

(defun aplac-update-mode-menu ()
  "Update Spice mode menu."
  (interactive)
  (easy-menu-remove aplac-menu-list) ; for XEmacs
  (setq aplac-mode-menu-list (aplac-create-mode-menu))
  (easy-menu-add aplac-menu-list)	; for XEmacs
  (easy-menu-define aplac-menu aplac-mode-map
		    "Menu keymap for Aplac Mode." aplac-menu-list))


(defvar aplac-menu-list nil
  "Aplac Mode menu.")

;;------------------------------------------------------------
;; Aplac mode syntax table
;;------------------------------------------------------------


(defvar aplac-mode-syntax-table nil
  "The syntax table used in `aplac-mode' buffers")

(if aplac-mode-syntax-table
    ()
  (setq aplac-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n "> b" aplac-mode-syntax-table)
  (modify-syntax-entry ?c "w 2"  aplac-mode-syntax-table)
  (modify-syntax-entry ?e "w 4"  aplac-mode-syntax-table)
  (modify-syntax-entry ?/ "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?' "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?. "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?, "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?- "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?@ "w"    aplac-mode-syntax-table)
  (modify-syntax-entry ?[ "("    aplac-mode-syntax-table)
  (modify-syntax-entry ?] ")"    aplac-mode-syntax-table)
  (modify-syntax-entry ?= "."    aplac-mode-syntax-table)
  (set-syntax-table aplac-mode-syntax-table))

;; note: in aplac syntax, nearly all caracters are valid words! For instance
;; a model can be called BIP_TYPICAL#1.2[foo]
;; this is why nearly all caracters here have word-class



;;------------------------------------------------------------
;; Abbrev hook bindings
;;------------------------------------------------------------


(defvar aplac-mode-abbrev-table nil
  "Abbrev table to use in `aplac-mode' buffers.")
(if aplac-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'aplac-mode-abbrev-table ())
    (define-abbrev aplac-mode-abbrev-table "'pu"    "" 'tempo-template-aplac-pulse)
    (define-abbrev aplac-mode-abbrev-table "'su"    "" 'tempo-template-aplac-defmodel)
    (define-abbrev aplac-mode-abbrev-table "'ac"    "" 'tempo-template-aplac-ac)
    (define-abbrev aplac-mode-abbrev-table "'sin"   "" 'tempo-template-aplac-sine)
    (define-abbrev aplac-mode-abbrev-table "'sffm"  "" 'tempo-template-aplac-sffm)
    (define-abbrev aplac-mode-abbrev-table "'exp"   "" 'tempo-template-aplac-exp)
    (define-abbrev aplac-mode-abbrev-table "'noi"   "" 'tempo-template-aplac-noise)
    (define-abbrev aplac-mode-abbrev-table "'pat"   "" 'tempo-template-aplac-pattern)
    (define-abbrev aplac-mode-abbrev-table "'comp"  "" 'tempo-template-aplac-comp)
    (define-abbrev aplac-mode-abbrev-table "'compd" "" 'tempo-template-aplac-compd)
    (define-abbrev aplac-mode-abbrev-table "'opa"   "" 'tempo-template-aplac-linear-opa)
    (define-abbrev aplac-mode-abbrev-table "'opad"  "" 'tempo-template-aplac-linear-opad)
    (define-abbrev aplac-mode-abbrev-table "'pmag"  "" 'tempo-template-aplac-phmag)
    (define-abbrev aplac-mode-abbrev-table "'gmag"  "" 'tempo-template-aplac-gmag)
    (define-abbrev aplac-mode-abbrev-table "'fc"    "" 'tempo-template-aplac-fc)
    (define-abbrev aplac-mode-abbrev-table "'ugf"   "" 'tempo-template-aplac-ugfc)
    (define-abbrev aplac-mode-abbrev-table "'tf"    "" 'tempo-template-aplac-period)      
    (define-abbrev aplac-mode-abbrev-table "'tfm"   "" 'tempo-template-aplac-period-macro)
    (define-abbrev aplac-mode-abbrev-table "'dc"    "" 'tempo-template-aplac-duty-macro)      
    (define-abbrev aplac-mode-abbrev-table "'he"    "" 'tempo-template-aplac-circuit-header)
    (setq abbrevs-changed ac)))


;;------------------------------------------------------------
;; font-lock stuff
;;------------------------------------------------------------


;; We try to use usual font-lock faces, plus a few specific ones:


(custom-add-to-group
 'aplac-faces 'font-lock-comment-face 'custom-face)
(custom-add-to-group
 'aplac-faces 'font-lock-keyword-face 'custom-face)
(custom-add-to-group
 'aplac-faces 'font-lock-type-face 'custom-face)
(custom-add-to-group
 'aplac-faces 'font-lock-function-name-face 'custom-face)
(custom-add-to-group
 'aplac-faces 'font-lock-variable-name-face 'custom-face)
(custom-add-to-group
 'aplac-faces 'font-lock-warning-face 'custom-face)



;(defvar aplac-doc-face	'aplac-doc-face
;  "Face name to highlight aplac netlist documentation.")

;(defface aplac-doc-face
;  '((((class color) (background light)) (:foreground "blue1"))
;    (((class color) (background dark)) (:foreground "blue1"))
;    (t (:inverse-video t)))
;  "Face name to highlight aplac netlist documentation."
;  :group 'aplac-faces)


(defvar aplac-analysis-face 'aplac-analysis-face
  "Face name to highlight aplac analysis commands.")

(defface aplac-analysis-face
  '((((class color) (background light)) (:foreground "Red2" :bold t))
    (((class color) (background dark)) (:foreground "Lightgreen" :bold t))
    (t (:bold t)))
  "Aplac mode face used to highlight analysis commands."
  :group 'aplac-faces)


(defvar aplac-output-face 'aplac-output-face
  "Face name to highlight aplac output commands.")

(defface aplac-output-face
  '((((class color) (background light)) (:foreground "purple2"))
    (((class color) (background dark)) (:foreground "magenta2")))
  "Aplac mode face used to highlight output commands."
  :group 'aplac-faces)


(defvar aplac-instance-name-face 'aplac-instance-name-face
  "Face name to highlight aplac instance names.")

(defface aplac-instance-name-face
  '((((class color) (background light)) (:foreground "Forestgreen" :bold t))
    (((class color) (background dark)) (:foreground "Yellow":bold t))
    (t (:bold t)))
  "Aplac mode face used to highlight instance names."
  :group 'aplac-faces)


(defvar aplac-model-name-face 'aplac-model-name-face
  "Face name to highlight model names in instanciations.")

(defface aplac-model-name-face
  '((((class color) (background light)) (:foreground "Red3"))
    (((class color) (background dark)) (:foreground "Red3"))
    (t (:bold t)))
  "Aplac mode face used to highlight model names in instanciations."
  :group 'aplac-faces)


(defvar aplac-programming-statement-face 'aplac-programming-statement-face
  "Face name to highlight aplac programming statements.")

(defface aplac-programming-statement-face
  '((((class color) (background light)) (:foreground "Red3"))
    (((class color) (background dark)) (:foreground "Red3"))
    (t (:bold t)))
  "Aplac mode face used to highlight programming statements."
  :group 'aplac-faces)


(defvar aplac-directive-face 'aplac-directive-face
  "Face name to highlight aplac directive statements.")

(defface aplac-directive-face
  '((((class color) (background light)) (:foreground "cyan"))
    (((class color) (background dark)) (:foreground "black"))
    (t (:bold t)))
  "Aplac mode face used to highlight directive statements."
  :group 'aplac-faces)


;(defvar aplac-parameter-face 'aplac-parameter-face
;  "Face name to highlight aplac parameters.")

;(defface aplac-parameter-face
;  '((((class color) (background light)) (:foreground "hotpink"))
;    (((class color) (background dark)) (:foreground "black"))
;    (t (:bold t)))
;  "Aplac mode face used to highlight parameters."
;  :group 'aplac-faces)


(defvar aplac-component-name-face 'aplac-component-name-face
  "Face name to highlight aplac component instances.")

(defface aplac-component-name-face
  '((((class color) (background light)) (:foreground "cyan"))
    (((class color) (background dark)) (:foreground "black"))
    (t (:bold t)))
  "Aplac mode face used to highlight aplac component instances. ."
  :group 'aplac-faces)


(defvar aplac-user-model-keyword-face 'aplac-user-model-keyword-face
  "Face name to highlight aplac aplac user model calls.")

(defface aplac-user-model-keyword-face
  '((((class color) (background light)) (:foreground "cyan"))
    (((class color) (background dark)) (:foreground "black"))
    (t (:bold t)))
  "Aplac mode face used to highlight aplac user model calls."
  :group 'aplac-faces)


;; APLAC is case-insensitive
(put 'aplac-mode 'font-lock-keywords-case-fold-search t)



;;------------------------------------------------------------
;; List of Aplac keywords/syntax - order is important!!
;;------------------------------------------------------------

;(defconst aplac-non-comment-string "\"[A-Z$][$A-Za-z0-9.\/\\]*\""
;  "Regexp that describes a string with a $ character inside")

(defconst aplac-model-name "\\([a-z]\\sw+[^ \t\n=]*\\)"
  "Regexp that describes a syntactically correct model or defmodel name")

(defconst aplac-instance-name "\\([a-z][^ \t\n]+\\)"
  "Regexp that describes a syntactically correct defmodel/mosfet/diode/bipolar instance name")

(defconst aplac-line-break "\\(\n\\s-*\\+\\s-*\\)*"
  "Regexp that matches a (possible) line break (\n+)")

(defconst aplac-library-regexp   
; bls  "^\\s-*[*$]*#\\(include\\|libdir\\|library\\)\\s-+\\(?:key=\\w+\\s-\\)*"
  "^\\s-*[*$]*#\\(include\\|libdir\\|library\\)\\s-\"+\\(?:key=\\w+\\s-\\)\"*"
  "Regexp that matches the beginning of library path or filename, or include filename")

(defconst aplac-analysis-keywords
  '(
    "Analyse" "Analyze" "DataFile" "GetAnalyserParam" "GetAnalyzerParam"
    "Guess" "OptimMethod" "Prepare" "SetAnalyserParam" "SetAnalyzerParam"
    )
  "List of Aplac analysis keywords")

(defconst aplac-optim-methods
  '(
    "Anneal" "Exhaustive" "Genetic" "Goal" "GoalData" "Gradient" "GravCenter"
    "GravCentre" "HookeJeeves" "MDSearch" "MinMax" "NelderMead" "Random"
    "Tuning" "None"
    )
  "List of optimization methods.")

(defconst aplac-programming-statements
  '(
    "aplacvar" "declare" "NWA" "Procedure" "EndProcedure" "Read" "SetVar"
    "Sweep" "EndSweep" "Break" "BreakSweep" "Calc" "Call" "CloseFile" 
    "ContinueSweep" "Default" "DefaultModel" "DefNPortFile" "BreakSweep"
    "Calc" "Call" "CloseFile" "ContinueSweep " "Default" "DefaultModel"
    "DefNPortFile" "Else" "EndCalc" "EndFor" "EndIf" "EndWhile" "ExitSweep"
    "For" "Function" "Global" "GoalData" "If" "Init" "OpenFile" "Public"
    "Reload" "Repeat" "Sweep" "Then" "Until" "var" "Vector" "While"
    )
  "List of APLAC Programming statements.")

(defconst aplac-directives
  '(
    "CaseSensitiveUnits" "define" "else" "endif" "ifdef" "ifndef" "include"
    "libdir" "library" "undef" "ver"
    )
  "List of APLAC programming directives.")

(defconst aplac-visual-output-keywords
  '(
    "Display" "Graphics" "Print" "Show"
    )
  "List of APLAC visual output keywords.")


(defconst aplac-model-keywords
  '(
    "res" "r" "cap" "ind" "npn" "pnp" "lpnp" "d" "nmos" "pmos" "njf" "pjf" "nsw" "psw" "opa" "modfas" "logic" "a2d" "d2a" "macro"
    )
  "List of Aplac models")


;bls
;(defconst aplac-commands
;  '(
;    "a2d" "addlib" "alter" "checksoa" "chrand" "chrent" "chrsim" "connect" "conso" "d2a" "defmac" "defwave" "distrib" "end" "endl" "extract" "global" "guess" "ic" "include" "init" "lib" "libfas" "loop" "mcmod" "moddup" "modlogic" "meas" "nocom" "nodeset" "notrc" "optfour" "option" "options" "optnoise" "optpwl" "optwind" "param" "plotbus" "probe" "ramp" "restart" "save" "setbus" "setsoa" "sigbus" "sinus" "solve" "table" "topcell" "use" "width"
;    )
;  "List of Aplac commands (dot cards)")


(defconst aplac-variables
  '(
    "f" "freq" "t" "time"
    )
  "List of APLAC standard variables.")

(defconst aplac-user-model-keywords
  '(
    "DefModel" "EndModel" "Model" "ModelSet"
    )
  "List of user-defined model keywords.")

(defconst aplac-constants
  '(
    "Boltzmann" "co" "ElemQ" "Epso" "EpsSi" "EpsSiO2" "Etao" "Infinite" "j" "kPerQ" "kT0" "Muuo" "pi" "RhoGold"
    )
  "List of Aplac built-in constants.")


(defconst aplac-component-names
  '(
    ;; analogue:
    "Accelerometer" "AirInd" "BJT" "BlackBox" "BSIM" "BSIM3" "Cap" "CCCS" "CCVS" "Coax" "CSource" "Curr" "DCBlock" "DCFeed" "DefNPort" "Diode" "Divider" "DMOS" "DynElem" "EKVMOS" "Filter" "Gm" "Gyrator" "HBT" "HICUM" "IdealTransformer" "IGBT" "Ind" "InputSignal" "IPoly" "JFET" "JJ" "LCFilter" "LDMOS" "LoopFilter" "MESFET" "MEXTRAM" "MOS9" "MOSFET" "Muc" "NPort" "OpAmp" "Param" "PhaseDetector" "PhaseDetector2" "PINDiode" "PLL" "Poly" "Res" "RmPort" "RmRel" "Short" "SMOS" "Subtractor" "Summer" "SumPoint" "Switch" "TF" "ThermalNetwork" "TLine" "TLineDisp" "Toroid" "Trans" "VBIC" "VCCS" "VCO" "VCVS" "Volt" "Wire" "XAmplifier" "XAmplModulator" "XMixer" "XOscillator" "XSummer" "XVCO" "Zblock" 
    ;; RF
    "Aclin" "Circulator" "Clin" "Cmbend" "Cpw" "Maclin" "Mbend" "Mclin" "Mcros" "Mcurve" "Mgap" "Micap" "Mlange" "Mlayout" "Mlin" "Mloc" "Mlsc" "MLSLange" "Mrind" "Mrstub" "Mslit" "Mspind" "Mstep" "Msub" "Mtaper" "Mtee" "Mtfc" "Mtfr" "MultiLayerStruct" "Pad" "Ribbon" "Rind" "Sbclin" "Sbend" "Sclin" "SCSwitch" "Sgap" "Shole" "Slin" "Sloc" "Slsc" "Spind" "Srind" "Ssclin" "Sslin" "Sslot" "Sssub" "Sstep" "Ssub" "Stee" "Via"
    ;;System components (formula based)
    "AdjChSelectivity" "Amplifier" "Antenna" "AudioIntermodulation" "Blocking" "ChebFil" "CoChRejection" "Duplexer" "Gain" "GainBudget" "HarmonicDistortion" "Icp" "IF_Circuit" "Iip3" "Iip3Budget" "IntermodulationRejection" "Isolator" "Mixer" "NFBudget" "NoiseBandwidth" "NoiseFigure" "Oscillator" "PowerDivider" "ReadNthFilter" "Rf Sensitivity" "SpuriousResponse" "SwapBranch" "System" "TwoPort"
    ;; System components (discrete time)
    "AdaptiveFilter" "ADC" "Adder" "Amplifier" "AmplModulator" "And" "Attenuator" "Backlash" "BandpassNonLin" "BERMeter" "BesselBP" "BesselBS" "BesselHP" "BesselLP" "BinaryChannel" "BitDetector" "BitGenerator" "BitMapper" "Buffer" "ButterworthBP" "ButterworthBS" "ButterworthHP" "ButterworthLP" "CauerBP" "CauerBS" "CauerHP" "CauerLP" "CDelay2" "CErrorVector" "ChebyshevBP" "ChebyshevBS" "ChebyshevHP" "ChebyshevLP" "Clock" "CMul" "Compressor" "Correlator" "Counter" "CRot" "DAC" "Decision" "Delay" "Delay2" "Demultiplexer" "DFlip flop" "DiffAmp" "Differentiator" "DownSampler" "DPSKModulator" "DQPSKEncoder" "DualNDivider" "ErrorVector" "Expander" "FIFO" "Formula" "FreqConverter" "FreqCounter" "FreqDemodulator" "FreqDivider" "FreqSamplingFil" "FullAdder" "GaussianBP" "GaussianLP" "GaussianNoise" "HalfAdder" "HilbertTransformer" "IChebyshevBP" "IChebyshevBS" "IChebyshevHP" "IChebyshevLP" "ImpulseResponse" "Integrator" "JKFlip flop" "LaplaceTransform" "Limiter" "MatchedFilter" "MedianFilter" "Mixer" "Monostable" "MSKModulator" "Multipath" "Multiplexer" "Nand" "Nor" "Not" "NthPower" "Or" "Oscillator" "PhaseDemodulator" "PhaseDetector" "PhaseModulator" "PhaseShifter" "Pole" "Polynomial" "Probability" "PSShiftRegister" "Pulse" "PulseJitter" "PulseModulator" "PWL" "QADemodulator" "QAModulator" "Quantizer" "RadioChannel" "RaisedCosineBP" "RaisedCosineLP" "RCHP" "RCLP" "ReadFile" "ReciprocalSinc" "Rectifier" "Resonator" "SampleHold" "Schmitt" "SlipDecoder" "SlipEncoder" "SNRMeter" "SpectrumAnalyser" "SpectrumAnalyzer" "SPShiftRegister" "StoreFile" "Summer" "System" "TimeAverage" "TrigPulse" "TwoPort" "UniformNoise" "UpDownCounter" "UpSampler" "VCO" "VGA" "Voltage" "Waveform" "WindowBP" "WindowBS" "WindowHP" "WindowLP" "WordGenerator" "WordIndicator" "Xor" "Zero" "ZTransform"
    ;; Electromagnetic components
    "AmplShot" "Cylinder" "DataVolume" "DefEMPort" "ElectroMagnetics" "Ellipsoid" "EmbedIndexRegion" "EMCap" "EMInd" "EMPort" "EMRes" "Helix" "HuygenSurface" "MagneticBox" "MagneticPatch" "MetallicBox" "Patch" "RadPat" "RadPower" "SarSeek" "Slab" "SnapShot" "SSCheck" "SSPower" "ThinWire" "TimeProbe" "TraceMax" "Triangle"
    )
  "List of Aplac built-in components.")

(defconst aplac-parameters
  '(
    ;; A
    ("A" "A0" "A1" "A1R" "A2" "A2R" "A3" "A3R" "AALPHA" "AB" "ABCSWITCH" "ABCTRIM" "ABCTYPE" "ABET" "ABETA" "ABS" "ABS_ERR" "AC" "AC10" "ACC" "ACDE" "ACFO" "ACGAM" "ACGD" "ACGS" "ACM" "ACTIVE" "AD" "ADDRESS" "AE" "AEPI" "AEX" "AF" "AFAB" "AFAG" "AFN" "AGAM" "AGAMMA" "AGMX" "AGS" "AIDS" "AIR" "AIRGAP" "AJC" "AJE" "AJS" "AKE" "AKG" "AKP" "ALB" "ALCES" "ALFAV" "ALHC" "ALIT" "ALJEI" "ALJEP" "ALPHA" "ALPHA0" "ALPHA1" "ALPHA_0" "ALPHAR" "ALPHATCE" "ALPR" "ALQAV" "ALQF" "ALT0" "ALVS" "AMP" "AMP_ERR" "AMST" "ANGLE" "ANGLES" "AO" "APLACVAR" "APLACVARS" "APLACVARVECTOR" "APLACVERSION" "APPENDFILE" "AR" "ARD" "AREA" "ARG" "ARI" "ARS" "ART" "AS" "ASCII" "ASL" "ASS" "ASYMMETRIC" "AT" "ATTEN" "AUDIOFILTER" "AUTOSCALE" "AUTOSCALEP" "AUTOSCALEX" "AUTOSCALEY" "AUTOSCALEY2" "AV" "AV_AC" "AVBC" "AVC1" "AVC2" "AVL" "AVST" "AVT0" "AVTO" "AXIS") 
    ;; B
    ("B" "B0" "B1" "B1TC" "B2" "BANDWIDTH" "BASEBANDLIMIT" "BASIC" "BAT" "BEGIN_DOCUMENT" "BEGIN_EQ" "BEQ" "BETA" "BETA0" "BETA2" "BETA_0" "BETA_1" "BETA_AC" "BETATCE" "BETSQ" "BETWEEN" "BEX" "BEXV" "BF" "BFM" "BFN" "BIAS" "BIG_SCREEN" "BINUNIT" "BL" "BOX" "BP" "BR" "BRANCH" "BRANCHVECTOR" "BRD" "BRG" "BRI" "BRM" "BRS" "BS" "BURST_NOISE" "BUTTERW" "BV" "BVD" "BVS" "BVTO" "BW")
    ;; C
    ("C" "C0" "C01" "C02" "C1" "C10" "C1S" "C2" "C4" "CAL" "CAPLACVAR" "CAPM" "CAPMOD" "CAUER" "CBCO" "CBD" "CBEO" "CBS" "CC" "CCCS" "CCJC" "CCJE" "CCJS" "CCS" "CCSO" "CCVS" "CDE" "CDIS" "CDS" "CDS1" "CDS2" "CDS3" "CDSC" "CDSCB" "CDSCD" "CDST" "CE" "CELLSIZE" "CENTER" "CENTERFREQ" "CEOX" "CF" "CFO" "CGBO" "CGBOM" "CGD" "CGD1" "CGD2" "CGD3" "CGD4" "CGDE" "CGDL" "CGDO" "CGDOM" "CGDOTC" "CGDP" "CGDPTC" "CGDT" "CGDTCE" "CGE" "CGS" "CGS1" "CGS2" "CGS3" "CGS4" "CGS5" "CGS6" "CGSL" "CGSO" "CGSOM" "CGSOTC" "CGSP" "CGSPTC" "CGST" "CGSTCE" "CHANNELSEPARATION" "CHEBYSH" "CIN" "CIRCLE" "CIRCLEGOAL" "CIRCUIT" "CIT" "CITI" "CJ" "CJC" "CJCI0" "CJCP" "CJCX0" "CJE" "CJEI0" "CJEP" "CJEP0" "CJGATE" "CJM" "CJO" "CJS" "CJS0" "CJSW" "CJSWG" "CJW" "CKAPPA" "CL" "CLC" "CLE" "CLVL" "CMATRIX" "CMRR" "CO" "COAT" "COFF" "COL" "COLOR" "COMMA" "COMPLEX" "COMPONENTVECTOR" "CON" "COND" "CONST" "CONTOUR" "CONTOURSTEPVECTOR" "CONTROL" "CONV_STRATEGY" "CONVOL" "COOLING_METHOD" "COPLANAR" "CORNER" "COUT" "COVER" "COX" "CPD" "CPG" "CPU" "CRF" "CSDF" "CSO" "CSU" "CTA" "CTH" "CTHSCALE" "CTP" "CUBECELLSIZE" "CURR" "CURRENTS" "CURRM" "CUTOFF" "CVECTOR" )
    ;; D
    ("D" "D0" "DA" "DA1" "DA2" "DATE" "DB" "DBGOAL" "DC" "DCABS_ERR" "DCERR" "DCFNOM" "DCGMAX" "DCGMIN" "DCINIT_SOURCE_STEP" "DCITER" "DCOP" "DCOPS" "DCOPSOF" "DCRMAX" "DCRMIN" "DCSOURCE_STEP_CYCLES" "DCU" "DCZ" "DCZEROPIVOT" "DDELTA" "DEAR" "DEBUG_DERIV" "DECIMALS" "DEGREE" "DEL" "DELAY" "DELTA" "DELTA_0" "DELTA_F" "DELTAT" "DEN" "DENS" "DENSE" "DENSELOGGRID" "DERIV" "DEV" "DEVIATION" "DGAMMA" "DI" "DIAM" "DIODE" "DIODE_BC" "DIODE_BE" "DIR" "DISPLAY" "DIST" "DISTANCE" "DIV" "DIV_FL_F" "DL" "DL0" "DLC" "DLIMIT" "DLVL" "DNL" "DO" "DOCUMENT_STYLE" "DOUBLE" "DRAW_ALL" "DRAW_STYLE" "DROUT" "DS" "DSUB" "DT0H" "DTA" "DTEMP" "DUM1" "DUM2" "DUPLEXSEPARATION" "DURATION" "DV" "DVT0" "DVT0W" "DVT1" "DVT1W" "DVT2" "DVT2W" "DW" "DW0" "DWB" "DWC" "DWG" )
    ;; E
    ("E" "E0" "EA" "EAIC" "EAIE" "EAIS" "EANC" "EANE" "EANS" "EAP" "EC" "EE" "EF" "EFI" "EG" "EI" "ELM" "EM" "EN" "ENC" "END_DOCUMENT" "END_EQ" "ENG" "ENI" "EO" "EOL" "EP" "EPC" "EPHI" "EPI" "EPS" "EPS_OPTIONS" "EQ" "ER" "ERASE" "ERR" "ETA" "ETA0" "ETAALP" "ETAB" "ETABET" "ETADSR" "ETAGAMR" "ETALPHA" "ETAMR" "ETAZET" "ETC" "ETDC" "ETDISABLE" "ETEN" "ETHETA" "ETINTENS" "ETJ" "ETK" "ETMAXTEMP" "ETNO" "ETNONLIN" "ETORD" "ETOT" "ETP" "ETPOS" "ETRAD" "ETRN" "ETSCALE" "ETSENS" "ETU0" "EULER" "EXAVL" "EXMOD" "EXP" "EXPHI" "EXTRAPOLATE" )
    ;; F    
    ("F" "F1" "F2" "FA" "FAC" "FAVL" "FB" "FBC" "FBET1" "FBET2" "FC" "FCC" "FCRBI" "FDQR0" "FE" "FERMI" "FEX" "FF" "FFE" "FGEO" "FI" "FIELD_WIDTH" "FILE" "FINESMITH" "FINESMITHY" "FIRST" "FIRSTIF" "FIT1" "FIXED" "FL_F" "FLAG" "FLICKER_NOISE" "FLICKER_NOISE_F0" "FM" "FNOM" "FO" "FORMAT" "FOUT" "FP" "FP1" "FP2" "FQI" "FREQ" "FREQ_DEPENDENT" "FREQS" "FRX" "FS" "FS1" "FS2" "FTHC" "FTHE1" "FTR" "FTX" "FULL" "FUNC" "FZ" )
    ;; G
    ("G" "G0" "G1" "G2" "GA" "GAIN" "GAINS" "GAM1R" "GAMA" "GAMM" "GAMMA" "GAMMA1" "GAMMA2" "GAMMA_0" "GAMMATC" "GAMOOR" "GDAMP" "GE" "GEAR" "GENERATIONS" "GET" "GGD" "GGS" "GLOBAL_PARAM" "GMAX" "GMIN" "GMRES" "GMRES_ERR" "GMRES_ITER" "GMRES_M" "GO" "GOAL" "GOAL2" "GOAL_LEVEL" "GOALDATA" "GRAD" "GRFORMAT" "GRID" "GROUND" "GS" "GT" "GTFE" "GTHE1" "GUESS" "GZ" )
    ;; H
    ("H" "HARMONIC" "HB" "HB_EQU" "HBABS_ERR" "HBDAMPSTEPS" "HBERR" "HBGMAX" "HBGMIN" "HBINIT_SOURCE_STEP" "HBITER" "HBRMAX" "HBRMIN" "HBSOURCE_STEP_CYCLES" "HBU" "HBZ" "HBZEROPIVOT" "HDIF" "HFC" "HFE" "HFE1" "HFE2" "HFETA" "HFG1" "HFG2" "HFGAM" "HISTOGRAM" "HJCI" "HJEI" "HL" "HOLES" "HP" "HPARAM" "HPGL" "HRCF" "HS" "HU" )
    ;; I
    ("I" "IAPLACVAR" "IBA" "IBB" "IBBE" "IBBT" "IBCI" "IBCIP" "IBCIS" "IBCN" "IBCNP" "IBCXS" "IBD" "IBEI" "IBEIP" "IBEIS" "IBEN" "IBENP" "IBEPS" "IBETS" "IBF" "IBFC" "IBFE" "IBIAS" "IBN" "IBO" "IBR" "IBV" "IBVL" "IC" "ICAPLACV" "ICH"  "ICN" "ICP" "IDEAL" "IDP" "idParam" "IDSS" "IF_NO_CHANGE" "IF_RF" "IGNORE_STAT" "IGO" "IHC" "IIP2" "IIP3" "IIP3METHOD" "IJ" "IJS" "IJTH" "IK" "IKF" "IKP" "IKR" "IKS" "IL" "ILK" "IM" "IMASS" "IN" "IN_LO" "INDEX" "INIT_METHOD" "INIT_SAMPLE" "INIT_SOURCE_STEP" "INIT_VAL" "INL" "INT" "INTER" "INTERPOL" "INTERVAL" "INV" "IO" "IOFF" "IPK" "IPKTC" "IRB" "IREF" "IREIS" "IREPS" "IS" "ISC" "ISCS" "ISE" "ISF" "ISHORT" "ISN" "ISNPN" "ISOLATION" "ISP" "ISR" "ISRR" "ISS" "ISTRING" "ISW" "ITER" "ITEXT" "ITF" "ITSS" )
    ;; J
    ("JBF" "JBR" "JLC" "JLE" "JNO" "JS" "JSSW" "JSW" )
    ;; K
    ("K" "K0" "K1" "K1D" "K2" "K2D" "K3" "K3B" "K3D" "KAPPA" "KB" "KD" "KE" "KETA" "KF" "KFN" "KG" "KGAMMA" "KOR" "KP" "KR" "KRBI" "KS" "KSI" "KT" "KT0" "KT1" "KT1L" "KT2" "KTRG" "KV" "KVS" )
    ;; L
    ("L" "L1" "L2" "L_SIDE" "LAMBDA" "LAMBDA0 " "LAMBDA_0" "LAMBDAR" "LAMBDATC" "LAP" "LAST" "LATB" "LATCHUP" "LATERAL" "LATL" "LAYER" "LD" "LD1" "LDIF" "LEAKIND" "LEFT" "LENGTH" "LER" "LETA" "LEVEL" "LF" "LFG1" "LFG2" "LFGAM" "LG" "LG1" "LGAIN" "LIMIT" "LIMITEDR" "LIN" "LINE" "LINE_WIDTH" "LINEAR" "LINT" "LK" "LK1" "LK2" "LL" "LLC" "LLN" "LMAX" "LMIN" "LMLT" "LMS" "LMUS" "LN0" "LNB" "LND" "LO_IF" "LO_IN" "LO_RF" "LOAD" "LOAD_GUESS" "LOG" "LOGIC" "LOGICHIGH" "LOGPOLAR" "LOGX" "LOGY" "LOGY2" "LOOP" "LOSS" "LOT" "LOT_VAR" "LOWERRX" "LP" "LP1" "LP2" "LPHI" "LPNP" "LREF" "LS" "LSBO" "LSC" "LSSS" "LSSS_SAMPLES" "LT" "LU0" "LU1" "LVAR" "LVFB" "LW" "LWC" "LWL" "LWLC" "LWN" "LX2E" "LX2M" "LX2MS" "LX2MZ" "LX2U0" "LX2U1" "LX3E" "LX3MS" "LX3U1" )
    ;; M
    ("M" "M1" "M2" "M3" "MA" "MAGOAL" "MARKER" "MARKER_ALL" "MARKER_ONLY" "MARKOV_METHOD" "MARKOV_SAMPLES" "MASS" "MATCH" "MATERIAL" "MATRIX" "MATRIX3D" "MATRIXXY" "MATRIXXZ" "MATRIXYZ" "MAX" "MAXNORM" "MAXTAU" "MAZ" "MAZGOAL" "MBCI" "MBCX" "MBEI" "MBEP" "MC" "MC_AND_OPT" "MC_ASSEMBLE" "MC_AVERAGE" "MC_DRAW_ALL" "MC_LOG_ACCEPTED" "MC_LOG_ALL" "MC_LOG_REJECTED" "MC_NOMINAL" "MC_REJECTED" "MC_SAMPLES" "MC_SAVE_EACH" "ME" "MEA_FILE" "MEAN" "METHOD" "MFP" "MGAP" "MGD" "MGS" "MIN" "MINTAU" "MITERED" "MIXCONST" "MJ" "MJ0" "MJC" "MJE" "MJS" "MJSW" "MJSWG" "MJW" "MLOC" "MLSC" "MOBMOD" "MOD_FILE" "MODE" "MODEL" "MODEL_DAMP" "MODEL_LEVEL" "MODULUS" "MOIN" "MOR" "MREI" "MREP" "MS" "MSC" "MSF" "MST" "MSTTC" "MTAU" "MULT" "Mult" "MULTX" "MULTY" "MULTY2" "MUR" "MUS" "MUZ" "MVST" "MXI" )
    ;; N
    ("N" "N0" "N1" "N2" "N_FING" "NA" "NA0" "NAME" "NANODE" "NAPLACVAR" "NASCII" "NAVE" "NAWBM" "NB" "NB0" "NBBE" "NBF" "NBITS" "NBR" "NBV" "NBVL" "NBW" "NC" "NCH" "NCI" "NCIP" "NCN" "NCNP" "NCONTOURS" "ND" "ND0" "NDC" "NDE" "NDEN" "NDEV" "NDRIFT" "NE" "NEB" "NEFF" "NEI" "NEN" "NEPI" "NEPI1" "NEPI2" "NEXT" "NF" "NFACTOR" "NFAR" "NFBR" "NFCR" "NFING" "NFLOOR" "NFMOD" "NFP" "NFR" "NFREQ" "NFREQS" "NFS" "NG" "NGATE" "NGOALS" "NI" "NINT" "NITER" "NJ" "NK" "NKF" "NL" "NLC" "NLE" "NLX" "NMOS" "NMWELL" "NNUM" "NO" "NO_AC" "NO_ANALYSIS" "NO_BURST_NOISE" "NO_DC" "NO_DERIV" "NO_FLICKER_NOISE" "NO_HB" "NO_LOT" "NO_MC" "NO_SHOT_NOISE" "NO_SS" "NO_STAT" "NO_TEMP" "NO_TEXT" "NO_TEXT_WINDOWS" "NO_THERMAL_NOISE" "NO_TRAN" "NO_VERBOSE" "NO_YPARAM" "NOCAP" "NODE" "NODEVECTOR" "NOFF" "NOIA" "NOIB" "NOIC" "NOIMOD" "NOISE" "NOISE_FLOOR" "NOISECONTRIB" "NOISECONTRIBRANGE" "NOISECONTRIBS" "NOISELESS" "NOM" "NONLINEAR" "NORM1" "NORM2" "NORMAL" "NORMALIZE" "NP" "NPAR" "NPCA" "NPEAK" "NPN" "NPOINTS" "NPTICKS" "NQSMOD" "NR" "NRA" "NRB" "NRC" "NRD" "NRG" "NRS" "NS" "NSPIKES" "NSS" "NSTRING" "NSUB" "NTOTAL" "NTR" "NTRAN" "NTW" "NUM" "NUM_TEMP" "NV" "NWA" "NWINDOW" "NXTICKS" "NY2TICKS" "NYTICKS" )
    ;; O
    ("OBJECTVECTOR" "OCT" "OFF" "OFFSET" "OLAY" "ON" "OPAQUE" "OPENFILE" "OPT" "OPT_CYCLES" "OPT_FTOL" "OPT_MC_FILE" "OPT_MITERED" "OPT_RANGE" "OPT_SAMPLES" "OPT_STEP" "OPT_STEPMAX" "OPT_STEPMIN" "OPT_UPDATE" "OPT_VAR" "OPT_XTOL" "OPT_ZOOM" "OPTIMIZE" "ORDER" "OUT" "OUTPUT" "OVERLAP" "OVERSAMPLING" )
    ;; P
    ("P" "P02" "P03" "P04" "P05" "P1" "P10" "P11" "P2" "P20" "P21" "P30" "P31" "P40" "P400" "P41" "P_TRA" "PA" "PAD" "PARAM" "PARASITIC" "PASSIVE" "PATTERN" "PAXISVALUES" "PB" "PBSW" "PBSWG" "PC" "PCLM" "PD" "PDIBLC1" "PDIBLC2" "PDIBLCB" "PDOL" "PE" "PEN" "PER_CENT" "PERI" "PERIOD" "PHA" "PHA_ERR" "PHAGOAL" "PHASE" "PHASE_DOMAIN" "PHASE_FREQ" "PHASE_NOISE" "PHASE_NOISE_SPEC" "PHASEGOAL" "PHI" "PHI0" "PHIBR" "PJ" "PJW" "PLK" "PLOTTER" "PMAX" "PMIN" "PMOS" "PNP" "POLAR" "POLES" "POLY" "POPULATION" "POS" "POWER" "POWER_MODIFIED" "PPS" "PRINT" "PRINTVECTOR" "PROMPT" "PROPAG" "PRT" "PRWB" "PRWG" "PS" "PSAT" "PSATTC" "PSC" "PSCBE1" "PSCBE2" "PSOPHFILTER" "PT" "PTA" "PTF" "PTP" "PTS" "PUBLIC" "PUBLICS" "PULSE" "PULSED" "PVAG" "PWL" "PWL_ACC" "PWL_GRID" "PWL_ITER" )
    ;; Q
    ("Q" "Q0" "Q1" "Q2" "QAVL" "QB" "QBO" "QCO" "QGAD" "QGAG" "QGCL" "QGDH" "QGG0" "QGGB" "QGI0" "QGQH" "QGQL" "QGSH" "QM" "QP0" "QPS" "QTF" )
    ;; R
    ("R" "R1" "R10" "R2" "R_SIDE" "RA" "RAD" "RANDOM" "RANGE" "RASCII" "RB" "RBC" "RBD" "RBD2" "RBI" "RBI0" "RBM" "RBP" "RBS" "RBS2" "RBV" "RBX" "RC" "RCC" "RCI" "RCI0" "RCO" "RCP" "RCV" "RCX" "RD" "RD_0" "RD_1" "RDC" "RDIODE_0" "RDIODE_1" "RDS" "RDSW" "RE" "REAL" "REANNEAL_TIME" "RECIPROCAL" "RECTSMITH" "RECTSMITHI" "RECTSMITHR" "REFER" "RELTMAX" "REPORT" "RESTARTS" "RESULT" "RF" "RF_IF" "RG" "RG_0" "RG_1" "RGD" "RGH" "RHO" "RHON" "RHOR" "RHOW" "RI" "RIGHT" "RIGHT_ANGLE" "RIGOAL" "RIN" "RINCM" "RIPPLE" "RIZ" "RIZGOAL" "RMAX" "RMIN" "RN" "RNE" "ROFF" "ROLLOFF" "RON" "ROUNDED" "ROUT" "RPE" "RPIN" "RQ" "RS" "RS_0" "RS_1" "RSC" "RSH" "RSHM" "RSU" "RTC" "RTCE" "RTH" "RTHSCALE" "RTOLC" "RX_NAME" )
    ;; S
    ("S" "S11" "S2" "SAMPLES" "SAR" "SATLIM" "SAVE" "SAVE_GUESS" "SC" "SCFC" "SCI" "SCIDEAL" "SCRCV" "SCREEN" "SECONDIF" "SEED" "SEP" "SET" "SF0" "SFFM" "SFH" "SG0" "SHAPE" "SHOT_NOISE" "SHUNT" "SIBC" "SIGMA" "SIGMA0" "SIGMAC" "SIGMAE" "SIGMAG" "SIGMAOL" "SIGN" "SIMTIME" "SIN" "SIZE" "SKA" "SKIP" "SL" "SL2GAMOO" "SL2K" "SL2KO" "SL2VTO" "SL3VTO" "SLA1" "SLA2" "SLA3" "SLALP" "SLGAM1" "SLGAMOO" "SLK" "SLKO" "SLMO" "SLOPE" "SLTHE1R" "SLTHE2R" "SLTHE3R" "SLVSBT" "SLVSBX" "SLVTO" "SLZET1" "SMITH" "SMITHY" "SMOOTH" "SN" "SOLVE" "SORT" "SOURCE_STEP_CYCLES" "SPAN" "SPARAM" "SPARSE" "SPECTRUM" "SPECTRUM2" "SPECTRUM_MIN" "SPECTRUMGOAL" "SPECTRUMGOAL2" "SPIKE" "SPULSE" "SR" "SRN" "SRP" "SS" "SSB_BW" "SSNF" "SSTD" "SSTD_ANALYSIS_CYCLES" "SSTD_SOURCE_FIXED" "STA1" "STAT_VAR" "STATISTICS" "STATMAX" "STATMIN" "STDERR" "STEP" "STLTHE1" "STLTHE2" "STLTHE3" "STMO" "STOP" "STORE" "STOREXY" "STOREXZ" "STOREYZ" "STRATEGY" "STRING" "STRING2" "STRINGVAR" "STRIP" "STTHE1R" "STTHE2R" "STTHE3R" "STVTO" "SUB" "SUBTH" "SUMMARY" "SWA1" "SWA2" "SWA3" "SWALP" "SWGAM1" "SWK" "SWKO" "SWTHE1" "SWTHE2" "SWTHE3" "SWVSBX" "SWVTO" "SYMMETRIC" "SYSTEM" )
    ;; T
    ("T" "T0" "TAB" "TABLE" "TAND" "TAND0" "TANDC" "TANDFREQ" "TANDG" "TANDOL" "TAU" "TAU1" "TAU2" "TAUD" "TAUDC" "TAUG" "TAUGD" "TAUNE" "TAUS" "TAVC" "TB" "TBV1" "TBV2" "TC" "TC1" "TC2" "TCB" "TCBI1" "TCBI2" "TCBM1" "TCBM2" "TCC1" "TCC2" "TCE" "TCE1" "TCE2" "TCJ" "TCJSW" "TCJSWG" "TCV" "TD" "TE" "TEF0" "TEMP" "TEMPC" "TEMPK" "TEMPM" "TEX_CMATRIX" "TEX_CVECTOR" "TEX_MATRIX" "TEX_VECTOR" "TEXT" "TEXT2" "TEXTCOLOR" "TEXTCOLOUR" "TEXTERASE" "TEXTPEN" "TF" "TGND" "THCS" "THE1R" "THE2R" "THE3R" "THERMAL_NOISE" "THETA" "TIKF" "TIME" "TIME_DEPENDENT" "TIMESTEP" "TITLE" "TJ" "TLEVR" "TLINE_LEVEL" "TLINE_M" "TLINE_RELTMAX" "TM" "TMAX" "TME" "TMIN" "TMOD" "TNBBE" "TNF" "TNOM" "TNOMC" "TNOMK" "TOL" "TONE" "TOOTH" "TOX" "TOXM" "TPB" "TPBSW" "TPBSWG" "TPG" "TR" "TR1" "TR2" "TR_NAME" "TRAN" "TRAN_NOI" "TRANABS" "TRANERR" "TRANFNOM" "TRANGMAX" "TRANGMIN" "TRANITER" "TRANRMAX" "TRANRMIN" "TRANSPARENT" "TRANU" "TRANZ" "TRANZEROPIVOT" "TRAPEZ" "TRB1" "TRB2" "TRBM1" "TRBM2" "TRC1" "TRC2" "TRD1" "TRD2" "TRE1" "TRE2" "TREF" "TRG1" "TRG2" "TRIAL_METHOD" "TRS1" "TRS2" "TRUNC_ERR" "TS" "TS_ITER1" "TS_ITER2" "TSF" "TSNK" "TT" "TVBBE1" "TVBBE2" "TX_NAME" "TYPE" )
    ;; U
    ("U" "U0" "U00" "U1" "UA" "UA1" "UB" "UB1" "UC" "UC1" "UCEX" "UCRIT" "UEXP" "UN" "UNC" "UNE" "UNIT" "UNLIMITED" "UNTIL" "UO" "UOPEN" "UP" "UPBC" "UPBE" "UPDATE" "UPPERRX" "USER_MODEL" "UTE" "UTRA" )
    ;; V
    ("VA" "VAF" "VALUE" "VAR" "VAR_NR" "VB" "VBBE" "VBC" "VBD" "VBF" "VBI" "VBITC" "VBM" "VBR" "VBR_0" "VBR_1" "VBX" "VCCS" "VCES" "VCO_FL_F" "VCOIL" "VCON" "VCVS" "VDC" "VDCI" "VDCX" "VDD" "VDDM" "VDE" "VDEI" "VDELTA" "VDEP" "VDRIFT" "VDS" "VDS0" "VECTOR" "VEF" "VER" "VERS" "VERSION" "VEXP" "VFB" "VFB0" "VGB" "VGC" "VGE" "VGEXP" "VGEXP_0" "VGJ" "VGS" "VI" "VJ" "VJC" "VJE" "VJS" "VK" "VK_0" "VLF" "VLFC" "VLFE" "VLIM" "VLR" "VMAX" "VMIN" "VO" "VOFF" "VOFFCV" "VOLT" "VOLTAGES" "VON" "VOP" "VPK" "VPKO" "VPKOTC" "VPKS" "VPKSTC" "VPKTC" "VPO" "VPR" "VPT" "VPTCI" "VPTCX" "VPTS" "VRB" "VREF" "VRT" "VS" "VSAT" "VSBTR" "VSBXR" "VST" "VST_0" "VSTTC" "VT" "VT0" "VTEMP" "VTF" "VTH" "VTH0" "VTL" "VTO" "VTO_0" "VTO_1" "VTO_R" "VTOR" "VTOTC" "VTR" )
    ;; W
    ("W" "W0" "WAVEFORM" "WAVEFORM2" "WAVEFORMGOAL" "WAVEFORMGOAL2" "WBE" "WBM" "WC" "WD" "WDF" "WDIA" "WDOG" "WE" "WEIGHT" "WEPI" "WEPI1" "WEPI2" "WER" "WETA" "WF" "WFFREQ" "WFPERIODS" "WFPOINTS" "WFREQ" "WHOLELINE" "WIDTH" "WINDOW" "WINT" "WK1" "WK2" "WL" "WLC" "WLN" "WMAX" "WMIN" "WMLT" "WMS" "WMUS" "WN0" "WNB" "WND" "WOT" "WPHI" "WR" "WREF" "WSP" "WT" "WU0" "WU1""WVAR" "WVFB" "WW" "WWC" "WWL" "WWLC" "WWN" "WX2E" "WX2M" "WX2MS" "WX2MZ" "WX2U0" "WX2U1" "WX3E" "WX3MS" "WX3U1" )
    ;; X
    ("X" "X2E" "X2M" "X2MS" "X2MZ" "X2U0" "X2U1" "X3D" "X3E" "X3MS" "X3U1" "XAXISVALUES" "XBC" "XBE" "XC" "XCJC" "XCJE" "XCJS" "XEXT" "XI" "XIBI" "XII" "XIKF" "XIN" "XIS" "XISR" "XJ" "XJBC" "XJBS" "XL" "XP" "XPART" "XQC" "XQJ" "XRB" "XRBP" "XRBX" "XRC" "XRCX" "XRE" "XRS" "XT" "XTB" "XTF" "XTI" "XVO" "XW" "XY" "XY2" "XY2GOAL" "XYGOAL" )
    ;; Y
    ("Y" "Y2" "Y2AXISVALUES" "Y2GOAL" "Y3D" "YAXISVALUES" "YC" "YGOAL" "YPARAM" )
    ;; Z
    ("Z" "Z0" "Z3D" "ZCI" "ZCX" "ZE" "ZEI" "ZEP" "ZEROPIVOT" "ZEROS" "ZET1R" "ZETACI" "ZETARBI" "ZETARBX" "ZETARCX" "ZETARE" "ZO" "ZOOM" "ZOOM_METHOD" "ZPARAM" "ZS" )
    )
  "List of APLAC parameters.")



(defconst aplac-font-lock-keywords

  (list

   ;; random comments - text between '$' and line-end is
   ;; ignored by aplac. Random comments are used to disable 
   ;; items and for end-of-line notes. 
   '("\\s-*\$[^\"*\n]*" 0 font-lock-comment-face t)



;; The exception to the $ comment rule occurs when '$' is between
;; double quotes.
   '("\"\$[A-Za-z0-9.\/\\]*\"" 0 font-lock-string-face t)






   ;; documentary notes - lines beginning with '*' 
   ;;                     used to document your netlist  
   '("^\\s-*\\*[^$\n].*" 0 font-lock-doc-face t)

;   ;; analysis keywords 
   (list   
    (concat
     "^\\s-*\\("
     (regexp-opt aplac-analysis-keywords)
     "\\)\\>")
     '(0 aplac-analysis-face))


     
   ;; directives (font-lock-constant-face)
   (list   
    (concat
     "^\\s-*#\\("
     (regexp-opt aplac-directives)
     "\\)\\>")
     '(0 font-lock-constant-face))
     
;;bls ;; visual output
   (list   
    (concat
     "^\\s-*\\("
     (regexp-opt aplac-visual-output-keywords)
     "\\)\\>")
     '(0 aplac-output-face))

	   
   ;; defmodel and ends statement
   (list   
    (concat
     "^\\s-*"
     "\\(defmodel\\|model\\|modelset\\)"
     "\\s-+" aplac-model-name "\\>")
    '(1 aplac-user-model-keyword-face)
    '(2 aplac-model-name-face nil t))

;|endmodel\\

   ;; model definition
;   (list   
;    (concat
;     "^\\s-*"
;     "\\(model\\)"
;     "\\s-+" aplac-model-name aplac-line-break "\\s-+\\("
;     (regexp-opt aplac-model-keywords)
;     "\\)" )
;    '(1 font-lock-keyword-face)
;    '(2 font-lock-function-name-face)
;    '(4 font-lock-type-face))

	   
   ;; assignments (xx=zz) - slows down fontification a lot
   '("\\(\\<\\w*\\(\\s(\\w*\\s)\\)*\\)\\s-*=" 1 font-lock-variable-name-face)
   ;; slightly faster
   ;;'("\\<\\([^ \t\n]+\\)\\s-*=" 1 font-lock-variable-name-face)
	    
   ;; libraries, includes....
   (list
    (concat   
     aplac-library-regexp
     "\\s-*\\<\\(\\w+\\)\\>" )
    '(1 font-lock-keyword-face)   
    '(2 aplac-model-name-face nil t))

;bls
;   ;; instances of subcircuits/mosfets/diodes
;   (list
;    (concat   
;     "^\\s-*" aplac-instance-name
;     "\\(\\s-+[^*(=\t\n]+\\|\n\\s-*[+]\\)*\\s-*"
;     "\\<" aplac-model-name "\\>"
;     "\\(\\s-*\n\\|\\s-+[^=]\\)" )
;    '(1 aplac-instance-name-face)   
;    '(3 aplac-model-name-face))

    

   ;; programming statements
   (list
    (concat
     "^\\s-*\\("
     (regexp-opt aplac-programming-statements)
     "\\)\\>")
     '(0 aplac-programming-statement-face))


   ;; component instances
   (list
    (concat
     "^\\s-*\\("
     (regexp-opt aplac-component-names)
     "\\)" " " "\\s-*"  aplac-model-name
     "\\>")
    '(1 aplac-component-name-face)
    '(2 aplac-instance-name-face))




;bls
;   ;; '.' keywords (commands)
;   (list   
;    (concat
;     "^\\s-*\\("
;     (regexp-opt aplac-commands)
;     "\\)\\>")
;    '(0 font-lock-keyword-face))
	   
   ;; highlight additional . unknowns (to avoid stupid typing errors)
   '("^[ \t]*[\.][^ \t\n]*" 0 font-lock-warning-face)
	   
   ;; other keywords - every non fontified word that starts a line    
   ;; except '+' which is line continuation, '*' that starts a doc comment
   ;; and '.' that starts a command.
   '("^[ \t]*[^+*\n.]\\w+" . font-lock-keyword-face)

   ;; there are zillions of parameters in aplac ; (changed for emacs, GVdP)
   (list
    (concat
     "\\<\\("
     (mapconcat 'regexp-opt aplac-parameters "\\|")
     "\\)\\>" )
    '(0 font-lock-variable-name-face))

   )
  "Regexp describing fontification syntax of Aplac keywords"
)



;;------------------------------------------------------------
;; Misc formating functions
;;------------------------------------------------------------


(defun aplac-uncomment-region (beg end)
  "Uncomment selected region - comment symbol is '*'"
  (interactive "r")
  (comment-region beg end -1))


(defun aplac-comment-uncomment-region (beg end &optional arg)
  "Comment out line in region if not commented out, uncomment it if already
commented out (exclusive-or type of comment) - comment symbol is '$' "
  (interactive "r\nP")
  (goto-char beg)
  (narrow-to-region beg end)
  (while (not(eobp))
    (beginning-of-line)
    (if (looking-at comment-start)
	(delete-char (length comment-start))
      (if (looking-at "[ \t]*$") ()
	(insert comment-start) ) )
    (forward-line 1)
    )
  (widen))


(defun aplac-comment-bar (&optional aligned)
  "Insert solid comment bar from column zero to end of line. If optional
   argument is provided, bar will be added from current column."
  (interactive)
  (if (not aligned) (beginning-of-line) )
  (insert "*")
  (insert-char ?- (- aplac-end-comment-column (current-column)))
  (insert "\n"))


(defun aplac-about ()
  (interactive)
  (sit-for 0)
  (message "aplac-mode version %s, © %s" aplac-mode-version aplac-mode-developer))

;; the next function could be much more complete
(defun aplac-reformat-netlist ()
  (interactive)
  (let ((pos (point)))
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*\\+\\([^ ]\\)" nil t)
      (replace-match "+ \\1" ))
    (font-lock-fontify-buffer)
    (goto-char pos))
  )

(defun aplac-customize ()
  "Call the customize function with `aplac-mode' as argument."
  (interactive)
  (customize-browse 'aplac-mode))


;;------------------------------------------------------------
;; Templates/skeletons
;;------------------------------------------------------------

;; bls: disabled, for now. There's a lot of work to do here, if
;; it is even worth doing.

;;; Voltage sources

;(tempo-define-template
; "aplac-vcvs"
; '("E"
;   (p "[name]: ") '(just-one-space)    
;   (p "[positive node]: ") '(just-one-space)
;   (p "[negative node]: ") '(just-one-space)
;   (p "[positive controlling node]: ") '(just-one-space)
;   (p "[negative controlling node]: ") '(just-one-space)
;   (p "[gain]: ") '(just-one-space)
;   )   
; "vcvs"
; "template for inserting an APLAC voltage controlled voltage source")

;(tempo-define-template
; "aplac-ccvs"
; '("H"
;   (p "[name]: ") '(just-one-space)    
;   (p "[positive node]: ") '(just-one-space)
;   (p "[negative node]: ") '(just-one-space)
;   (p "[current source]: ") '(just-one-space)
;   (p "[gain]: ") '(just-one-space)
;   )   
; "ccvs"
; "template for inserting an APLAC current controlled voltage source")


;;; Current sources

;(tempo-define-template
; "aplac-vccs"
; '("G"
;   (p "[name]: ") '(just-one-space)    
;   (p "[positive node]: ") '(just-one-space)
;   (p "[negative node]: ") '(just-one-space)
;   (p "[positive controlling node]: ") '(just-one-space)
;   (p "[negative controlling node]: ") '(just-one-space)
;   (p "[transadmitance]: ") '(just-one-space)
;   )   
; "vccs"
; "template for inserting an APLAC voltage controlled current source")

;(tempo-define-template
; "aplac-cccs"
; '("F"
;   (p "[name]: ") '(just-one-space)    
;   (p "[positive node]: ") '(just-one-space)
;   (p "[negative node]: ") '(just-one-space)
;   (p "[current source]: ") '(just-one-space)
;   (p "[gain]: ") '(just-one-space)
;   )   
; "cccs"
; "template for inserting an APLAC current controlled current source")


;;; Waveforms

;(define-skeleton aplac-pwl
;  "Skeleton for Piece Wise Linear waveform"
;  "Time/value doublet: "
;  "pwl(" str   
;  ( "Next doublet: (%s) "   
;    " "str )
;  resume:
;  ")")

;(tempo-define-template
; "aplac-pulse"
; '("Pulse("
;   (p "[start value]: ") '(just-one-space)    
;   (p "[pulsed value]: ") '(just-one-space)
;   (p "[delay]: ") '(just-one-space)   
;   (p "[rise time]: ") '(just-one-space)
;   (p "[fall time]: ") '(just-one-space)   
;   (p "[pulse duration]: ") '(just-one-space)   
;   (p "[period]: ")
;   ")")   
; "pulse"
; "template for inserting an APLAC Pulse waveform")

;(tempo-define-template
; "aplac-ac"
; '("ac("
;   (p "[magnitude]: ") '(just-one-space)    
;   (p "[phase]: ") '(just-one-space)
;   ")")   
; "ac"
; "template for inserting an APLAC AC waveform")

;(tempo-define-template
; "aplac-pattern"
; '("Pattern "
;   (p "[Vhi]: ") '(just-one-space)    
;   (p "[Vlo]: ") '(just-one-space)
;   (p "[delay]: ") '(just-one-space)   
;   (p "[rise time]: ") '(just-one-space)
;   (p "[fall time]: ") '(just-one-space)   
;   (p "[Bit duration]: ") '(just-one-space)   
;   (p "[Bits]: ")
;   )   
; "pattern"
; "template for inserting an APLAC Pattern function")

;(tempo-define-template
; "aplac-sine"
; '("sin("
;   (p "[Offset]: ") '(just-one-space)    
;   (p "[Amplitude]: ") '(just-one-space)
;   (p "[Frequency]: ") '(just-one-space)   
;   (p "[delay]: ") '(just-one-space)   
;   (p "[Damping factor]: ") '(just-one-space)   
;   ")")   
; "sine"
; "template for inserting an APLAC SINE function")

;(tempo-define-template
; "aplac-sffm"
; '("sffm("
;   (p "[Offset]: ") '(just-one-space)    
;   (p "[Amplitude]: ") '(just-one-space)
;   (p "[Carrier frequency]: ") '(just-one-space)   
;   (p "[Modulation index]: ") '(just-one-space)   
;   (p "[Signal frequency]: ") '(just-one-space)   
;   ")")   
; "sffm"
; "template for inserting an APLAC Single Frequency FM function")

;(tempo-define-template
; "aplac-exp"
; '("exp("
;   (p "[start value]: ") '(just-one-space)    
;   (p "[target value]: ") '(just-one-space)
;   (p "[rise delay]: ") '(just-one-space)
;   (p "[tau1]: ") '(just-one-space)
;   (p "[fall delay]: ") '(just-one-space)
;   (p "[tau2]: ") '(just-one-space)
;   ")")   
; "exp"
; "template for inserting an APLAC EXP waveform")


;(tempo-define-template
; "aplac-noise"
; '("noise("
;   (p "[White noise level]: ") '(just-one-space)    
;   (p "[Flicker noise level]: ") '(just-one-space)
;   (p "[Alpha]: ") '(just-one-space)
;   (p "[Cut-off freq]: ") '(just-one-space)
;   (p "[Filter order]: ") '(just-one-space)
;   ")")   
; "noise"
; "template for inserting an APLAC NOISE waveform")


;;; Macromodels

;(tempo-define-template
; "aplac-comp"
; '("COMP"
;   (p "[Instance name]: ") " "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Output]: ") " "
;   (p "[Model name]: ") " "
;   'n)   
; "comp"
; "template for inserting an APLAC Single output comparator")

;(tempo-define-template
; "aplac-compd"
; '("COMPD"
;   (p "[Instance name]: ") " "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Model name]: ") " "
;   'n)   
; "compd"
; "template for inserting an APLAC Differential output comparator")


;(tempo-define-template
; "aplac-linear-opa0"
; '("Y"
;   (p "[Instance name]: ") " OPAMP0 "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Output]: ") " "
;   (p "[Ground]: ") " "
;   'n)   
; "opa0"
; "template for inserting an APLAC single output linear opamp")

;(tempo-define-template
; "aplac-linear-opa0d"
; '("Y"
;   (p "[Instance name]: ") " OPAMP0D "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Ground]: ") " "
;   'n)   
; "opa0d"
; "template for inserting an APLAC differential output linear opamp")


;(tempo-define-template
; "aplac-linear-opa1"
; '("Y"
;   (p "[Instance name]: ") " OPAMP1 "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Output]: ") " "
;   (p "[Ground]: ") " "
;   'n)   
; "opa1"
; "template for inserting an APLAC single output 1-pole linear opamp")

;(tempo-define-template
; "aplac-linear-opa1d"
; '("Y"
;   (p "[Instance name]: ") " OPAMP1D "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Ground]: ") " "
;   'n)   
; "linear opa1d"
; "template for inserting an APLAC differential output 1-pole linear opamp")

;(tempo-define-template
; "aplac-linear-opa2"
; '("Y"
;   (p "[Instance name]: ") " OPAMP2 "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Output]: ") " "
;   (p "[Ground]: ") " "
;   'n)   
; "linear opa2"
; "template for inserting an APLAC single output 2-pole linear opamp")

;(tempo-define-template
; "aplac-linear-opa2d"
; '("Y"
;   (p "[Instance name]: ") " OPAMP2D "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Ground]: ") " "
;   'n)   
; "linear opa2d"
; "template for inserting an APLAC differential output 2-pole linear opamp")


;(tempo-define-template
; "aplac-delay"
; '("DEL"
;   (p "[Instance name]: ") " "    
;   (p "[Input]: ") " "
;   (p "[Output]: ") " "
;   (p "[Delay value]: ") " "
;   'n)   
; "del"
; "template for inserting an APLAC delay")

;(tempo-define-template
; "aplac-satr"
; '("Y"
;   (p "[Instance name]: ") " SATR "    
;   (p "[Input]: ") " "
;   (p "[Output]: ") " "
;   'n)   
; "satr"
; "template for inserting an APLAC saturating resistor")

;(tempo-define-template
; "aplac-satv"
; '("Y"
;   (p "[Instance name]: ") " SATV "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   'n)   
; "satv"
; "template for inserting an APLAC voltage limiter")

;(tempo-define-template
; "aplac-vswitch"
; '("Y"
;   (p "[Instance name]: ") " VSWITCH "    
;   (p "[Input]: ") " "
;   (p "[Output]: ") " "
;   (p "[Positive controlling node]: ") " "
;   (p "[Negative controlling node]: ") " "
;   'n)   
; "vswitch"
; "template for inserting an APLAC voltage controlled switch")

;(tempo-define-template
; "aplac-cswitch"
; '("Y"
;   (p "[Instance name]: ") " CSWITCH "    
;   (p "[Input]: ") " "
;   (p "[Output]: ") " IC: "
;   (p "[Controlling current]: ") " "
;   'n)   
; "cswitch"
; "template for inserting an APLAC current controlled switch")

;(tempo-define-template
; "aplac-saho"
; '("Y"
;   (p "[Instance name]: ") " SA_HO "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   'n)   
; "saho"
; "template for inserting an APLAC sample&hold")

;(tempo-define-template
; "aplac-trho"
; '("Y"
;   (p "[Instance name]: ") " TR_HO "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Controlling node]: ") " "
;   'n)   
; "trho"
; "template for inserting an APLAC track&hold")


;(tempo-define-template
; "aplac-peakd"
; '("Y"
;   (p "[Instance name]: ") " PEAK_D "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Controlling node]: ") " "
;   'n)   
; "peakd"
; "template for inserting an APLAC peak detector")


;(tempo-define-template
; "aplac-levdso"
; '("Y"
;   (p "[Instance name]: ") " LEV_D "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   'n)   
; "levdso"
; "template for inserting an APLAC single-output level detector")


;(tempo-define-template
; "aplac-levddo"
; '("Y"
;   (p "[Instance name]: ") " LEV_D "    
;   (p "[Positive input]: ") " "
;   (p "[Negative input]: ") " "
;   (p "[Positive Output]: ") " "
;   (p "[Negative Output]: ") " "
;   (p "[Reference node]: ") " "
;   'n)   
; "levddo"
; "template for inserting an APLAC differential-output level detector")


;;; Extracts   

;(tempo-define-template
; "aplac-phmag"
; '(".EXTRACT AC label=\"Phase margin\" xycond(vp("
;   (p "[Node]: " lname)    
;   "),vdb(" (s lname) ")<0.0)+180 "   
;   'n)
; "phmag"
; "template for extracting the phase margin")

;(tempo-define-template
; "aplac-pmmin"
; '(".EXTRACT AC label=\"PM min\" min(vp("
;   (p "[Node]: " lname)    
;   "),0,xdown(vdb(" (s lname) ",0))+180 "   
;   'n)
; "pmmin"
; "template for extracting the minimal phase before unity gain frequency")

;(tempo-define-template
; "aplac-gmag"
; '(".EXTRACT AC label=\"Gain margin\" xycond(vdb("
;   (p "[Node]: " lname)    
;   "),abs(vp(" (s lname) "))>179) "   
;   'n)
; "gmag"
; "template for extracting the gain margin")

;(tempo-define-template
; "aplac-fc"
; '(".EXTRACT AC label=\"Cut freq\" xdown(vdb("
;   (p "[Node]: " lname)    
;   "),yval(vdb(" (s lname) "),1)-3) "   
;   'n)
; "fc"
; "template for extracting the cut frequency")

;(tempo-define-template
; "aplac-ugfc"
; '(".EXTRACT AC label=\"Unity gain freq\" xdown(vdb("
;   (p "[Node]: " lname)    
;   "),0) "   
;   'n)
; "ugfc"
; "template for extracting the unity gain frequency")

;(tempo-define-template
; "aplac-period"
; '(".EXTRACT TRAN xdown(v("
;   (p "[Node]: " lname)    
;   "),"
;   (p "[threshold]: " vth)
;   ","
;   (p "[estimation time]: " t)
;   ",end)"
;   "-xdown(v(" (s lname) "),"(s vth) ","(s t) ",start) $period"   
;   'n)
; "period"
; "template for extracting the period of a signal")


;;; Some macros (defmac)


;(tempo-define-template
; "aplac-period-macro"
; '(".DEFMAC period(a,th,time)=xdown(a,th,time,end)"
;   "-xdown(a,th,time,start)"   
;   'n)
; "period"
; "macro for extracting the period of signal a")

;(tempo-define-template
; "aplac-duty-macro"
; '(".DEFMAC duty_cycle(a,th,time)=(xdown(a,th,time,end)"
;   "-xup(a,th,time,end))/(xdown(a,th,time,end)-xdown(a,th,time,start))*100"
;   'n)
; "duty cycle"
; "macro for extracting the duty cycle of signal a")


;; DEFMODEL and HEADER

;(tempo-define-template
; "aplac-defmodel"
; '(".DEFMODEL "
;   (p "[defmodel name]: " lname) 'r 'n 'n
;   ".ENDS " (s lname)  '>)
; "defmodel"
; "template for inserting an Aplac defmodel")



(tempo-define-template
 "aplac-circuit-header"
 '('aplac-default-header
   'n)
 "header"
 "template for inserting a header for a circuit")


;;------------------------------------------------------------
;; Index menu (using `imenu.el') to create an index of defmodel/models
;;------------------------------------------------------------

;bls
;(defconst aplac-misc-model-names
;  '(
;    "res" "r" "cap" "ind"   
;    "njf" "pjf" "nsw" "psw"
;    "opa" "modfas" "logic" "a2d" "d2a"
;    "macro"
;    )
;  "List of misc Aplac models types (no transistors)")

(defvar aplac-imenu-generic-expression
  (list
;    '("Definitions"
;      "^\\s-*#define\\s-+\\([a-z ]\\w+\\)"
;      1)
    '("Inclusions"
      "^\\s-*#include\\s-+\\([\"\"$a-z]\\w+\\)"
      1)
    '("Libraries"   
      "^\\s-*#library\\s-+\\([a-z]\\w+\\)"   
      1)
    '("Models"   
      "^\\s-*model\\s-+\\([a-z]\\w+\\)"   
      1)
    '("Defmodels"   
      "^\\s-*defmodel\\s-+\\([a-z]\\w+\\)"   
      1)
    )
  "Imenu generic expression for Aplac Mode.  See `imenu-generic-expression'."
  )



(defun aplac-imenu-init ()
  "Initialize index menu."
  (set (make-local-variable 'imenu-generic-expression)
       aplac-imenu-generic-expression)
  (set (make-local-variable 'imenu-case-fold-search) t)    
  (imenu-add-to-menubar "Models")
  )


;;------------------------------------------------------------
;; Hacks to implement the find function menu bar for aplac defmodels/models
;; Fortunately aplac only provides one means of abstraction so the parsing is
;; very easy.   
;;------------------------------------------------------------


(when (fboundp 'function-menu)
  (require 'func-menu)

  (defconst fume-function-name-regexp-aplac
    "^\\s-*\\(defmodel\\|model\\)\\s-+\\([a-z]\\w+\\)"
    "Expression to parse Aplac subcircuit names.")

  (defun fume-find-next-aplac-function-name (buffer)
    "Searches for the next aplac subcircuit name in BUFFER."
    (set-buffer buffer)
    (setq case-fold-search 't)		;;otherwise func-menu bombs....
    (if (re-search-forward fume-function-name-regexp nil t)
	(let ((beg (match-beginning 2))
	      (end (match-end 2)))
	  (cons (buffer-substring beg end) beg))))
  )    

(defun aplac-func-menu-init ()
  "Initialize function menu."

  ;; hook in the aplac mode regular expression above into the association list of
  ;; regexps used by the function menu generator
  (setq fume-function-name-regexp-alist
	(purecopy
	 (append
	  fume-function-name-regexp-alist
	  (list
	   '(aplac-mode . fume-function-name-regexp-aplac)))))
    
    
  ;; hook in the search method above into the association list used by the
  ;; function menu generating code
  (setq fume-find-function-name-method-alist
	(purecopy
	 (append
	  fume-find-function-name-method-alist
	  (list '(aplac-mode . fume-find-next-aplac-function-name)))))
  ;; Now activate func-menu - I hope that these settings don't
  ;; interfere with users settings    
  (make-local-variable 'fume-menubar-menu-name)
  (make-local-variable 'fume-buffer-name)
  (make-local-variable 'fume-index-method)
  (setq fume-menubar-menu-name "Defmodels"
	fume-buffer-name "*Subcircuits List*"
	fume-index-method 2
	)
  (make-local-hook 'find-file-hooks)
  (add-hook 'find-file-hooks 'fume-add-menubar-entry)    
  (define-key global-map '(shift button2) 'fume-mouse-function-goto)
  (fume-add-menubar-entry)
  )


;;------------------------------------------------------------
;; speedbar stuff
;;------------------------------------------------------------


(defun aplac-speedbar-initialize ()
  "Initialize speedbar."
  ;; general settings
  ;; APLAC file extensions (extracted from `auto-mode-alist')
  (let ((mode-alist auto-mode-alist))
    (while mode-alist
      (when (eq (cdr (car mode-alist)) 'aplac-mode)
	(speedbar-add-supported-extension (car (car mode-alist))))
      (setq mode-alist (cdr mode-alist))))
  )

(defun aplac-speedbar (&optional arg)
  "Open/close speedbar."
  (interactive)
  (if (not (fboundp 'speedbar))
      (error "WARNING:  Speedbar is only available in newer Emacs versions")
    (condition-case ()			; due to bug in `speedbar-el' v0.7.2a
	(speedbar-frame-mode arg)
      (error (error "WARNING:  Install included `speedbar.el' patch first")))))



;;------------------------------------------------------------
;; Changelog and sections support
;;------------------------------------------------------------

(defconst aplac-section-regexp-start "^[$*]\\s-*"
  "Aplac mode section header start regexp.")


(defun aplac-add-changelog-entry (changelog-entry)
  "Find changelog section (create it if not found) and add an entry for today."
  (interactive "sChangelog entry: ")
  (goto-char (point-min))
  (if (not (re-search-forward "^\*[\t ]*Changelog" nil t))
      (aplac-add-section "Changelog" (point-max) ))

  (aplac-goto-section "Changelog")
  ;(forward-line 2)
  (let ((string (concat "* " (substring (current-time-string) 0 11)
			(substring (current-time-string) -4) " "
;			(user-full-name) " <" user-mail-address ">")))
			(user-full-name) )))
    (if (not (search-forward string nil t))
	(insert "\n" string "\n\n")
      (forward-line 2))
    (insert "    * " changelog-entry "\n")))
      

(defun aplac-goto-section (section)
  "Move point to the beginning of the specified section;    
If the section is not found, leave point at previous location."
  (interactive "ssection: ")
  (let ((pos (point)))
    (goto-char (point-min))
    (if (not (re-search-forward   
	      (concat aplac-section-regexp-start section "\\b") nil t))
	(progn (message "Couldn't find section %s" section)
	       (goto-char pos)
	       )   
      (progn
	(forward-line 2)
	(recenter))))) ;; added recentre


(defun aplac-add-section (section &optional arg)
  "Add a section in buffer at (optionnal) point arg"
  (interactive "ssection:")
  (if arg   
      (goto-char arg))
  (aplac-comment-bar)
  (insert   
   (concat "*\t" section " \n"))
  (aplac-comment-bar))


(defun aplac-section-p (section)
  "checks if named section is in file, returns t if found, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward   
     (concat aplac-section-regexp-start section "\\b") nil t)
    ))


;;------------------------------------------------------------
;; File initialisation support
;;------------------------------------------------------------
            
(defun aplac-file-initialize ()
  "Create a template for new file"
  (interactive)
  (if (file-exists-p "~/.myaplacdefaults.i")
      (insert-file-contents "~/.myaplacdefaults.i")
    
    (insert   
     (concat aplac-default-header
	     "\n\n\n\n"))
    (aplac-add-section "CHANGELOG")
    (insert "\n\n\n\n")
    )
  (aplac-add-changelog-entry "File created")
  )


;; bls: for this to work, the full path to the file must be specified
;; in the #include or #library line. APLAC provides also #libdir, which
;; is good, but makes this job more complicated. For now, this section
;; will be left in, but will not always be useable. Also, "#libdir"
;; will not trigger 

;;------------------------------------------------------------
;; Support for automatic loading of library files on mouse click
;;------------------------------------------------------------

;;------------------------------------------------------------
;; Mouse bindings (only used by 'aplac-load-file-at-mouse')
;; I separate this from aplac-mode-map so that this particular
;; mouse binding doesn't interfere with other bindings


(if aplac-running-xemacs
    (require 'overlay)
  (require 'lucid)) ;; what else can we do ??

(defconst aplac-library-regexp-start
  "^\\s-*[*$]*#\\(include\\|libdir\\|library\\)\\s-+\\(?:key=\\w+\\s-\\)*"
  "Regexp that matches the beginning of library or include filename")


(defconst aplac-library-regexp-end
  "\\([^ \t\n\"']*\\)"
  "Regexp that matches the end of library or include filename")

(defvar aplac-mode-mouse-map nil
  "Map containing mouse bindings for aplac-mode.")

(if aplac-mode-mouse-map   
    ()
  (setq aplac-mode-mouse-map (make-sparse-keymap))
  (set-keymap-parent aplac-mode-mouse-map aplac-mode-map)
  ; mouse button bindings
  (define-key aplac-mode-mouse-map "\r" 'ffap)
  (if (string-match "XEmacs" emacs-version)
      (define-key aplac-mode-mouse-map 'button2 'ffap-at-mouse)
    (define-key aplac-mode-mouse-map [mouse-2] 'ffap-at-mouse))
  (if (string-match "XEmacs" emacs-version)
      (define-key aplac-mode-mouse-map 'Sh-button2 'mouse-yank)
    (define-key aplac-mode-mouse-map [S-mouse-2] 'mouse-yank-at-click))
  )

(unless (fboundp 'set-extent-keymap)
  (defun set-extent-keymap (extent keymap)
    (set-extent-property extent 'local-map keymap)
    )
  )

(defun aplac-colourize-libraries (beg end old-len)
  "This function colourizes libraries and included files when the mouse
passes over them. Clicking on the middle-mouse button loads them in a buffer.
BEWARE, this feature was hard to implement, and contains (non-fatal) bugs
primarily because emacs 20 does not have the same support for this as xemacs
does."
  (save-excursion
    (let (end-point)
      (goto-char end)
      (end-of-line)
      (setq end-point (point))
      (goto-char beg)
      (beginning-of-line)  ; scan entire line !
      ; delete overlays existing on this line   
      (let ((overlays (overlays-in (point) end-point)))
	(while overlays
	  (if (overlay-get (car overlays) 'detachable)
	      (delete-overlay (car overlays))
	    )
	  (setq overlays (cdr overlays))
	  )
	)
      ; make new ones, could reuse deleted one ?
      (while (search-forward-regexp aplac-library-regexp-start end-point t)
	( let (start-lib extent)
	  (setq start-lib (point))
	  (search-forward-regexp aplac-library-regexp-end end-point)
					; (let ((end-lib (point)))
	  (or (extent-at (point) (buffer-name) 'mouse-face) ;; not yet extended
	      (progn
		(setq extent (make-extent start-lib (point)))
		(set-extent-property extent 'start-closed t)
		(set-extent-property extent 'end-closed t)
		(set-extent-property extent 'detachable t)
		(set-extent-property extent 'mouse-face 'highlight)
		(set-extent-keymap extent aplac-mode-mouse-map)
		)
	      )
	  )
	)
      )
    )
  )

(defun aplac-colourize-libraries-buffer ()
  (interactive)
  ; delete overlays
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (if (overlay-get (car overlays) 'detachable)
	  (delete-overlay (car overlays))
	)
      (setq overlays (cdr overlays))
      )
    )
  ; remake overlays
  (aplac-colourize-libraries  (point-min) (point-max) nil)
  )

;; ffap-at-mouse isn't available in xemacs < 21
;; so define this function to do more or less the same....
(unless (fboundp 'ffap-at-mouse)
(defun ffap-at-mouse (event)
  "loads file under button 2 click. Checks if file is readable."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (beginning-of-line)
    (if (looking-at (concat aplac-library-regexp-start   
			    aplac-library-regexp-end))
        (if (file-readable-p (match-string 2))
            (find-file (match-string 2))
          (message "File '%s' isn't readable" (match-string 2)))
      )))
)



;;------------------------------------------------------------
;; Support for compilation (simulation) - doesn't work currently
;;------------------------------------------------------------

(defvar aplac-simulation-font-lock-keywords
  (list
   '("TOTAL POWER DISSIPATION" . font-lock-keyword-face)
   '("Performing .* analysis" . font-lock-keyword-face)
   '("\\<\\w+ = \\w+\\>" . font-lock-keyword-face)
   )
  "Additional expressions to highlight in Compilation mode.")


(defun aplac-simulation-buffer-name-function (arg)
  (concat "*Aplac-simulation-" (buffer-name) "*")
  )



;;------------------------------------------------------------
;; Aplac-mode starts here
;;------------------------------------------------------------
;;;###autoload
(defun aplac-mode ()
  "Aplac-mode is a major mode for highlighting APLAC netlist files.
APLAC is the simulator product of APLAC Solutions Corporation.
This mode requires font-lock,easy-menu and several other modes that
are part of (x)emacs since ages.
Xemacs-21 is well supported. For Emacs or older versions of Xemacs, your
mileage may vary.
Turning on Aplac mode calls the value of the variable `aplac-mode-hook'   
with no args, if that value is non-nil.

- COMMENTS:
Two types of comments are used:
- the '*' symbol is used to comment out a whole line - that symbol has
to be at the beginning of a line. Use (optionally) for documentation.
- the '$' symbol is used to commment your netlist    

- ABBREVS:
Abbrevs can be turned on and off via the menu. To see the available abbrevs,
use `M-x list-abbrevs'.

- KEY BINDINGS:
\\{aplac-mode-map}

- DEFAULT TEMPLATE:
When you create a new file, a default template is inserted in it. You can
create your own template simply by putting it in your home directory, in a
file called `.myaplacdefaults' .

- HIGHLIGHTING (fontification):
By default, aplac-mode uses its own  set of faces for font-lock. You
can modify these by using the 'customize' package (group: aplac-mode) .
Aplac-mode will then use a custom set of font-lock faces to fontify your file.
Libraries are a special case: library names will be highlighted, and you can  
load them using shift/middle mouse button.

- COMPILATION (simulation)
You can use the 'compile' button to start Aplac with the current buffer as
input netlist. This functionality is also accessible through the menu.


- FUNC-MENU/IMENU/SPEEDBAR
Support for both func-menu and imenu are available in aplac-mode, imenu is activated
by default if it is available (this can be overridden with the 'customize package).
This allows you to have a list of subcircuits and models in a menu.   
Speedbar functionalities are also provided together with imenu support.
  

- CUSTOMIZATION
Aplac-mode is customisable through the (X)emacs 'customize' packages.
Customisable features are faces for font-locking, section headers, Imenu/func-menu
activation and switches to pass to aplac as a compiler.

 
To add automatic support put something like the following in your .emacs file:
  \(autoload 'aplac-mode \"aplac-mode\" nil  t\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.cir\" . aplac-mode\) \
auto-mode-alist\)\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.ckt\" . aplac-mode\) \
auto-mode-alist\)\)    

This will trigger aplac-mode on all files that end in '.cir' or '.ckt' .
If your netlists file don't have a particular extension like '.cir'
you can try adding the following lines at the end of the file:

*** Local Variables:
*** mode:aplac
*** End:

This should trigger aplac-mode on that particular file when opened
(but don't forget to put:
    \(autoload 'aplac-mode \"aplac-mode\" nil  t\)
in your .emacs)

Copyright © 2002 Brennan Sharp <brennanzl@xtra.co.nz>"

  (interactive)
  (kill-all-local-variables)
  (setq tempo-interactive t)
  (setq mode-name "Aplac")
  (setq major-mode 'aplac-mode)   
  (use-local-map aplac-mode-map)
  (setq case-fold-search 't)


  ;; abbreviations table
  (setq local-abbrev-table aplac-mode-abbrev-table)
  (set-syntax-table aplac-mode-syntax-table)

  ;; support for comments
  (set (make-local-variable 'comment-start) "$")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*\\$") ; is buffer local   
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
   
  ;; use Aplac as compiler, on current buffer
  (set (make-local-variable  'compile-command)
       (concat "aplac "
	       
		aplac-simulator-switches " " (file-name-nondirectory buffer-file-name)))
  (set (make-local-variable  'compilation-read-command) 'nil)
  (set (make-local-variable  'compilation-buffer-name-function)
       'aplac-simulation-buffer-name-function)
  (set (make-local-variable 'compilation-error-regexp-alist) nil)
  (set (make-local-variable 'compilation-font-lock-keywords) nil)
  (set (make-local-variable 'compilation-file-regexp-alist) nil)
  (setq compilation-font-lock-keywords 'aplac-simulation-font-lock-keywords)


  ;; add Aplac mode menu
  (if (not aplac-menu-list)
      (setq aplac-menu-list (aplac-create-mode-menu)))
  (easy-menu-add aplac-menu-list)	; for XEmacs
  (easy-menu-define aplac-menu aplac-mode-map
		    "Menu keymap for Aplac Mode." aplac-menu-list)

  ;; initialize default header for this buffer
  (set (make-local-variable 'aplac-default-header)   
       (concat   
	"*---------------------------------------------------------------\n"
	"*-- Project       : \n"
	"*-- Circuit name  : " (buffer-name) "\n"
	"*---------------------------------------------------------------\n"
	"*-- Designer(s)   : " (user-full-name) " <" user-mail-address ">\n"
	"*-- Library       : \n"
	"*-- Purpose       : \n"
	"*-- Inputs        : \n"
	"*-- Outputs       : \n"
	"*-- Supplies      : \n"
	"*-- References    : \n"
	"*---------------------------------------------------------------\n"
	)   
       )
  ;; if new file add a default template
  (if (= (buffer-size) 0)
      (aplac-file-initialize))
    
  ;; initialize imenu/func-menu
  (if aplac-use-imenu
      (if (fboundp 'imenu)
	  (progn   
	    (aplac-imenu-init)
	    (aplac-speedbar-initialize))))

  (if aplac-use-func-menu
      (if (fboundp 'function-menu)
	  (aplac-func-menu-init)))

  ;; font lock start-up
  (set (make-local-variable 'font-lock-defaults)
       (list 'aplac-font-lock-keywords nil t)) ;; nil -> t, don't do strings

  ; now hook in 'aplac-mode-colourize-libraries  

  (make-local-hook 'font-lock-mode-hook)
  (make-local-hook 'font-lock-after-fontify-buffer-hook); doesn't exist in emacs 20
  (add-hook 'font-lock-mode-hook 'aplac-colourize-libraries-buffer t t)
  (add-hook 'font-lock-after-fontify-buffer-hook 'aplac-colourize-libraries-buffer t t) ; not in emacs 20
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'aplac-colourize-libraries t t)

  ;; now run aplac-mode-hook
  (run-hooks 'aplac-mode-hook)

  )

(provide 'aplac-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGELOG ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Created: 21/04/2002 0.0  - copied from eldo-mode
;;                            (Copyright 1997-2001 Emmanuel Rouat)
;; Changed: 21/04/2002 0.1  - replaced word 'eldo' with "aplac"
;;                     0.20 - change comment character from "*" to "$"
;;                     0.21 - change doc comment character from "!" to "*"
;;                     0.22 - transpose comment and doc behaviour
;;                     0.23 - eliminate title line business (n.a.)
;;                     0.24 - eliminate auto-fill and paragraph business
;;                          - replace .aplac with .myaplacdefaults for
;;                            starting new files
;;          22/04/2002 0.25 - get rid of dot cards, e.g. . in .tran
;;          23/04/2002 0.26 - added programming, analysis and directive
;;                            words and faces
;;          27/04/2002 0.27 - added huge list of aplac parameters
;;                          - set up component keyword and ID colouring
;;                          - added my preferred spelling of any aplac
;;                            words that use only USA spelling. This will
;;                            be a trap if aplac does not accept different
;;                            spelling and user does not #define the
;;                            trouble words as necessary. Some aplac words
;;                            have English aliases, others don't (yet?)
;;          29/04/2002 0.28 - removed user-mail-address entry from
;;                            changelog. I broke it I think. Will fix it.
;;           5/05/2002 0.29 - Geert Van der Plas provided this fix: <The
;;                            'zillion' amount of parameters isn't handled
;;                            correctly. There seem to be too many
;;                            parameters for regexp-opt to handle. I've
;;                            solved the problem by splitting the parameter
;;                            list in sublists and merging the list after
;;                            regexp-opt'ing the parts.>
;;                            Thanks Geert.
;;          14/05/2002 0.30 - file initialize: content changed
;;                          - comment and string handling improved
;;          27/07/2992 0.31 - APLAC v7.70 released. New APLAC features 
;;                            accommodated.                       
;;
;;   
;;; aplac-mode.el ends here

;;; Local Variables:
;;; mode:Emacs-lisp
;;; End:   

