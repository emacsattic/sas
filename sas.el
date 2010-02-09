;;-*-emacs-lisp-*-
;;;  file name: sas.el
;;;
;;;  Version 1.10
;;; 
;;;    sas-mode:  indent, run etc, SAS programs.
;;;    Copyright (C) 2001 Tom Cook
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;  Author:   Tom Cook
;;;            Dept. of Biostatistics
;;;            University of Wisconsin - Madison
;;;            Madison, WI 53706
;;;            cook@biostat.wisc.edu
;;   Last change: 6/7/01
;;
;;
;;  Acknowledgements:  
;;
;;  Menu code for XEmacs/Lucid emacs and startup mods
;;  contributed by arossini@biostats.hmc.psu.edu
;;
;;  Code for electric semi stolen from sas-mode.el written by Mark Riggle
;;          (sasmsr@unx.sas.com)

;;  startup mods.  1/30/95

(if (assoc "\\.sas$" auto-mode-alist) nil
  (setq auto-mode-alist
        (append 
         '(("\\.sas$" . sas-mode)
           ;; ("\\.s$" . S-mode) ;; Uncomment this if you use .s for S files
           ("\\.lst$" . sasl-mode))
         auto-mode-alist)))

;;  variables section
(defvar sas-mode-map () "")
(defvar sasl-mode-map () "")
(defvar sasd-mode-map () "")
(defvar sas-mode-syntax-table nil "SAS-mode syntax table")
(defvar sasl-mode-syntax-table nil "SASL-mode syntax table")
(defvar sas-require-confirmation t
  "*Require confirmation when revisiting sas-output which has changed on disk.")
(defvar sas-file-name nil "Root (sans .sas) name of sas file.")
(setq-default sas-file-name nil)
(make-variable-buffer-local 'sas-file-name) ; TDC 3/3/95

;; added sas-program 4/29/94.  user can specify a different version of sas.
(defvar sas-program "sas" "*Name of program which runs sas.")

;; user variables 
(defvar sas-mode-hook nil "")
(defvar sasl-mode-hook nil "")
(defvar sasd-mode-hook nil "")
(defvar sas-pre-run-hook nil
  "Hook to execute prior to running SAS vis submit-sas.")
(defvar sas-custom-file-name "~/.sas"
  "Customization file. If you plan to change this, this variable must be
set before loading sas-mode.")
(defvar sas-use-fontlock t "If t, will use font-lock in sas source buffers")

(defvar sas-options-string ""
  "*Options to be passed to sas as if typed on the command line.")
(defvar sas-indent-width 4 "*Amount to indent new sas statements from DATA, PROC etc.")
(defvar sas-indent-continue-width 4 "*Amount to indent continued sas statements")
(defvar sas-notify t "*Beep and display message when job is done?")  ;; added 4/7/94
(defvar sas-error-notify t
  "*If sas-notify is t, then indicate errors in log file upon completion")
;; added 5/2/94
(defvar sas-get-options nil "Options to be passed to SAS in sas-get-dataset")
(defvar sas-get-options-history nil "History list of Options passed to SAS in sas-get-dataset")
(defvar sas-dataset nil "Name of sas dataset associated with this buffer")
(make-variable-buffer-local 'sas-dataset)

;;added 9/19/97
(defvar sas-proc-print-statement "proc print data = %s.%s ;\n"
  "*SAS print statement to be used in sas-get-dataset.
Passed to format with arguments LIBNAME and DATASET")

(defvar sas-page-number-max-line 3
  "*Number of lines from the page break in which to search for the page number")
(defvar sas-submitable t
  "*If t the current sas program is submittable.  This variable exists so that
certain files, which are not intended to be run alone but rather %included
won't be run by mistake.")
(make-variable-buffer-local 'sas-submitable)

(defvar sas-indent-ignore-comment "*"
  "*Comments with start with this string are ignored in indentation.")

(defvar sas-fix-indent-comment "+;"
  "*this string is appended to sas-indent-ignore-comment and comments of
this type set the indentation column for future statements ") ;; 2/2/95 TDC

(defvar sas-notify-popup nil
  "*If t (and sas-notify is also t), causes emacs to create a
popup window when the SAS job is finished.")

(defvar sas-buffer-file-name nil   ;; changed to -file  5/31/95 TDC
  "Stores the buffer file name for the sas source code in order to check
whether or not the file has changed.")
(make-variable-buffer-local 'sas-buffer-file-name)
(defvar sas-tmp-libname "_tmp_" "*Libname to use for sas-get-dataset.")
(defvar sas-electric-semi t
  "*Treat ';' as an indenter command also ans maybe add a newline,
    see sas-auto-newline")
(defvar sas-auto-newline t
  "*Non-nil means TAB in SAS mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.")
(defvar sas-string-delimiter "")
(setq sas-string-delimiter "\\(['\"]\\)" )


;; keymaps etc...

(require 'shell)

(if (not sas-mode-syntax-table)
    (progn (setq sas-mode-syntax-table (make-syntax-table))
           (modify-syntax-entry ?_  "w" sas-mode-syntax-table)
           (modify-syntax-entry ?\\  "." sas-mode-syntax-table)
           (modify-syntax-entry ?'  "\"" sas-mode-syntax-table)))
(if (not sasl-mode-syntax-table)
    (setq sasl-mode-syntax-table (make-syntax-table)))

(if sas-mode-map ()
   (setq sas-mode-map (make-sparse-keymap))
   ;;(define-key sas-mode-map "\C-c\C-i" 'indent-sas-statement)
   (define-key sas-mode-map "\C-c\C-a" 'beginning-of-sas-statement)
   (define-key sas-mode-map "\e\C-a" 'beginning-of-sas-proc)
   (define-key sas-mode-map "\e\C-e" 'next-sas-proc)
   (define-key sas-mode-map "\C-c\C-s" 'switch-to-sas-source)
   (define-key sas-mode-map "\C-c4S" 'switch-to-sas-source-other-window)
   (define-key sas-mode-map "\C-c4\C-s" 'switch-to-sas-source-other-window)
   (define-key sas-mode-map "\C-c\C-t" 'switch-to-sas-log)
   (define-key sas-mode-map "\C-c4\C-t" 'switch-to-sas-log-other-window)
   (define-key sas-mode-map "\C-c4t" 'switch-to-sas-log-other-window)
   (define-key sas-mode-map "\C-c\C-o" 'switch-to-sas-lst)
   (define-key sas-mode-map "\C-c4\C-o" 'switch-to-sas-lst-other-window)
   (define-key sas-mode-map "\C-c4o" 'switch-to-sas-lst-other-window)
   (define-key sas-mode-map "\C-c\C-r" 'run-sas-on-region)
   (define-key sas-mode-map "\C-c\C-b" 'run-sas-on-buffer)
   (define-key sas-mode-map "\C-c\C-l" 'submit-sas)
   (define-key sas-mode-map "\C-c\C-d" 'sas-get-dataset)
   (define-key sas-mode-map "\C-c\C-c" 'switch-to-sas-process-buffer)
   (define-key sas-mode-map ";" 'electric-sas-semi)

   (cond ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
	       (>= emacs-major-version 19))
	  (define-key sas-mode-map [menu-bar sas]
	    (cons "SAS" (make-sparse-keymap "sas")))
	  (define-key sas-mode-map [menu-bar sas get-dataset]
	    '("View a SAS dataset" . sas-get-dataset))
	  ;;(define-key sas-mode-map [menu-bar sas indent-current]  3/30/95 TDC
      ;;'("Indent current statement" . indent-sas-statement))
	  (define-key sas-mode-map [menu-bar sas log-other-window]
	    '("SAS log other window" . switch-to-sas-log-other-window))
	  (define-key sas-mode-map [menu-bar sas log]
	    '("SAS log" . switch-to-sas-log))
	  (define-key sas-mode-map [menu-bar sas lst-other-window]
	    '("SAS lst other window" . switch-to-sas-lst-other-window))
	  (define-key sas-mode-map [menu-bar sas lst]
	    '("SAS lst " . switch-to-sas-lst))
	  (define-key sas-mode-map [menu-bar sas run-on-region]
	    '("Submit Region " . run-sas-on-region))
	  (define-key sas-mode-map [menu-bar sas run]
	    '("Submit File " . submit-sas))
	  ))
   )

(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       ;; XEmacs menu code
       (defvar sas-mode-menu
         '("SAS"
           ["View a SAS dataset" sas-get-dataset t]
           ;;["Indent current statment" indent-sas-statement t]  3/30/95 TDC
           ["SAS log other window" switch-to-sas-log-other-window t]
           ["SAS log" switch-to-sas-log t]
           ["SAS lst other window" switch-to-sas-lst-other-window t]
           ["SAS lst" switch-to-sas-lst t]
           ["Submit Region" run-sas-on-region t]
           ["Submit File" submit-sas t]))
       
       (defun sas-mouse-menu (e)
         (interactive "e")
         (mouse-set-point e)
         (beginning-of-line)
         (search-forward ":" nil t)
         (popup-menu sas-mode-menu))
       
       (defun sas-install-menubar ()
         (if default-menubar
             (let ((menu (cond ((eq major-mode 'sas-mode) sas-mode-menu)
                               (t (error "not in SAS mode")))))
               (set-buffer-menubar (copy-sequence default-menubar))
               (add-menu nil "SAS" (cdr menu)))))
       
       (add-hook 'sas-mode-hook 'sas-install-menubar)))


(if sasl-mode-map ()
   (setq sasl-mode-map (make-sparse-keymap))
   (define-key sasl-mode-map "\C-c\C-s" 'switch-to-sas-source)
   (define-key sasl-mode-map "\C-c4s" 'switch-to-sas-source-other-window)
   (define-key sasl-mode-map "\C-c4\C-s" 'switch-to-sas-source-other-window)
   (define-key sasl-mode-map "\C-c\C-t" 'switch-to-sas-log)
   (define-key sasl-mode-map "\C-c4\C-t" 'switch-to-sas-log-other-window)
   (define-key sasl-mode-map "\C-c4t" 'switch-to-sas-log-other-window)
   (define-key sasl-mode-map "\C-c\C-o" 'switch-to-sas-lst)
   (define-key sasl-mode-map "\C-c4\C-o" 'switch-to-sas-lst-other-window)
   (define-key sasl-mode-map "\C-c4o" 'switch-to-sas-lst-other-window)
   (define-key sasl-mode-map "\C-c\C-p" 'sas-fix-page-numbers)
   (define-key sasl-mode-map "\C-c\ep" 'sas-page-fix)
   (define-key sasl-mode-map "\C-c\C-f" 'fix-page-breaks)
   (define-key sasl-mode-map "\C-c\C-d" 'sas-get-dataset)
   (define-key sasl-mode-map "\C-c\C-c" 'switch-to-sas-process-buffer)

   (cond ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
	       (>= emacs-major-version 19))
 	  (define-key sasl-mode-map [menu-bar sas]
	    (cons "SAS" (make-sparse-keymap "sas")))
	  (define-key sasl-mode-map [menu-bar sas get-dataset]
	    '("View a SAS dataset" . sas-get-dataset))
	  (define-key sasl-mode-map [menu-bar sas renumber-pages]
	    '("Renumber remaining pages" . sas-page-fix))
	  (define-key sasl-mode-map [menu-bar sas renumber-page]
	    '("Renumber current page" . sas-fix-page-numbers))
	  (define-key sasl-mode-map [menu-bar sas log-other-window]
	    '("SAS log other window " . switch-to-sas-log-other-window))
	  (define-key sasl-mode-map [menu-bar sas log]
	    '("SAS log" . switch-to-sas-log))
	  (define-key sasl-mode-map [menu-bar sas lst-other-window]
	    '("SAS lst other window" . switch-to-sas-lst-other-window))
	  (define-key sasl-mode-map [menu-bar sas lst]
	    '("SAS lst " . switch-to-sas-lst))
	  (define-key sasl-mode-map [menu-bar sas source-other-window]
	    '("SAS source other window" . switch-to-sas-source-other-window))
	  (define-key sasl-mode-map [menu-bar sas source]
	    '("SAS source" . switch-to-sas-source))
	  ))
   )


(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       ;; XEmacs menu code.
       (defvar sasl-mode-menu
	 '("SASl"
	   ["View a SAS dataset" sas-get-dataset t]
	   ["Renumber remaining pages" sas-page-fix t]
	   ["Renumber current page" sas-fix-page-numbers t]
	   ["SAS log other window" switch-to-sas-log-other-window t]
	   ["SAS log" switch-to-sas-log t]
	   ["SAS lst other window" switch-to-sas-lst-other-window t]
	   ["SAS lst" switch-to-sas-lst t]
	   ["SAS source other window" switch-to-sas-source-other-window t]
	   ["SAS source" switch-to-sas-source t]))
       
       (defun sasl-mouse-menu (e)
	 (interactive "e")
	 (mouse-set-point e)
	 (beginning-of-line)
	 (search-forward ":" nil t)
	 (popup-menu sasl-mode-menu))
       
       (defun sasl-install-menubar ()
	 (if default-menubar
	     (let ((menu (cond ((eq major-mode 'sasl-mode) sasl-mode-menu)
			       (t (error "not in SASl mode")))))
	       (set-buffer-menubar (copy-sequence default-menubar))
	       (add-menu nil "SASl" (cdr menu)))))

       (add-hook 'sasl-mode-hook 'sasl-install-menubar)
       ))


(if sasd-mode-map ()
  (setq sasd-mode-map (make-sparse-keymap))
  (define-key sasd-mode-map "\C-c\C-r" 'revert-sas-dataset)
  (define-key sasd-mode-map "\C-c\C-s" 'switch-to-dataset-source-buffer)
  (define-key sasd-mode-map "\C-c\C-t" 'switch-to-dataset-log-buffer)
  (define-key sasd-mode-map "\C-c\C-d" 'sas-get-dataset)
  (cond ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
	       (>= emacs-major-version 19))

	 ;; FSF Emacs Menu code
	 (define-key sasd-mode-map [menu-bar sas]
	   (cons "SAS" (make-sparse-keymap "sas")))
	 (define-key sasd-mode-map [menu-bar sas get-dataset]
	   '("View a SAS dataset" . sas-get-dataset))
	 (define-key sasd-mode-map [menu-bar sas dataset-log-buffer]
	   '("SAS log" . switch-to-dataset-log-buffer))
	 (define-key sasd-mode-map [menu-bar sas source-buffer]
	   '("SAS source " . switch-to-dataset-source-buffer))
	 (define-key sasd-mode-map [menu-bar sas revert]
	   '("Revert dataset" . revert-sas-dataset))
	 ))
  )


(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       ;; XEmacs menu code
       (defvar sasd-mode-menu
	 '("SASd"
	   ["View a SAS dataset" sas-get-dataset t]
	   ["Revert dataset" revert-sas-dataset t]
	   ["SAS log" switch-to-sas-log t]
	   ["SAS source" switch-to-sas-source t]))
       (defun sasd-mouse-menu (e)
	 (interactive "e")
	 (mouse-set-point e)
	 (beginning-of-line)
	 (search-forward ":" nil t)
	 (popup-menu sasd-mode-menu))
       (defun sasd-install-menubar ()
	 (if default-menubar
	     (let ((menu (cond ((eq major-mode 'sasd-mode) sasd-mode-menu)
			       (t (error "not in SASd mode")))))
	       (set-buffer-menubar (copy-sequence default-menubar))
	       (add-menu nil "Sasd" (cdr menu)))))
       (add-hook 'sasd-mode-hook 'sasd-install-menubar)
       ))


;;  function definitions 

(defun sas-mode-variables ()
  (make-local-variable 'sentence-end)
  (setq sentence-end ";[\t\n */]*")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \t]*$")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^[ \t]*$")
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sas-indent);; use my indent
  ;;(setq indent-line-function 'sas-indent-line)  ;; try Riggle's indent
  (make-local-variable 'comment-start)
  (setq comment-start "\\*\\|/\\*")
  (make-local-variable 'comment-end)
  (setq comment-end ";\\|\\*/")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\*+")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'sas-file-name))


(defun sas-mode ()
  "Major mode for editing and running sas programs.  "
  (interactive)
  (kill-all-local-variables)
  (use-local-map sas-mode-map)
  (setq major-mode 'sas-mode)
  (setq mode-name "SAS")
  (sas-mode-variables)
  (set-syntax-table sas-mode-syntax-table)
  (set-sas-file-name)
  (cond (sas-use-fontlock
         (require 'sas-fontlock)
         (make-local-variable 'font-lock-defaults)
         (setq font-lock-defaults '(sas-mode-font-lock-keywords nil t))
         (turn-on-font-lock)))
  (run-hooks 'sas-mode-hook)
  (message "SAS mode updated 6/7/01.  See info file for documentation.")
  )

(defun sasl-mode ()
  "Major mode for editing sas log and lst files.
    \\{sasl-mode-map}
    Entry into this mode calls the value of `sasl-mode-hook'
    if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sasl-mode-map)
  (setq major-mode 'sasl-mode)
  (setq mode-name "SAS")
  (set-syntax-table sasl-mode-syntax-table)
  (make-local-variable 'sas-file-name)  ; TDC 3/3/95
  (if (null sas-file-name) (set-sas-file-name))
  (run-hooks 'sasl-mode-hook))

(defun sasd-mode ()
  "Major mode for viewing sas datasets.
    \\{sasd-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map sasd-mode-map)
  (setq major-mode 'sasd-mode)
  (setq mode-name "SAS")
  (run-hooks 'sasd-mode-hook)
  )



(defun beginning-of-sas-statement (arg &optional comment-start nested);; nested added 2/14/95
  "Move point to beginning of current sas statement.  First argument arg is a repeat
    argument.  COMMENT-START t means that if the statement starts with a comment, stay
at the beginning of the comment rather that skipping ahead to the first real text.
If NESTED is non-nil, and point is inside \"()\", go to the beginning of the text
immediately after the opening \"(\"."
  (interactive "P")
  (let (pos (end-string (if nested "\\(;[ \n\t*/]*$\\)\\|[()]" ";[ \n\t*/]*$" )))
    ;; 5/14/97  added \t to regexp in case TAB follows ";"
    (if (search-forward ";" nil 1) (forward-char -1))
    (setq pos (point))
    (sas-forward-char-noerr -1)
    (while (not (or (bobp) (looking-at "[;(]")))  
      (if (looking-at ")") (progn (forward-char 1) (backward-sexp 1)))
      (re-search-backward end-string (point-min) 1 arg))
    (if (looking-at"[;(]") (sas-forward-char-noerr 1))
    (skip-chars-forward " \t\n\f;")  ;; remove "\" in "\ " 10/11/00
    (if comment-start nil
      (if (looking-at "\\*/")
          (progn (forward-char 2)
                 (skip-chars-forward " \t\n\f;")))  ;; remove "\" in "\ " 10/11/00
      (while (looking-at "/\\*")
        (if (not (search-forward "*/" pos t 1));;(;; (point-max) 1 1)
            (forward-char 2))
        (skip-chars-forward " \t\n\f")  ;; remove "\" in "\ " 10/11/00
        ))
    ))

;; this function isn't much good anymore.  3/30/95 TDC
;;;(defun indent-sas-statement (arg)
;;;  "Indent all continuation lines sas-indent-width spaces from first
;;;line of statement."
;;;  (interactive "p")
;;;  (let (end)
;;;  (save-excursion
;;;    (if (> arg 0)
;;;        (while (and (> arg 0) (search-forward ";" (point-max) 1 1))
;;;          (setq end (point))
;;;          (sas-forward-char-noerr -1)
;;;          (beginning-of-sas-statement 1)
;;;          (forward-char 1)
;;;          (indent-region (point) end (+ (current-column) (1- sas-indent-width)))
;;;          (search-forward ";" (point-max) 1 1)
;;;          (setq arg (1- arg)))))))

(defun sas-indent ()
  "Indent function for SAS mode."
  (interactive)
  (let (indent prev-end line-start;; line start added 3/1/97 TDC
               (pos (- (point-max) (point)))
               (case-fold-search t)
               (cur-ind (current-indentation))
               (comment-start (sas-comment-start));; 3/12/95 TDC
               (paren-start (sas-paren-start));; 3/12/95 TDC
               (sas-current-point (point))
               (sas-indent-continue-width sas-indent-continue-width)
               )
    (save-excursion
      (cond ((progn 
               (back-to-indentation)
               (setq line-start (point));; 3/1/97 TDC
               (or (bobp)
                   (looking-at
                    "data[ ;]\\|proc[ ;]\\|run[ ;]\\|endsas[ ;]\\|g?options[ ;]\\|%macro[ ;]\\|%mend[ ;]")))
;;;  Case where current statement is DATA, PROC, etc...
             (setq prev-end (point))
                                        ;(princ paren-start)
             (goto-char (min (or paren-start
                                 comment-start
                                 (point-min)) line-start));; line-start 3/1/97 TDC
;;;  Added 6/27/94
;;;  May get fooled if %MACRO, %DO, etc embedded in comments
             (setq indent (+ (* (- (sas-how-many "^[ \t]*%macro\\|[ \t]+%do"
                                                 prev-end)
                                   (sas-how-many "^[ \t]*%mend\\|%end" prev-end))
                                sas-indent-width)
                             (if;; 3/1/97  TDC
                                 (and (or paren-start comment-start) (< (or paren-start comment-start (point-min)) line-start))
                                 (sas-get-column (or paren-start comment-start) 1)
                               0)
                             )));; 2/1/95 TDC
;;;  Case where current line begins with sas-indent-ignore-comment
            ((progn;; added 6/27/94  to leave "* ;" comments alone.
               (back-to-indentation)
               (and (not (looking-at "*/"))
                    (looking-at (concat sas-indent-ignore-comment "\\|/\\*"))))
             (setq indent (current-indentation)))
;;;  Case where current statement not DATA, PROC etc...
            (t (beginning-of-line 1)
               (skip-chars-backward " \n\f\t")
               (sas-forward-char-noerr -1)
               (setq indent (or (sas-next-line-indentation) cur-ind))))) ; 3/12/95 TDC
    (save-excursion 
      (let (beg end)
        (back-to-indentation)
        (setq end (point))
        (beginning-of-line 1)
        (setq beg (point))
        (delete-region beg end)
        (indent-to indent)))
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;; 3/12/95 TDC
(defun sas-next-line-indentation ()
  "Returns the indentation for the next line of sas code.  Assumes that point is before the last character of the line."
  (let (prev-end indent paren-start)
    (cond
             ;;; next line is start of new statement
     ((looking-at ";")       ;  modified 1/31/95
      (setq indent (sas-next-statement-indentation)))
     ;;((looking-at ")")       ; 2/14/95 
     ;;(setq indent (+ (sas-next-statement-indentation) sas-indent-width)))
             ;;; current line ends with end of comment 
     ((save-excursion;; added 4/28/94 to properly check
        (sas-forward-char-noerr -1);; for end of comment
        (setq prev-end (point))
        (looking-at "*/"));;  improved 1/31/95
      (save-excursion                            
        (sas-backward-comment)
        ;;(search-backward "*/" (point-min) 1 1)  ;; comment start is first /*
        ;;(search-forward "/*" prev-end 1 1)      ;; after previous */ 
        ;;(if (not (bobp)) (backward-char 2))                       ;; 2/1/95 TDC
        (skip-chars-backward " \n\f\t")
        (sas-forward-char-noerr -1)
        (setq indent (sas-next-line-indentation))
        ))
             ;;; ignorable comment
     ((save-excursion;; added 6/27/94 to leave "* ;" comments alone
        (progn
          (beginning-of-sas-statement 1 t)
          (and (not (looking-at "*/"))
               (looking-at sas-indent-ignore-comment))))
      (setq indent nil))
                  ;;; paren's open on this line.
     ((save-excursion (forward-char 1);; added 2/14/95 to check for open
                      ;;(if (re-search-backward    ;; parentheses
                      ;;   "[)(]"
                      ;;   (save-excursion (back-to-indentation) (point)) 1)
                      ;;  (if (looking-at "(")
                      ;;      (setq indent (+ (current-column) 1))))
                      (setq paren-start
                            (condition-case ()
                                (scan-lists (point) -1 1)
                              (error 0)))
                      (back-to-indentation)
                      (<= (point) paren-start))
      (goto-char paren-start)
      (setq indent (+ 1 (current-column) sas-indent-continue-width)))
     ((save-excursion (beginning-of-sas-statement 1) (bobp));; added 4/13/94
      (setq indent sas-indent-continue-width));; so the first line works
                    ;;;  2/27/97 check for parens closing. 
     ((progn (back-to-indentation)
             (setq prev-end (point))
             (end-of-line)
             (if (re-search-backward ")" prev-end t)
                 (progn (setq paren-start
                              (condition-case ()
                                  (scan-lists (point) -1 1)
                                (error (point-max))))
                        (>= prev-end paren-start))))
      (goto-char (1- paren-start))
      (sas-next-line-indentation))
     (t
      (if (progn
            (save-excursion
              (beginning-of-line 1)
              (skip-chars-backward " \n\f\t")
              (sas-forward-char-noerr -1)
              (or (looking-at ";")
                  (sas-forward-char-noerr -1) (looking-at "\\*/"))))
          (setq indent (+ (current-indentation) sas-indent-continue-width))
        (setq indent (current-indentation)))))))

;; added 9/31/94 
(defun sas-next-statement-indentation ()
  "Returns the correct indentation of the next sas statement.
The current version assumes that point is at the end of the statement.
This will (hopefully) be fixed in later versions."
  (if (bobp) 0
    (let (paren-start );; 3/12/95
      (save-excursion
        (if
        (and (setq paren-start (condition-case ()
                                   (scan-lists (point) -1 1)
                                 (error nil)))
             (save-excursion (back-to-indentation)
                             (<= (point) paren-start)))
        (progn (goto-char (1+ paren-start));; 3/1/97 TDC
               (if (sas-indent-after-p)
                   (+ (current-column) sas-indent-width)
                 (current-column)))
        ;; (and nil (re-search-backward    ;;  added 2/14/95 to check for open "("
        ;; "[)(\"\']"
        ;; (save-excursion (back-to-indentation) (point)) 1)
        ;; (looking-at "("))
        ;;(+ (current-column) 1) 2/27/97 commented out don't know what it's for.
        (let ((prev-end (point)))
          (beginning-of-sas-statement 1 nil t);; 2/14/95 added args to get comments/() right
          (while (and (not (bobp))
                      (not (looking-at "*/"))
                      (not (looking-at;;  2/2/95 TDC
                            (concat sas-indent-ignore-comment sas-fix-indent-comment)))
                      (looking-at sas-indent-ignore-comment)
                      (and (looking-at "/\\*");; 3/1/97 TDC
                           (save-excursion
                             (search-forward "*/" sas-current-point t))))
            (skip-chars-backward " \n\f\t")
            (sas-forward-char-noerr -1)
            (setq prev-end (point))
            (sas-backward-comment);;  2/14/95 skip past prior comments  TDC
            (skip-chars-backward " \n\f\t")
            (sas-forward-char-noerr -1)
            (beginning-of-sas-statement 1 t))
          (cond ((sas-indent-after-p);; sas-indent-after-p added 3/1/97 TDC
                 (+ (current-column) sas-indent-width));; 2/14/95 TDC
                ((looking-at "%?end[ ;\n]\\|%mend[ ;\n]\\|\\*/")
                 (max (- (current-indentation) sas-indent-width) 0))
                ((looking-at (concat sas-indent-ignore-comment sas-fix-indent-comment))
                 (current-indentation));;  2/2/95  TDC
                ((looking-at ")")
                 (forward-char 1)
                 (backward-sexp)
                 (beginning-of-sas-statement 1);; 3/1/97 TDC
                 (sas-next-statement-indentation)
                 )
                (
                 t (current-column)));; 2/27/97  replaced current-indentation with current-column 
          ))))))

;; created 3/1/97 
(defun sas-indent-after-p ()
  "Should next statement be indented by sas-indent-width?"
  (or
   ;;(if (looking-at "cards[ \n\t;]") (setq sas-indent-continue-width 0) nil)  ;;Trying to get "cards" to work 2/17/97 - later
   (looking-at
    "data[ \n\t;]\\|proc[ \n\t]\\|%?do[ \n\t;]\\|select[ \n\t;]\\|%macro[ \n\t]\\|/\\*") ;; add select 
   (save-excursion
     (re-search-forward
      "\\(\\b%?then\\>[ \n\t]*\\b%?do\\>\\|\\bwhen\\>[ \n\t]*(.*)[ \n\t]*\\b%?do\\>\\|\\b%?else\\>[ \n\t]*\\b%?do\\>\\)\\|\\(;[ \t]*$\\)"
      prev-end 1 1)
     (match-beginning 1)))
                        ;;; fixed 1/30/95 to avoid being fooled by
                        ;;; variable names starting with "do"
                        ;;; 2/14/95 stop search at next ";[ \t]$"
  )


;; added 2/1/95
;; changed 3/10/95 to return position of comment rather than column
(defun sas-comment-start()
  "If the current line is inside a /* */ comment, returns column in which the
opening /* appears.  returns nil otherwise."
  (let ((pos (point)))
    (save-excursion
      (if (and (search-backward "*/" (point-min) 1 1)
               (search-forward "/*" pos 1 1))
          ;;(current-indentation)
          (point)
        nil))))

;;; 3/9/95 TDC
;; changed 3/10/95 to return position of paren-start rather than column
(defun sas-paren-start()
  "If the current line is inside (), returns column in which the
opening ( appears.  returns nil otherwise."
  (let (beg)
    (save-excursion
      (if (setq beg (condition-case ()
                        (scan-lists (point) -1 1)
                      (error nil)))
          (progn (goto-char beg)
                 (point))
                 ;;(1+ (current-column)))
        nil))))
        
;;; 3/10/95 TDC
(defun sas-get-column (arg &optional add)
  "Returns column of position ARG.  Optional ADD returns column + ADD."
  (let ((col (if arg (save-excursion (goto-char arg) (current-column)))))
    (if (and col add)
        (+ col add))))


;;  2/7/95
(defun sas-backward-comment ()
  "Go to the beginning of the current comment."
  (interactive)
  (let ((start (point)) pos)
    (if (and (looking-at " ") (not (bobp)))
        (progn (skip-chars-backward " \t\n\f")
               (sas-forward-char-noerr -1)))
    (cond ((and (progn (search-backward "*/" (point-min) 1 1)
                (search-forward "/*" (+ 2 start) 1 1))
               (progn (forward-char -2)
               (> start (point))))
           (setq start (point))
           (skip-chars-backward " \n\f\t")
           (sas-forward-char-noerr -1)
           ;;(if (not (bobp)) (forward-char -1))
           (setq pos (sas-backward-comment))
           (if pos (goto-char pos)
             (goto-char start)))
          ((progn
             (sas-forward-char-noerr -2)
             ;;(if (not (bobp)) (forward-char -2))
                  (beginning-of-sas-statement 1 t)
                  (and (looking-at sas-indent-ignore-comment)
                       (> start (point))))
           (setq start (point))
           (skip-chars-backward " \n\f\t")
           (sas-forward-char-noerr -1)
           (setq pos (sas-backward-comment))
             (if pos (goto-char pos)
               (goto-char start)))
          (t (progn
               (goto-char start)
               nil)))))

;; 2/8/95
(defun sas-forward-char-noerr (arg)
  "move arg characters or as far as possible in the desired direction,
don't return error"
  (if (> arg 0) (forward-char (min arg (- (point-max) (point))))
    (forward-char (max arg (- (point-min) (point))))))


;;  Created 6/27/94 to verify that RUN; statements match PROC and DATA
;;  statements.  Useful since indentation my be goofy w/o "RUN;"
(defun sas-check-run-statements ()
  "Check to see that \"run\" statements are matched with proc, data statements."
  (interactive)
  (let (pos (ok t) (eob-ok t))
    (save-excursion
      (beginning-of-line)
      (while ok
        (if (re-search-forward "\\(^[ \t]*run[ ;]\\)\\|\\(^[ \t]*proc \\|^[ \t]*data[ ;]\\)" nil 1)
              (if (match-beginning 2)
                  (if (re-search-forward "\\(^[ \t]*run[ ;]\\)\\|\\(^[ \t]*proc \\|^[ \t]*data[ ;]\\)" nil t)
                      (progn (setq pos (point))
                             (setq ok (match-beginning 1)))
                    (setq eob-ok nil pos (point-max))))
                (setq ok nil)))
    (setq ok (eobp)))
  (if (and ok eob-ok) (message "Run statements match")
    (goto-char pos)
    (beep)
    (message "Missing Run Statement."))))

;; electric-sas-semi and electric-sas-terminator copied from sas-mode.el by Mark Riggle
;;   (SAS Institute Inc., sasmsr@unx.sas.com)
;; They seem to work OK. 
(defun electric-sas-semi (arg)
  "Insert character and correct line's indentation if not on column 1"
  (interactive "P")
  (if (and sas-electric-semi 
	   (not (= 0 (current-column)))
	   (not (eq (preceding-char) ?\;)))
      (electric-sas-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-sas-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos
	 in-string
	 (end (point)))
    (if (and (not arg) (eolp)
	     (= 1 (prefix-numeric-value arg))
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    ;; check if we are in a string
		    ;;  assume strings can only be on one line
		    (while (re-search-forward sas-string-delimiter end t)
			   (setq in-string t)
			   (if (re-search-forward (char-to-string (preceding-char))
				 end t)
			     (setq in-string nil)))
		    in-string)))
      (progn
	(insert last-command-char)
	(sas-indent)
	(if nil ;; sas-flash-end
	  (save-excursion
	    (forward-word -1)
	    (if (looking-at "end;")
	      nil)) ;;(sas-flash-open)))
      )
	(and sas-auto-newline
	     (progn
	       (newline)
	       (sas-indent)))
	)
      (self-insert-command (prefix-numeric-value arg)))))


(defun sas-fix-life-tables (start end)
  "Remove censored and duplicate observations from life tables generated by
Proc Lifetest.  Operates on current region.  A major space saver if there is
heavy censoring."
  (interactive "r")
  (save-excursion 
    (shell-command-on-region
     start end
     "sed \"\\?          *\\.          *\\.          *\\.    ?d\"" t)))

(defun sas-fix-page-numbers (offset &optional page-num)
  "Fix number of current page in sas output files after editing.  Add
OFFSET to actual page number."
  (interactive "P")
  (if (not offset) (setq offset 0))
  (if (not page-num) (setq page-num (sas-page-number)))
  (save-excursion
    (if (/= (preceding-char) ?\C-l) (backward-page 1))
    (let (end len mstart mend)
      (save-excursion
        (forward-line sas-page-number-max-line)
        (setq end (point)))
      (if (re-search-forward
           "\\(^[0-9]+[ ]\\)\\|\\([ ][0-9]+$\\)"
           end t)
          (progn (setq len (- (match-end 0) (match-beginning 0))
                       mstart (match-beginning 0)
                       mend (match-end 0))
                 (delete-region mstart mend)
                 (if (eolp)
                 (insert (format
                          (concat "%" len "d") (+ page-num offset)))
                 (insert (substring
                          (concat (+ (sas-page-number) offset) "      ")
                          0 len))))))))

(defun sas-page-fix (start)
  "Fix page numbers in sas output from point to end of file.  If START is given this will be the number for the current page."
  (interactive "P")
  (let (offset (pnum (sas-page-number)))
  (if (not start) (setq offset 0)
    (setq offset (- start pnum)))
  (while (not (eobp))
    (sas-fix-page-numbers offset pnum)
    (setq pnum (1+ pnum))
    (forward-page 1))))

(defun sas-fix-page-breaks ()
  "Fix page breaks in SAS 6 print files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "\f") (delete-char 1))
    (replace-regexp "^\\(.+\\)\f" "\\1\n\f\n")
    (goto-char (point-min))
    (replace-regexp "^\f\\(.+\\)" "\f\n\\1")
    (goto-char (point-min))
    (replace-regexp "$" "")
    (goto-char (point-min))
    (replace-regexp "\\([^\\$]+\\)" "\n\\1")
    (goto-char (point-max))
    (if (not (bobp))
        (progn (backward-char 1)
               (if (not (looking-at "\n"))
                   (progn (forward-char 1) (open-line 1)))))
    ;;;(basic-save-buffer)
    )
  )

(defun sas-page-number ()
  ;; like what-page except it returns an integer page number
  "Return page number of point in current buffer."
  (let ((opoint (point))) (save-excursion
       (goto-char (point-min))
       (1+ (sas-how-many page-delimiter opoint)))))

(defun sas-how-many (regexp &optional end)
  ;; a copy of `how-many' which returns an integer
  ;; rather than a message
  "Return number of matches for REGEXP following/preceding point."
  (let ((count 0) opoint)
    (save-excursion
      (cond ((> (point) end) ;; 3/1/97 TDC
             (setq opoint end)
             (setq end (point))
             (goto-char opoint)))
        (while (and (not (eobp))
                    (progn (setq opoint (point))
                           (re-search-forward regexp end t)))
          (if (= opoint (point))
	   (forward-char 1)
       (setq count (1+ count))))
        count)))

(defun beginning-of-sas-proc ()
  "Move point to beginning of sas proc, macro or data step."
  (interactive)
  (let ((case-fold-search t))
    (forward-char -1)
    (while (not (or (looking-at "data\\|proc\\|%macro")
                    (bobp)))
      (re-search-backward "proc\\|data\\|%macro" (point-min) 1)
      (beginning-of-sas-statement 1))))

(defun next-sas-proc (arg)
  "Move point to beginning of next sas proc."
  (interactive "P")
  (let ((case-fold-search t))
    (forward-char 1)
    (if (re-search-forward
         "^[ \t]*\\(data[ ;]\\|proc[ ;]\\|endsas[ ;]\\|g?options[ ;]\\|%macro[ ;]\\)"
         nil t arg)
        (beginning-of-sas-statement 1)
      (forward-char -1))))

(defun set-sas-file-name ()
  "Stores the name of the current sas file"
  (let ((name (buffer-file-name)))
    (cond ((not name )
           (setq sas-submitable nil))  ;; 3/1/97 for buffers without files - TDC
          ((string-match (substring name -4 nil) "\\.sas\\|\\.lst\\|\\.log")
           (setq sas-file-name (substring name 0 (- (length name) 4)))
           (setq sas-buffer-file-name (buffer-file-name))
           )
          (t (message "This file does not have a standard suffix")))))

;;  created 6/27/94
(defun sas-set-alternate-file-name (name)
  "Stores the NAME of an alternate sas file.  When this file is submitted with
submit-sas, the  alternate file will be submitted instead.  sas-submitable
is automatically sets to t."
    (interactive "f")
    (cond ((string-match (substring name -4 nil) "\\.sas\\|\\.lst\\|\\.log")
           (setq sas-file-name (substring name 0 (- (length name) 4)))
           (setq sas-submitable t))
          (t (message "This file does not have a standard suffix"))))

(defun switch-to-sas-source ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file "sas"))

(defun switch-to-sas-lst ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file "lst"))

(defun switch-to-sas-log ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file "log"))

(defun switch-to-sas-source-other-window ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file-other-window "sas"))

(defun switch-to-sas-lst-other-window ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file-other-window "lst"))

(defun switch-to-sas-log-other-window ()
  "Switches to sas source file associated with the current file"
  (interactive)
     (switch-to-sas-file-other-window "log"))
 
;; (defun switch-to-sas-file (suff &optional revert silent)
;;   "Switches to sas \"SUFF\" file associated with the current file"
;;   (let* ((sfile sas-file-name)
;;          (buf (get-file-buffer (concat sfile "." suff)))
;;          (sas-require-confirmation
;;           (and sas-require-confirmation (not revert))))
;;     (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
;;         (find-file (concat sfile "." suff))
;;       (progn (switch-to-buffer buf)
;;              (if (not (verify-visited-file-modtime (current-buffer)))
;;                  (progn (revert-buffer t t)
;;                         (if (not silent)
;;                             (message "File has changed on disk.  Buffer automatically updated."))))))
;;     (setq sas-file-name sfile))
;;   (if (string-equal suff "sas")
;;       (if (not (string-equal major-mode "sas-mode"))
;;           (sas-mode))
;;     (if (not (string-equal major-mode "sasl-mode"))
;;         (sasl-mode))))
;; 
;; (defun switch-to-sas-file-other-window (suff)
;;   "Switches to sas \"SUFF\" file associated with the current file"
;;   (let* ((sfile sas-file-name)
;;          (buf (get-file-buffer (concat sfile "." suff))))
;;     (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
;;         (find-file-other-window (concat sfile "." suff))
;;       (progn (switch-to-buffer-other-window buf)
;;              (if (not (verify-visited-file-modtime (current-buffer)))
;;                  (progn (revert-buffer t t)
;;                         (message "File has changed on disk.  Buffer automatically updated.")))))
;;     (setq sas-file-name sfile))
;;   (if (string-equal suff "sas")
;;       (if (not (string-equal major-mode "sas-mode"))
;;           (sas-mode))
;;     (if (not (string-equal major-mode "sasl-mode"))
;;         (sasl-mode))))

(defun switch-to-sas-file (suff)
  "Switches to sas \"SUFF\" file associated with the current file"
  (switch-to-buffer (set-sas-file-buffer suff))
  )

(defun switch-to-sas-file-other-window (suff)
  "Switches to sas \"SUFF\" file associated with the current file"
  (switch-to-buffer-other-window (set-sas-file-buffer suff))
  )

(defun display-sas-lst ()
  "Display to sas source file associated with the current file in another
window, but don't select it."
  (interactive)
    (display-sas-file "lst"))

(defun display-sas-file (suff)
  "Displays sas \"SUFF\" file associated with the current file"
  (display-buffer (set-sas-file-buffer suff))
  )

;;  The following was created 6/7/94 to handle buffers without messing up
;;  windows.
(defun set-sas-file-buffer (suff &optional revert silent)
  "Sets current buffer to sas \"SUFF\" file associated with the current file"
  (let* ((sfile sas-file-name)
         (buf (get-file-buffer (concat sfile "." suff)))
         (sas-require-confirmation
          (and sas-require-confirmation (not revert))))
    (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
        (set-buffer (find-file-noselect (concat sfile "." suff)))
      (progn (set-buffer buf)
             (if (not (verify-visited-file-modtime (current-buffer)))
                 (progn (revert-buffer t t)
                        (if (not silent)
                            (message "File has changed on disk.  Buffer automatically updated."))))))
    (setq sas-file-name sfile))
  (if (string-equal suff "sas")
      (if (not (string-equal major-mode "sas-mode"))
          (sas-mode))
    (if (not (string-equal major-mode "sasl-mode"))
        (sasl-mode)))
  (current-buffer))

(defun switch-to-sas-process-buffer ()
  "Switch to sas-process-buffer"
  (interactive)
  (let (buf proc-name)
    (setq proc-name (concat "SAS" sas-file-name)
          buf (concat "*" proc-name "*"))
    (switch-to-buffer-other-window buf)))

(defun submit-sas ()
  ;; 6/17/94  added sas-submitable local variable.
  "Submit SAS file as shell command."
  (interactive)
  (if (or sas-submitable
          (progn
            (beep)
            (y-or-n-p
             (format "Submission is disabled for this file.  Submit it anyway? "))))
      ;;(progn 
      (or (catch 'still-running
        (if (or                       ;;  change 5/31/95 TDC
             ;;(string-equal sas-buffer-name (buffer-name)) 
             (string-equal sas-buffer-file-name (buffer-file-name)) 
             (not
              (y-or-n-p
               (format "The name of this buffer has changed.  Submit the new file? "))))
            (setq sas-buffer-file-name (buffer-file-name))
          (set-sas-file-name))
        (let ((sas-file sas-file-name) proc-name buf)
          (if (buffer-modified-p)
              (if (y-or-n-p (format "Buffer %s is modified. Save it? "
                                    (buffer-name)))
                  (save-buffer)))
          (setq proc-name (concat "SAS" sas-file)
                buf (concat "*" proc-name "*"))
          (if (get-buffer buf)
              (save-window-excursion
                ;;(switch-to-buffer buf)
                (set-buffer buf)         ;;; 3/21/97 
                (cond ((process-status proc-name)   ;; 3/21/97  Check for running
                       (beep)                       ;;        process
                       (throw 'still-running nil))) 
                (erase-buffer)
                (setq default-directory
                      (file-name-directory sas-file))))
          (run-hooks 'sas-pre-run-hook);; added 8/24/94
          (message "----  SAS job submitted   ----")
          (if (stringp sas-options-string)   ;; sas-options-string should be a
                                             ;;; list of words.  fixed 10/13/98 TDC
              (setq sas-options-string (sas-string-to-list sas-options-string)))
          (apply 'make-comint proc-name sas-program nil;; added sas-program 4/29/94
                         (reverse (append (list sas-file-name)
                                          (reverse sas-options-string))) )
          (save-window-excursion
            (switch-to-buffer buf)
            (make-variable-buffer-local 'comint-output-filter-functions)  ;; 2/13/94 TDC
            (setq comint-output-filter-functions nil)  ;;  avoid hangups when 
            (setq sas-file-name sas-file)              ;; comint-hilit loaded
            (bury-buffer))
          
          (message "----  SAS job submitted   ----")
          (if sas-notify;;  added 4/7/94
              (set-process-sentinel (get-process proc-name) 'sas-sentinel)
            (display-buffer buf t)))
          t)
          (message "A SAS process is still running.  Use C-c 4 times to kill it.")
     )
    (message "----  File not submitted  ----")))

;; added 10/13/98 - converts sas-options-string to list of options.
(defun sas-string-to-list (str)
  "Convert string to list of word constituents"
  (interactive)
  (if (string-match " +$" str)
      (setq str (substring str 0 (match-beginning 0))))
  (let ((outlist (list)))
    (while (not (string-equal str ""))
      (string-match " *\\([^ ]+\\)" str)
      (setq outlist
            (append outlist 
                    (list (substring str (match-beginning 1) (match-end 1)))))
      (setq str (substring str (match-end 1))))
    outlist))
;; 5/2/94 Modified sas-sentinel to check for errors in log file upon
;; completion.
(defun sas-sentinel (proc arg);; created 4/7/94
  "Notify user that SAS run is done"
  (beep)
  ;;(if (string-equal arg "finished\n")
  (save-excursion
    (let (msg buf win (sbuf (concat "*" (process-name proc) "*")))
      (setq msg
            (format "SAS %s %s"
                    (substring arg 0 -1)
                    (if sas-error-notify 
                        ;;(save-window-excursion
                        (progn
                          (set-buffer sbuf)
                          (setq buf (set-sas-file-buffer "log" t t))
                          (goto-char (point-min))
                          (setq win (get-buffer-window buf))
                          (save-window-excursion
                            (if win
                                (progn 
                                  (select-window win)
                                  (if (re-search-forward "^ERROR" nil t)
                                      " (See .log file for errors)"
                                    ""))
                              (set-buffer buf)
                              (if (re-search-forward "^ERROR" nil t)
                                  " (See .log file for errors)"
                                ""))))
                    "")))
    (set-buffer sbuf)
    (goto-char (point-max))
    (insert msg)
    (bury-buffer (get-buffer sbuf))
    (if (and sas-notify-popup window-system)
        (x-popup-dialog
         t
         (list "SAS Menu" (cons msg  nil) )))
    ;;(if (not (minibuffer-window-active-p)) (princ msg))
    (message msg))))



;; 5/2/94 Modified run-sas-on-region to separate log and output buffers.
;; 
(defun run-sas-on-region (start end append &optional buffer)
  "Submit region to SAS"
  (interactive "r\nP")
  (message "----  Running SAS  ----")
  (let ((sfile sas-file-name)
        (shell-file-name "/bin/sh")
        serror buff)
    (setq buffer (or buffer "*SAS output*"))
    (save-excursion
      (shell-command-on-region
       start end   ;; added sas-program 
       (concat sas-program " " (if (stringp sas-options-string) sas-options-string
                                (mapconcat 'identity  sas-options-string " "))
               " -nonews -stdio 2> /tmp/_temp_.log" nil))  ;; added sas-options-string 8/20/97 - Fixed 4/24/01
      (get-buffer-create "*SAS Log*")
      (save-window-excursion
        (switch-to-buffer "*SAS Log*")
        (erase-buffer)
        (insert-file-contents "/tmp/_temp_.log")
        (delete-file "/tmp/_temp_.log")
        (setq serror (re-search-forward "^ERROR" nil t))
        (if serror () (bury-buffer)))
      (setq buff (get-buffer-create buffer))
      (save-window-excursion
        (switch-to-buffer buff)
        (setq sas-file-name sfile)
        (if append
            (progn
              (end-of-buffer)
              (insert "\f\n"))
          (erase-buffer))
        (if (get-buffer "*Shell Command Output*")
            (progn (insert-buffer "*Shell Command Output*")
                   (kill-buffer "*Shell Command Output*"))
          (insert "SAS completed with no output."))
        (if append () (sasl-mode))
        (message "----  SAS Complete ----")))
    (if (not serror)
        (switch-to-buffer-other-window  buff)
      (switch-to-buffer-other-window "*SAS Log*")
      (goto-char serror)
      (beep)
      (message "Error found in log file.")
      )))
  
(defun run-sas-on-buffer (append)
  (interactive "P")
  (run-sas-on-region (point-min) (point-max) append))

(defun switch-to-dataset-log-buffer ()
  "Switch to log buffer for run-sas-on-region."
  (interactive)
  (switch-to-buffer-other-window "*SAS Log*"))

(defun switch-to-dataset-source-buffer ()
  "Switch to source buffer for run-sas-on-region."
  (interactive)
  (switch-to-buffer-other-window (format " *sas-tmp-%s*" sas-dataset)))

;; 4/19/95 TDC
(defun sas-set-options ()
  "Interactively set sas-options-string."
  (interactive)
  (let ((minibuffer-history sas-get-options-history))
    (setq sas-options-string (read-string "SAS options: " sas-options-string))
    (setq sas-get-options-history minibuffer-history)))


(defun sas-get-dataset (filename &optional arg opts-p append buffer vars)
  "Run proc contents and proc print on SAS dataset.  Automatically prompts 
for SAS options to use.  Default options are defined by the variable
`sas-get-options'.  Output may be updated from within output buffer with
\\[revert-sas-dataset] if dataset changes.  Also, the source code which generates the output
may be edited with \\[switch-to-dataset-source-buffer].  Typing \\[revert-sas-dataset] within the output buffer reexecutes
the (modified) source code."
  (interactive "fName of SAS dataset (file name):")
  (let ((file (file-name-nondirectory filename))
        (dir (file-name-directory filename))
        (opts sas-get-options)
        (minibuffer-history sas-get-options-history)
        buf fsize bfile)
    (setq bfile (expand-file-name filename))
    (setq buffer (or buffer (concat "*" bfile "*")))
    (setq opts (if opts-p opts (read-string "SAS options: " opts)))
    (setq sas-get-options-history minibuffer-history)
    (cond ((string-match (substring file -6 nil) "\\.ssd01")
      (setq file (substring file 0 (- (length file) 6))))
    (t (error "This file is not a SAS dataset.")))
    (setq buf (format " *sas-tmp-%s*" bfile))
    (get-buffer-create buf)
    (save-window-excursion
      (switch-to-buffer buf)
      (erase-buffer)
      (setq default-directory dir)
      (if opts 
          (insert (format "options  %s ;\n" opts)))
      (insert (format "title \"Contents of SAS dataset `%s'\" ;\n" file))
      (insert (format "libname %s '%s' ;\n" sas-tmp-libname dir))
      (if (not (equal arg 1))
               (insert (format "proc contents data = %s.%s ;\n" sas-tmp-libname file)))
      (if (equal arg 2) ()
        (insert (format sas-proc-print-statement sas-tmp-libname file))
        (if vars (insert (format "  var %s ;\n" vars))))
      (run-sas-on-region (point-min) (point-max) append
                         buffer)
      (get-buffer buffer)
      (if append () (sasd-mode))  ;; added 5/5/94 
      (setq sas-dataset bfile))
    (if (get-buffer-window buffer t)
        (raise-frame (window-frame (get-buffer-window buffer t)))
    (display-buffer buffer (not append)))
    ))
    
(defun revert-sas-dataset ()
  "Revert current sas dataset from disk version"
  (interactive)
  (let* ((file sas-dataset)
        (buf (format " *sas-tmp-%s*" file))
        (pos (point)))
      (save-window-excursion
        (switch-to-buffer buf)
        (run-sas-on-region (point-min) (point-max) nil
                           (concat "*" file "*"))
        )
      (goto-char pos)  ;; added 6/9/94
    (sasd-mode)  ;; added 5/5/94 
    (setq sas-dataset file)))


(defun sas-insert-local-variables ()  ;; created 6/17/94 
  "Add local variables code to end of sas source file."
  (interactive)
  (save-excursion
    (if (re-search-forward "* *Local Variables: *;" nil t)
        ()
      (goto-char (point-max))
      (insert "

**  Local Variables:  ;
**  End:  ;
page ;
"))))


(if (file-exists-p sas-custom-file-name) (load sas-custom-file-name))

(provide 'sas)

(message "SAS mode updated 6/7/01 See C-hm for changes.")

  

;;  Change Log:
;;  6/7/01  Tweaked code to handle select/end statements
;;          Added run-sas-on-buffer
;;  4/25/01 Fixed bug in run-sas-on-region (the way sas-options-string is handled)
;;  10/11/00 Fixed removed "\" in "\ " of string in "skip-chars-forward" which
;;           doesn't work in emacs 20. 
;;  2/2/98  Fixed Syntax table to properly handle "\", changed ses-sentinel to
;;          use set-buffer, rather than switch-to-buffer (who knows why
;;          switch-to-buffer was used!?)
;;  3/1/97  Fixed up sas.texi 
;;  3/1/97  Fixed up font-lock mode.
;;  3/1/97  More work on indentation.  Probably still needs sas-indent-region
;;          function for efficiency and to patch up any remaining problems with
;;          sas-indent.
;;  3/1/97  Incorporated Riggle's electric-semi. 
;;  
;;  3/13/95 More work on indentation - seems to work better for continued parenthetical
;;             statements.  
;;          Changed keybindings to eliminate the use of \C-c letter.  
;;          Fixed syntax table to make "_" a word constituent. 
;;          Added variable sas-indent-continue-width to allow user to customize
;;             indentation of continuation lines and other blocks separately.
;;          Made sas-file-name buffer local (which it should have been all along).
;;
;;  2/14/95 More work on indentation - seems to work ok for continued parenthetical
;;          statements .
;;
;;  2/13/95 Fixed submit-sas so it shouldn't hang if comint-hilit is loaded.
;; 
;;  2/1/95  Modified indentation to work better with /* */ comments.
;;
;;  1/30/95  Added changes contributed by arossini@biostats.hmc.psu.edu -
;;            startup for auto-mode-alist
;;            XEmacs/Lucid emacs
;; 
;;  8/30/94  Added new optional arguments to sas-get-dataset in order to implement 
;;           some features in sas.data.el
;;  
;;  8/3/94   Fixed some bugs in indentation w.r.t comments introduced 6/27.
;;
;;  6/27/94  Fixed up sas-mode documentation.  
;;           Fixed indentation to work with macros.
;;           Fixed indentation to handle comments better.
;;  
;;  6/7/94 fixed some bugs with menus and buffers and such.  
;;  
;;  Modified 5/3/94 to add function sas-get-dataset and ancillary functions.
;;    (see documentation to sas-get-dataset for details.)
;;  
;;  Modified 5/2/94 to:
;;     1) Split log output and lst output into separate buffers in command
;;        `run-sas-on-region'.
;;     2) Add error check when sas-notify is set to alert the user of errors in
;;        the log file.
;;  
;;  Modified 4/29/94 to add variable `sas-program' whose value is the command which
;;    invokes SAS.  Defaults to \"sas\" but may be set to \"sas609\" for example,
;;    to use sas version 6.09.
;;  
;;  Modified 4/7/94 to add sas-notify.  If variable sas-notify is set, beep and
;;  display message that SAS run is done.
;;  
;;  Modified 1/26/94 to fix a bug in the indentation code.
;;  
;;  Modified 1/19/94 to add variable `sas-require-confirmation'.  If `t', you
;;  will be prompted for confirmation if you switch to a .lst or .log file which
;;  has changed on disk.  If `nil', emacs will automatically visit the disk
;;  version no questions asked and throw away any edits you have made in the
;;  buffer.
;;  
;;  Modified 1/13/94 to list key bindings in menu.
;;  
;;  Modified 1/12/94 to completely remove Riggle's code.  
;;  
;;  Modified 1/10/94 to add sas menu to menu bar.  It contains roughly the
;;  same set of functions available through the C-c bindings.
;;  


;;  To Do:
;;  
;;  1)  Add feature to automatically check log file for errors.
;;           Done 5/2/94 (at least in part)
;;
;;  2)  Fix indendation to work better with macros.
;;           Done 6/27/94
;;
;;  3)  Spiff up feature to print sas data sets.
;;
;;  4)  Add facility to generate templates for various procs.
;;
;;  5)  Add facility for completion/abbreviations for options, variable names,
;;      etc.
;;
;;  6)  Make point-and-click interface to sas datasets to run sas-get-dataset.
;;
;;  7)  Add flashing for do/end pairs. (See Riggle's code)

