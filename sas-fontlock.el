;;-*-emacs-lisp-*-
;;;  file name: sas-fontlock.el
;;;
;;;  Version 0.5 (goes with version 1.10 of sas.el, sas-hilit.el, see below)
;;               adapted for FSF emacs version 20
;;; 
;;;    sas-fontlock:  fontlock SAS programs.
;;;    Copyright (C) 1999 Tom Cook
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
;;;  Author: Tom Cook,
;;;          Dept. of Biostatistics
;;;          University of Wisconsin - Madison
;;;          Madison, WI 53706
;;;          cook@biostat.wisc.edu
;;;  Last change: 10/10/2000
;;;               2/2/99  (adapted to emacs 20)
;;; 
;;;  Contributions : Anthony Rossini (rossini@stat.sc.edu)

(require 'font-lock)
(make-face 'font-lock-dataset-face)
(make-face 'font-lock-macro-face)
(defvar sas-flag-long-names t)
(if (string< (substring emacs-version 0 2) "20")
    (progn (if (null font-lock-face-attributes) (font-lock-make-faces))
           (if (assoc 'font-lock-dataset-face font-lock-face-attributes) nil
             (setq font-lock-face-attributes
                   (cons '(font-lock-dataset-face "ForestGreen" nil t) font-lock-face-attributes)))
           (if (assoc 'font-lock-macro-face font-lock-face-attributes) nil
             (setq font-lock-face-attributes
                   (cons '(font-lock-macro-face "ForestGreen") font-lock-face-attributes))))
  (set-face-foreground 'font-lock-macro-face "ForestGreen")
  (set-face-foreground 'font-lock-dataset-face "ForestGreen")
  (set-face-bold-p 'font-lock-dataset-face t)
  (setq font-lock-dataset-face 'font-lock-dataset-face)
  (setq font-lock-macro-face 'font-lock-macro-face)
  )
           


(setq sas-mode-font-lock-keywords
  '(("%include [^;]*;" . font-lock-macro-face)
    ("&+[A-Za-z0-9_]*\\>"  (0 font-lock-variable-name-face t))
    ("^[ \t]*%let[ \t]+\\([A-Za-z0-9_]*\\)" . (1 font-lock-variable-name-face))
    ("\\<\\(array\\|length\\|var\\|class\\)\\>" . font-lock-keyword-face)
    ("^[ \t]*\\(proc\\|data\\|%macro\\|run\\|%mend\\|endsas\\)[ \n\t;]" . font-lock-function-name-face)
    ("\\<\\(retain\\|format\\|input\\|infile\\|by\\|set\\|merge\\|label\\|g?options\\|where\\|%?if\\|%?then\\|%?else\\|%?while\\|%?do\\|%?until\\|%?end\\|%let\\|%str\\|libname\\)\\>" 
     . (0 font-lock-keyword-face nil))
    ("^[ \t]*\\(infile\\|proc\\)\\>[ \t]+\\([a-z0-9_.]*\\)" (2 font-lock-dataset-face t))
    ("^[ \t]*%macro\\>[ \t]+\\([a-z0-9_.]*\\)" (1 font-lock-macro-face t))
    ("^[ \t]*\\(set\\|merge\\)[ \t]+[a-z0-9_.]*[ \t]*([^)]*)[ \t]*\\([^();]*\\)")
    ("^[ \t]*\\(set\\|merge\\)[ \t]+[a-z0-9_.]*[ \t]*([^)]*)[ \t]*[a-z0-9_.]*[ \t]*([^)]*)[ \t]*\\([^();]*\\)")
    ("%[a-z0-9_]*\\>" . font-lock-macro-face)
    ("\\b\\(data\\|library\\|out\\)\\> *=[ \t\n]*\\([a-z0-9_.]*\\)" (2 font-lock-dataset-face t))
    ("^ *\\*[^/][^;]*;"   . (0 font-lock-comment-face t))
    ("; *\\(\\*[^/][^;]*\\)"   . (1 font-lock-comment-face t))
    (sas-fontlock-comment-find-function   (0 font-lock-comment-face t))
    (sas-fontlock-multi-datasets (0 font-lock-dataset-face))
    ))
(if sas-flag-long-names
    (setq sas-mode-font-lock-keywords
          (append '(("[a-z0-9_][a-z0-9_][a-z0-9_][a-z0-9_][a-z0-9_][a-z0-9_][a-z0-9_][a-z0-9_][a-z0-9_]+"
     (0 font-lock-comment-face t))) sas-mode-font-lock-keywords)))

(defun sas-fontlock-comment-find-function (limit)
  "Locate /* */ style comments.  LIMIT is end of search."
  (let (start end)
    (if (re-search-forward "\\(/\\*\\)\\|\\(\\*/\\)" limit t)
        (cond ((match-string 1)
               (setq start (match-beginning 1))
               (setq end (or (re-search-forward "\\*/" limit 1) limit))
               (store-match-data (list start end))
               t)))))

(defun sas-fontlock-multi-datasets (limit)
  "Locate dataset names following data, set or merge.  Finds multiple datasets
but doesn't fontify parenthetical dataset options.  LIMIT is end of search."
  (let (end start)
    (catch 'sas-multi 
      (while (< (point) limit)
        (if (cond ((save-excursion
                     ;;(beginning-of-sas-statement 1)
                     (if (search-backward ";" (point-min) 1)
                         (forward-char 1))
                     (skip-chars-forward " \n\t\l")
                     (cond ((> (point) limit)
                            nil)
                           ((looking-at "data\\|set\\|merge")
                            (setq start (point))
                            (setq end (or (search-forward ";" limit 1) limit))
                            )))
           ;;;(cond ((search-forward "(" end t)
                      ;;;(backward-char 1)
               
                   (while (> (- (sas-how-many "(" start)
                                (sas-how-many ")" start)) 0)
                     (goto-char (scan-lists (point) -1 1))
                     (forward-list)
                     (if (> (point) limit)
                         (throw 'sas-multi nil))
                     )
                   (sas-fontlock-next-dataset-name end));;))
                  (t (catch 'sas-multi
                       (while (and (< (point) limit) (re-search-forward "^[ \t]*\\(data\\|set\\|merge\\)\\>" limit 1))
                         (save-excursion 
                           (setq end (search-forward ";" limit 1)))
                 ;;;(cond ((search-forward "(" end t)
                        ;;;(backward-char 1)
                         (if (sas-fontlock-next-dataset-name end)
                             (throw 'sas-multi t))))))
            (throw 'sas-multi t))))))
             
(defun sas-fontlock-next-dataset-name (limit)
  "Locate the next unfontified dataset name following data, set or merge.
LIMIT is end of search."
  (catch 'sas-next
    (let (start)
      (while (progn (skip-chars-forward " \t\n\f" limit) (< (point) limit))
        (cond ((and (looking-at "[_a-zA-Z]")
                    (null (memq 'font-lock-dataset-face (text-properties-at (point)))))
               (setq start (point))
               (skip-chars-forward "A-Za-z_0-9.")
               (store-match-data (list start (point)))
               (throw 'sas-next t)
               )
              (t (condition-case nil
                     (if (not (looking-at ";"))
                         (forward-sexp)
                       (forward-char) 
                       (skip-chars-forward "\ \t\n\f;)"))
                   (error (goto-char limit) nil))))))))
                   ;;; on error, bail out!
                   ;;(error (forward-char) (skip-chars-forward "\ \t\n\f;)") nil))))))))

(provide 'sas-fontlock)

