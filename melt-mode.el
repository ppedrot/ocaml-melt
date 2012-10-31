;;; melt-mode.el --- Support for the Melt programming language

;; Copyright (C) 2011-2012 Matthias Puech

;; Author: Matthias Puech <puech@cs.unibo.it>
;; Created: 24 Jul 2012
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((cl "23") (tuareg "2.0.6") (flyspell "23"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;   This is a major mode for Melt [1]. Melt allows you to program
;;   LaTeX documents using OCaml. It combines the power of LaTeX to
;;   produce pretty documents while using the power of OCaml to
;;   program your macros.
;;
;;  [1] : <https://forge.ocamlcore.org/projects/melt/>
;;
;;; Installation:
;;
;;   (require 'melt-mode)
;;
;;; Requirements:
;;
;;    * tuareg-mode
;;
;;; Code:


(defgroup melt-faces nil
  "Special faces for the Melt mode."
  :group 'melt)

(defface melt-text-face
 '((t nil))
  "Face description for text."
  :group 'melt-faces)

(defface melt-math-face
 '((t (:foreground "burlywood")))
  "Face description for math."
  :group 'melt-faces)

(defface melt-verbatim-face
 '((t (:foreground "lightsteelblue")))
  "Face description for verbatim."
  :group 'melt-faces)

(defconst melt-font-other-re
  "[^\\$\{\}A-Za-z_0-9\012\015\300-\326\330-\366\370-\377'\"(]+")

(defconst melt-font-other-quotation-re
  "[^\\\"\\$\012\015\<]")

(defconst melt-font-verbatim-open-re
  "\<\\(\:[a-zA-Z0-9\. \_]+\:\\)?\\([^\:]\\)")

(defconst melt-font-quotation-delim
  "\"\\|\\$")

(defconst melt-font-ident-re
  "[A-Za-z_\300-\326\330-\366\370-\377][A-Za-z_\300-\326\330-\366\370-\377'0-9]*")

(defconst melt-font-int-re
  "\\(0[xX][0-9A-Fa-f][0-9A-Fa-f_]*\\|0[oO][0-7][0-7_]*\\|0[bB][01][01_]*\\)[lLn]?")

(defconst melt-font-decimal-re
  "[0-9][0-9_]*\\([lLn]\\|\\.[0-9_]*\\)?\\([eE][+-]?[0-9][0-9_]*\\)?")

(defconst melt-font-ident-or-num-re
  (regexp-opt `(,melt-font-ident-re "\\|" ,melt-font-int-re "\\|" ,melt-font-decimal-re)))

(defconst melt-font-char-re
  "'\\(\015\012\\|[^\\']\\|\\(\\\\\\([\\'\"ntbr ]\\|[0-9][0-9][0-9]\\|x[0-9A-Fa-f][0-9A-Fa-f]\\)\\)\\)'")

(defconst melt-font-quote-newline-re
  "'\\(\015\012\\|[\012\015]\\)")

(defconst melt-font-other-comment-re
  "[^(*\"'\012\015]+")

(defconst melt-font-other-string-re
  "[^\\\"\012\015]")

(defconst melt-init-stack '((stack-code))
  "The initial state of the parser")

(defun melt-push-stack (symb matcher)
  (put-text-property (point) (1+ (point)) 'syntax-table
                     (string-to-syntax (concat "(" matcher)))
  (push symb stack))

(defun melt-pop-stack (matcher)
  (put-text-property (point) (1+ (point)) 'syntax-table
                     (string-to-syntax (concat ")" matcher)))
  (pop stack))

(defun melt-font-parse-token (stack)
  "Parses from point and go to next token. Returns the stack
   at the end."

  ;; at newlines, save the stack.
  (if (looking-at "\\(\015\012\\|[\012\015]\\)")
      (put-text-property (point) (1+ (point)) 'melt-parse-state stack))

  (case (caar stack)

    (stack-code
     (cond
      ((looking-at "\{") ; enter record
       (melt-push-stack '(stack-code) "}"))
      ((looking-at "\}") ; enter anti
       (melt-pop-stack "{"))
      ((looking-at melt-font-ident-or-num-re) nil) ; ident
      ((looking-at melt-font-char-re) ; char
       (put-text-property (point) (match-end 0) 'face font-lock-string-face)
       (melt-push-stack '(stack-char) "'")
       (goto-char (1- (match-end 0)))
       (melt-pop-stack "'"))
      ((looking-at melt-font-quote-newline-re) nil) ; \\\n
      ((looking-at "\\\\\"")   ; enter string
       (melt-push-stack '(stack-string) "\""))
      ((looking-at melt-font-quotation-delim)   ; enter quote
       (let ((delim (match-string 0))
             (prec-word (save-match-data
                          (save-excursion
                            (if (looking-back "\\(\\w+\\)\\s-*" 1 t)
                                (match-string 1)
                              "")))))
         (melt-push-stack `(stack-quotation ,delim ,prec-word) delim)))
      ((looking-at "(\\*") ; enter comment
       (melt-push-stack '(stack-comment) ")")
       ;; override the ( in < so that ppss know it's a comment
       (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "<")))
      ((looking-at melt-font-other-re) nil) ; any other character
      ((looking-at ".") nil))) ; catchall

    (stack-comment                 ; comment state
     (cond
      ((looking-at "(\\*")
       (melt-push-stack '(stack-comment) ")"))
      ((looking-at "\\*)")
       (forward-char)
       (melt-pop-stack "(")
       ;; override the ) in > so that ppss know it's a comment
       (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ">")))
      ((looking-at melt-font-other-comment-re) nil)
      ((looking-at ".") nil)))

    (stack-quotation                     ; quotation state
     (let ((open-symb (car (cdr (car stack)))))
       (cond
        ((looking-at melt-font-quotation-delim)   ; end of quotation
         (let ((delim (match-string 0)))
           (if (equal open-symb delim)
               (melt-pop-stack delim)
             (melt-push-stack `(stack-quotation ,delim "") delim))))
        ((looking-at "\\\\[\\\\\"\$\<\{\}]") nil); escaped quote
        ((looking-at "(\\*") ; enter comment
         (melt-push-stack '(stack-comment) ")")
         ;; override the ( in < so that ppss know it's a comment
         (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "<")))
        ((looking-at "\{") ; enter antiquot
         (melt-push-stack `(stack-code) "}"))
        ((and (equal open-symb "\"") (looking-at melt-font-verbatim-open-re)) ; begin verbatim
         (melt-push-stack `(stack-verbatim
                            ,(string-to-char (match-string 2))) ">"))
        ((looking-at melt-font-other-quotation-re) nil) ; any non-special char
        ((looking-at ".") nil))))

    (stack-string              ; string state
     (cond
      ((looking-at "\\\\\"")   ; end of string
       (forward-char)
       (melt-pop-stack "\\"))
      ((looking-at "\\\\[\"\\]") nil) ; escaped char
      ((looking-at melt-font-other-string-re) nil) ; any non-special char
      ((looking-at ".") nil)))

    (stack-verbatim
     (let ((verb-symb (car (cdr (car stack)))))
       (case verb-symb (?< (setq verb-symb ?>))) ; special case: '>>' closes '<<'
       (cond
        ((looking-at (concat (char-to-string verb-symb) ">")) ; end verbatim
         (forward-char)
         (melt-pop-stack "<"))
        ((looking-at (concat "[^" (char-to-string verb-symb) "]+")) nil)
        ((looking-at ".") nil))))

    (otherwise (looking-at "."))) ; unknow stack element

  ;; Go to the end of the last matched regexp (looking-at)
  (goto-char (match-end 0))
  ;; and return stack
  stack)

(defface melt-emph-face '((t (:slant italic)))
  "Face for emph" :group 'melt-faces)
(defface melt-alert-face '((t (:foreground "red")))
  "Face for alert" :group 'melt-faces)
(defface melt-sectioning-5-face '((t (:height 1.1 :weight bold)))
  "Face for sectioning 5":group 'melt-faces)
(defface melt-sectioning-4-face '((t (:height 1.1 :inherit 'melt-sectioning-5-face)))
  "Face for sectioning 4":group 'melt-faces)
(defface melt-sectioning-3-face '((t (:height 1.1 :inherit 'melt-sectioning-4-face)))
  "Face for sectioning 3":group 'melt-faces)
(defface melt-sectioning-2-face '((t (:height 1.1 :inherit 'melt-sectioning-3-face)))
  "Face for sectioning 2" :group 'melt-faces)
(defface melt-sectioning-1-face '((t (:height 1.1 :inherit 'melt-sectioning-2-face)))
  "Face for sectioning 1" :group 'melt-faces)

(defconst melt-special-face-alist
  '(("emph" . melt-emph-face)
    ("chapter" . melt-sectioning-1-face)
    ("section" . melt-sectioning-2-face)
    ("subsection" . melt-sectioning-3-face)
    ("subsubsection" . melt-sectioning-4-face)
    ("paragraph" . melt-sectioning-5-face)
    ("alert" . melt-alert-face)
    ))

(defun melt-fontify-token (s begin end)
  "Given a stack s, fontify uniformly the region between begin
   and end"

  (unless (equal (caar s) 'stack-code)
    (put-text-property begin end 'face
                       (case (caar s)
                         (stack-comment 'font-lock-comment-face)
                         (stack-string 'font-lock-string-face)
                         (stack-verbatim 'melt-verbatim-face)
                         (stack-quotation
                          (let ((delim (car (cdr (car s))))
                                (prec-word (car (cdr (cdr (car s))))))
                            ;; (message "prec-word %S" prec-word)
                            (or
                             (cdr (assoc prec-word melt-special-face-alist))
                             (cond ((equal delim "\"") 'melt-text-face)
                                   ((equal delim "$") 'melt-math-face)))))))))

(defun melt-font-parse (stack end)
  "Iterates parsing until end, starting with stack. Returns stack
   at end"

  (while (< (point) end)
    (let ((old-point (point))
          (old-stack stack))
      (setq stack (melt-font-parse-token stack))
      (assert (> (point) old-point))
      ;; apply font to the region just parsed
      (melt-fontify-token
       ;; if we parsed a closing paren, fontify with old-stack. This
       ;; is a hack to fontify closing delimiters correctly
       (if (= (syntax-class (syntax-after (1- (point)))) 5) old-stack stack)
       old-point (point))
      ))

  stack)

(defun melt-font-parse-region (beg end)
  "Parses and fontifies from beg (actually from the first point
   before where a stack was saved) to end. Returns stack at end."

  (let (start-at state)
    (setq start-at
          (or (and (> beg (point-min))
                   (get-text-property (1- beg) 'melt-parse-state)
                   beg)
              (previous-single-property-change beg 'melt-parse-state)
              (point-min)))
    ;; (message "melt-font-parse-region %S - %S = %S" start-at end (- end start-at))
    (setq state (or (and (> start-at (point-min))
                         (get-text-property (1- start-at) 'melt-parse-state))
                    melt-init-stack))

    (goto-char start-at)
    (melt-font-parse state end)))

(defun melt-fontify-region (begin end &optional verbose)
  ;; (message "melt-fontify-region %S - %S= %S" begin end (- end begin))
  ;; first pass: keywords
  (font-lock-default-fontify-region begin end verbose)
  ;; second pass: comments, strings, chars, text, math, verbatim
  (melt-font-parse-region begin end)
)

(defun melt-indent-line (&optional p)
  "Indentation function: rely on tuareg-mode, except for
   quotations"

  (interactive "*p")
  (let ((stack
         (save-excursion (melt-font-parse-region
                          (line-beginning-position)
                          (line-beginning-position)))))
    (case (caar stack)
      (stack-code (tuareg-indent-command p))
      (stack-comment (tuareg-indent-command p))
      (stack-string (tuareg-indent-command p))
      ;; we indent quotations by the length of the stack
      (stack-quotation (save-excursion (indent-line-to (- (length stack) 2))))
      (stack-verbatim ())
      )))

;; We redefine this function so as to get rid of quotation ("<<
;; >>") handling by tuareg: we do it ourselves here.
(defun melt-syntax-propertize (start end)
  (goto-char start)
  (funcall
     (syntax-propertize-rules
      ("\\_<\\('\\)\\(?:[^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
       (1 "\"") (2 "\"")))
     start end))

(defun melt-font-set-font-lock ()
  "Declares keywords and fontify function. To declare as a hook
   to melt-mode"

  (set (make-local-variable 'tuareg-font-lock-keywords)
        (append tuareg-font-lock-keywords
                '(("\\w+" 0 font-lock-variable-name-face keep)
                  ("##\\w+" 0 font-lock-preprocessor-face t))))
  (set (make-local-variable 'syntax-propertize-function)
       'melt-syntax-propertize)
  (setq font-lock-defaults
        (append font-lock-defaults
                '((font-lock-fontify-region-function . melt-fontify-region)
                  (font-lock-extra-managed-props melt-parse-state syntax-table)))))

(defun melt-wrap-anti-text (beg end text)
  (goto-char end)
  (insert "\"}")
  (goto-char beg)
  (insert "{" text " \""))

(defun melt-wrap-anti-math (beg end text)
  (goto-char end)
  (insert "$}")
  (goto-char beg)
  (insert "{" text " $"))

(defun melt-insert-anti-text (beg end text)
  "Turns the argument region into a quoted antiquotation and place
   the cursor before the quote"
  (interactive
   (let ((text (read-string "Code: " nil nil melt-anti-history)))
     (add-to-history 'melt-anti-history text)
     (if (region-active-p)
         (list (region-beginning) (region-end) text)
       (list (point) (point)))))
  (melt-wrap-anti-text beg end text))

(defun melt-insert-anti-math (beg end text)
  "Turns the argument region into a quoted math antiquotation and place
   the cursor before the quote"
  (interactive
   (let ((text (read-string "Code: " nil nil melt-anti-history)))
     (add-to-history 'melt-anti-history text)
     (if (region-active-p)
         (list (region-beginning) (region-end) text)
       (list (point) (point)))))
  (melt-wrap-anti-math beg end text))

(defun melt-wrap-anti (text)
  `(lambda (beg end)
     (interactive
      (if (region-active-p)
          (list (region-beginning) (region-end))
        (list (point) (point))))
     (melt-wrap-anti-text beg end ,text)))

(defun melt-unwrap-anti ()
  (interactive)
  (atomic-change-group
    (condition-case nil
        (progn (save-excursion (re-search-forward "[\"$] *\}"))
               (delete-region (match-beginning 0) (match-end 0))
               (save-excursion (re-search-backward "\{ *\\w* *[\"$]"))
               (delete-region (match-beginning 0) (match-end 0)))
      (search-failed (error "No antiquotations found around point")))))

(defun melt-set-map ()
  (define-key melt-mode-map [(control ?{)] 'melt-insert-anti-text)
  (define-key melt-mode-map [(control meta ?{)] 'melt-insert-anti-math)
  (define-key melt-mode-map "\C-c\C-f\C-b" (melt-wrap-anti "textbf"))
  (define-key melt-mode-map "\C-c\C-f\C-c" (melt-wrap-anti "textsc"))
  (define-key melt-mode-map "\C-c\C-f\C-d" 'melt-unwrap-anti)
  (define-key melt-mode-map "\C-c\C-f\C-e" (melt-wrap-anti "emph"))
  (define-key melt-mode-map "\C-c\C-f\C-f" (melt-wrap-anti "textsf"))
  (define-key melt-mode-map "\C-c\C-f\C-n" (melt-wrap-anti "textnormal"))
  (define-key melt-mode-map "\C-c\C-f\C-r" (melt-wrap-anti "textrm"))
  (define-key melt-mode-map "\C-c\C-f\C-s" (melt-wrap-anti "textsl"))
  (define-key melt-mode-map "\C-c\C-f\C-t" (melt-wrap-anti "texttt"))
  (define-key melt-mode-map "\C-c\C-f\C-u" (melt-wrap-anti "textup"))
  (define-key melt-mode-map "\C-c\C-e" 'melt-insert-anti)
)

;;;###autoload
(define-derived-mode melt-mode tuareg-mode "Melt"
  "Major mode for editing Melt file"

  (require 'cl)
  ;; to avoid emacs from escaping \" in strings
  (modify-syntax-entry ?\" "." melt-mode-syntax-table)
  (modify-syntax-entry ?\\ "." melt-mode-syntax-table)
  (modify-syntax-entry ?$ "$" melt-mode-syntax-table)
  (add-hook 'melt-mode-hook 'melt-set-map)
  (add-hook 'melt-mode-hook 'melt-font-set-font-lock)
  (defvar melt-anti-history nil)
  ;; make all text flyspell-able
  (require 'flyspell)
  (add-to-list 'flyspell-prog-text-faces 'melt-text-face)
  ;; indentation
  (set (make-local-variable 'indent-line-function) 'melt-indent-line)
  (run-hooks 'melt-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlt\\'" . melt-mode))

(provide 'melt-mode)

;;; melt-mode.el ends here
