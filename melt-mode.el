(require 'cl)


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

(defconst melt-font-lock-keywords
  (append caml-font-lock-keywords
          (list
           '("\\w" 0 font-lock-variable-name-face keep))))

(defconst melt-font-other-re
  "[^\\$\}A-Za-z_0-9\012\015\300-\326\330-\366\370-\377'\"(]+"
)

(defconst melt-font-other-quotation-re
  "[^\\\"\\$\012\015\<]"
)

(defconst melt-font-verbatim-open-re
  "\<\\(\:[a-zA-Z0-9\. \_]+\:\\)?\\([^\:]\\)"
)

(defconst init-stack '((stack-code)))

(defun melt-font-propertize (stack end)
  (let ((continue t))
    (while (and continue (< (point) end))
      ;; (message "annot @ %d (stack:%S)" (point) stack)
      (case (caar stack)

        (stack-code
         (cond
          ((caml-font-looking-at "\}") ; enter anti
           (setq stack (cdr stack))
           (put-text-property (point) (match-end 0)
                              'syntax-table (string-to-syntax ")"))
           (goto-char (match-end 0)))
          ((caml-font-looking-at caml-font-ident-or-num-re) ; ident
           (goto-char (match-end 0)))
          ((caml-font-looking-at caml-font-char-re) ; char
           (put-text-property (point) (1+ (point))
                              'syntax-table (string-to-syntax "("))
           (put-text-property (1- (match-end 0)) (match-end 0)
                              'syntax-table (string-to-syntax ")"))
           (goto-char (match-end 0)))
          ((caml-font-looking-at caml-font-quote-newline-re) ; \\\n
           (goto-char (match-end 0)))
          ((caml-font-looking-at "\\\\\"")   ; enter string
           (put-text-property (point) (1+ (point))
                              'syntax-table (string-to-syntax "("))
           (goto-char (match-end 0))
           (setq stack (cons '(stack-string) stack)))
          ((caml-font-looking-at "\"")   ; enter quote "
           (put-text-property (point) (1+ (point))
                              'syntax-table (string-to-syntax "("))
           (goto-char (match-end 0))
           (setq stack (cons '(stack-quotation "\"") stack)))
          ((caml-font-looking-at "\\$")   ; enter quote $
           (put-text-property (point) (1+ (point))
                              'syntax-table (string-to-syntax "("))
           (goto-char (match-end 0))
           (setq stack (cons '(stack-quotation "$") stack)))
          ((caml-font-looking-at "(\\*") ; enter comment
           (put-text-property (point) (1+ (point))
                              'syntax-table (string-to-syntax "("))
           (goto-char (match-end 0))
           (setq stack (cons '(stack-comment) stack)))
          ((looking-at caml-font-newline-re) ; \n
           (goto-char (match-end 0))
           (setq continue (caml-font-put-state (match-end 0) stack)))
          ((caml-font-looking-at melt-font-other-re) ;any other character
           (goto-char (match-end 0)))
          (t
           (remove-text-properties (point) (1+ (point))
                                   '(syntax-table nil caml-font-state nil))
           (goto-char (1+ (point))))))

        (stack-comment                 ; comment state
         (cond
          ((caml-font-looking-at "(\\*")
           (put-text-property (point) (1+ (point))
                              'syntax-table (string-to-syntax "("))
           (setq stack (cons '(stack-comment) stack))
           (goto-char (match-end 0)))
          ((caml-font-looking-at "\\*)")
           (goto-char (match-end 0))
           (setq stack (cdr stack))
           (put-text-property (1- (point)) (point)
                              'syntax-table (string-to-syntax ")")))
          ((looking-at caml-font-newline-re)
           (goto-char (match-end 0))
           (setq continue (caml-font-put-state (match-end 0) stack)))
          ((caml-font-looking-at caml-font-other-comment-re)
           (goto-char (match-end 0)))
          (t
           (remove-text-properties (point) (1+ (point))
                                   '(syntax-table nil caml-font-state nil))
           (goto-char (1+ (point))))))

        (stack-quotation                     ; quotation state
         (let ((open-symb (car (cdr (car stack)))))
           (cond
            ((caml-font-looking-at "\"\\|\\$")   ; end of quotation
             (goto-char (match-end 0))
             (if (equal open-symb (match-string 0))
                 (progn
                   (setq stack (cdr stack))
                   (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ")")))
               (progn
                 (setq stack (cons `(stack-quotation ,(match-string 0)) stack))
                 (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax "(")))))
            ((caml-font-looking-at "\\\\[\"\$\<\{\}]") ; escaped quote
             (goto-char (match-end 0)))
            ((caml-font-looking-at "\{") ; enter antiquot
             (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "("))
             (setq stack (cons '(stack-code) stack))
             (goto-char (match-end 0)))
            ((and (equal open-symb "\"") (caml-font-looking-at melt-font-verbatim-open-re)) ; begin verbatim
             (setq stack (cons
                          `(stack-verbatim ,(string-to-char (match-string 2))) stack))
             (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "("))
             (goto-char (match-end 0)))
            ((looking-at caml-font-newline-re) ; newline: save state
             (goto-char (match-end 0))
             (setq continue (caml-font-put-state (match-end 0) stack)))
            ((caml-font-looking-at melt-font-other-quotation-re) ; any non-special char
             (goto-char (match-end 0)))
            (t                             ; unknown: remove annotation
             (remove-text-properties (point) (1+ (point))
                                     '(syntax-table nil caml-font-state nil))
             (goto-char (1+ (point)))))))

        (stack-string                     ; string state
         (cond
          ((caml-font-looking-at "\\\\\"")   ; end of string
           (goto-char (match-end 0))
           (put-text-property (1- (point)) (point)
                                'syntax-table (string-to-syntax ")"))
           (setq stack (cdr stack)))
          ((caml-font-looking-at "\\\\[\"\\]") ; escaped quote
           (goto-char (match-end 0)))
          ((looking-at caml-font-newline-re) ; newline: save state
           (goto-char (match-end 0))
           (setq continue (caml-font-put-state (match-end 0) stack)))
          ((caml-font-looking-at caml-font-other-string-re) ; any non-special char
           (goto-char (match-end 0)))
          (t                             ; unknown: remove annotation
           (remove-text-properties (point) (1+ (point))
                                   '(syntax-table nil caml-font-state nil))
           (goto-char (1+ (point))))))

        (stack-verbatim
         (let ((verb-symb (car (cdr (car stack)))))
           (case verb-symb
             (?< (setq verb-symb ?>)))
           (cond
            ((caml-font-looking-at (concat (char-to-string verb-symb) ">")) ; end verbatim
             (goto-char (match-end 0))
             (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ")"))
             (setq stack (cdr stack)))
            (t                             ; unknown: remove annotation
             (remove-text-properties (point) (1+ (point))
                                     '(syntax-table nil caml-font-state nil))
             (goto-char (1+ (point)))))))

        (otherwise
         (remove-text-properties (point) (point-max) '(caml-font-state nil))
         (setq continue nil)
         (goto-char (1+ (point))))))))

(defun melt-font-extend-after-change (beg end &optional old-len)
  (save-excursion
    (save-match-data
      (let ((caml-font-modified (buffer-modified-p))
            start-at
            state)
        (remove-text-properties beg end '(syntax-table nil caml-font-state nil))
        (setq start-at
              (or (and (> beg (point-min))
                       (get-text-property (1- beg) 'caml-font-state)
                       beg)
                  (previous-single-property-change beg 'caml-font-state)
                  (point-min)))
        (setq state (or (and (> start-at (point-min))
                             (get-text-property (1- start-at) 'caml-font-state))
                        init-stack))
        (goto-char start-at)
        ;; (message "propertize %d-%d = %d (stack: %S)" start-at
        ;;          end (- end start-at) state)
        (melt-font-propertize state end)
        (restore-buffer-modified-p caml-font-modified))))
)

(defun melt-delim-char-to-font (c)
  (case c
    (?\" 'melt-text-face)               ;text
    (?< 'melt-verbatim-face)            ;verbatim
    (?$ 'melt-math-face)                ;math
    (?' 'font-lock-string-face)         ;char
    (?\\ 'font-lock-string-face)        ;string
    (?\( 'font-lock-comment-face)       ;comment
    )
  )

(defun melt-fontify-region (begin end &optional verbose)
  (font-lock-default-fontify-region begin end verbose)
  (melt-font-extend-after-change begin end)
;  (message "fontify %d-%d = %d" begin end (- end begin))
  (while (< begin end)
    (let ((c (nth 1 (syntax-ppss begin))))
      (unless c (setq c 0))
      (let ((f (melt-delim-char-to-font (char-after c))))
        (cond
         ((= (syntax-class (syntax-after begin)) 4) ;it's an open delimiter
          (put-text-property begin (1+ begin) 'face
                             (melt-delim-char-to-font (char-after begin))))
         (f (put-text-property begin (1+ begin) 'face f))) ;inside f
        (setq begin (1+ begin))))))

(progn
  (setq elp-function-list
        '(melt-fontify-region melt-font-extend-after-change
                              melt-font-propertize
  font-lock-default-fontify-region syntax-ppss))
  (elp-instrument-list))

(defun melt-font-syntactic-face (s)
  (if (nth 4 s) font-lock-comment-face))

(defconst melt-font-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?' "w" tbl)
    (modify-syntax-entry ?_ "w" tbl)
    (modify-syntax-entry ?\" "." tbl)
    (modify-syntax-entry ?( "." tbl)
    (modify-syntax-entry ?) "." tbl)
    (modify-syntax-entry ?\{ "." tbl)
    (modify-syntax-entry ?\} "." tbl)
    (modify-syntax-entry ?[ "." tbl)
    (modify-syntax-entry ?] "." tbl)
    (modify-syntax-entry ?\\ "." tbl)
    (let ((i 192))
      (while (< i 256)
        (or (= i 215) (= i 247) (modify-syntax-entry i "w" tbl))
        (setq i (1+ i))))
    tbl))

;; font-lock commands are similar for caml-mode and inferior-caml-mode
(defun melt-font-set-font-lock ()
  (setq parse-sexp-lookup-properties t)
  (setq font-lock-defaults
        (list
         'melt-font-lock-keywords  ; keywords
         nil  ; keywords-only
         nil  ; case-fold
         nil  ; syntax-alist
         nil  ; syntax-begin
         (cons 'font-lock-syntax-table melt-font-syntax-table)
         '(font-lock-extend-after-change-region-function
           . nil)
         '(font-lock-syntactic-face-function
           . melt-font-syntactic-face)
         '(font-lock-fontify-region-function
           . melt-fontify-region)
         '(font-lock-extra-managed-props . (caml-font-state syntax-table))
         ))
)

(define-derived-mode melt-mode caml-mode "Melt"
  "Major mode for editing Melt file"
  (set-syntax-table melt-font-syntax-table)
  (remove-hook 'caml-mode-hook 'caml-font-set-font-lock)
  (add-hook 'melt-mode-hook 'melt-font-set-font-lock)
  (run-hooks 'melt-mode-hook)
  (add-to-list 'flyspell-prog-text-faces 'melt-text-face)
)

(provide 'melt-mode)
