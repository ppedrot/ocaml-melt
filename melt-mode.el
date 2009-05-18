(defvar melt-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.mlt\\'" . melt-mode))

(defun melt-mode ()
  "Major mode for editing Melt file"
  (tuareg-mode)
  (run-hooks 'melt-mode-hook)
)

(add-hook 'melt-mode-hook
      '(lambda ()
          ; cheap disactivation of literal and comments
         (defun tuareg-in-literal-or-comment () (cons nil nil))
         ))