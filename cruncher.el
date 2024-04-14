(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
;; cmake font lock
;; (use-package cmake-font-lock :straight t :ensure t)
;;for dap configuration https://emacs-lsp.github.io/dap-mode/page/configuration/
;; (use-package dap-cpptools :straight t :ensure t)

(defun my/format-buffer-if-lsp-active ()
  "Formats the current buffer with lsp-format-buffer if lsp-mode is active."
  (when (and (bound-and-true-p lsp-mode) (functionp 'lsp-format-buffer))
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'my/format-buffer-if-lsp-active)


(treemacs-do-switch-workspace "cruncher")
(treemacs)
