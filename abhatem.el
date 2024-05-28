(use-package tera-mode
    :straight (:host github :repo "svavs/tera-mode" :files ("*.el"))
    :ensure t)

;; markdown stuff
(use-package markdown-mode
    :straight t
    :ensure t)

;; toml stuff
(use-package toml-mode
	:straight t
	:ensure t)
(treemacs-do-switch-workspace "abhatem.com")
(treemacs)
