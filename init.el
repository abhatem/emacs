(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
	  (expand-file-name
              "straight/repos/straight.el/bootstrap.el"
              (or (bound-and-true-p straight-base-dir)
		  user-emacs-directory)))
	 (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
	(with-current-buffer
            (url-retrieve-synchronously
		"https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
		'silent 'inhibit-cookies)
	    (goto-char (point-max))
	    (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))


(custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(custom-enabled-themes '(tango-dark))
    '(safe-local-variable-values
	 '((lsp-csharp-solution-file . "/home/abhatem/workspace/RoboLensLinux/RoboLensLinux.sln"))))
(custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    )


(straight-use-package 'helm)
(straight-use-package 'helm-xref)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-buffers-list)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(global-set-key (kbd "C-'") #'yas/expand)



;; snippet stuff
(use-package yasnippet :straight t)


;; treemacs stuff
(use-package treemacs
    :straight t
    :ensure t)

(use-package treemacs-projectile :straight t :ensure t)
(treemacs-git-mode 'deferred)

(use-package rust-mode :straight t :ensure t)

(use-package lsp-mode
    :straight t
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
              (python-mode . lsp)

              ;; if you want which-key integration
              (lsp-mode . lsp-enable-which-key-integration))
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
              (csharp-mode . lsp)

              ;; if you want which-key integration
              (lsp-mode . lsp-enable-which-key-integration))

    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
			  (rust-mode . lsp)

			  ;; if you want which-key integration
			  (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp
    :bind-keymap
    ("C-c l" . lsp-command-map)
    :custom
    (lsp-keymap-prefix "C-c l"))

;; optionally
(use-package lsp-ui :straight t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :straight t :commands helm-lsp-workspace-symbol)
;; (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;; ;; if you are ivy user
;; (use-package lsp-ivy :straight t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :straight t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;; optional if you want which-key integration
(use-package which-key
    :straight t
    :config
    (which-key-mode))

(which-key-mode)


(setq gc-cons-threshold (* 100 1024 1024)
    read-process-output-max (* 1024 1024)
    treemacs-space-between-root-nodes nil
    company-idle-delay 0.0
    company-minimum-prefix-length 1
    lsp-idle-delay 0.1)  ;; clangd is fast

;; magit stuff
(use-package magit :straight t :ensure t
    :bind ("C-x g" . magit-status))


(with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (require 'dap-cpptools)
    (yas-global-mode))

;; for cool icons when completing
(use-package company-box
    :straight t
    :hook (company-mode . company-box-mode))

;; copilot
(use-package copilot
    :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
    :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)


;; cursor stuff
(setq-default cursor-type 'bar)

;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; projectile stuff
(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; helm projectile
(use-package helm-projectile
    :straight t
    :ensure t
    :config
    (helm-projectile-on))


;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))


;; git lens stuff
(use-package vc-msg
    :straight (vc-msg :type git :host github :repo "redguardtoo/vc-msg"))

;; docker stuff
(use-package dockerfile-mode :straight t :ensure t)
(use-package docker :straight t :ensure t)
;; docker-compose stuff
(use-package docker-compose-mode :straight t :ensure t)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; backup stuff (aut-save files are annoying)
(setq backup-by-copying t      ; don't clobber symlinks
    backup-directory-alist '(("." . "~/.emacs-saves"))    ; don't litter my fs tree
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2
    version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
    `((".*" "~/.emacs-saves/" t)))

;; cool dashboard
(use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "abhatem emacs")
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)

(setq dashboard-startup-banner "~/emacs/gears-5908_256.gif")


(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))



(add-hook `emacs-lisp-mode-hook
    (lambda ()
	(company-mode)))

;; indent 4 spaces width (to avoid erros)
(setq lisp-indent-offset 4) 


;; helm icons
(when (display-graphic-p)
    (use-package helm-icons :straight t :ensure t)
    (helm-icons-enable)
    )
;; (use-package all-the-icons
;; 	:straight t
;; 	:ensure t)


;; golden ratio
(use-package golden-ratio
    :straight t
    :ensure t
    :config
    (golden-ratio-mode 1))

(defun my/thesis()
    "load thesis stuff"
    (interactive)
    (load-file "~/emacs/thesis.el")
    )

(defun my/ml ()
    "load up machine learning related stuff"
    (interactive)
    (load-file "~/emacs/ml.el")
    )

(defun my/load-init ()
    "Switch to previously open buffer."
    (interactive)
    (load-file "~/.emacs")
    )

(defun my/cruncher ()
	"load up cruncher stuff"
	(interactive)
	(load-file "~/emacs/cruncher.el")
	)

;; open dot file
(defun my/open-init ()
    "open dot file"
    (interactive)
    (find-file "~/.emacs")
    )

(server-start)
(windmove-default-keybindings)
