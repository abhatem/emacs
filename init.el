(setq package-enable-at-startup nil)
;; compute startup times
(setq use-package-compute-statistics t)

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

;; use all the icons
(use-package all-the-icons :straight t :ensure t)

;; disable menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; for doom themes
;; (use-package doom-themes
;;     :straight t
;;     :ensure t
;;     :config
;;     ;; Global settings (defaults)
;;     (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;     (load-theme 'doom-one t)

;;     ;; Enable flashing mode-line on errors
;;     (doom-themes-visual-bell-config)
;;     ;; Enable custom neotree theme (all-the-icons must be installed!)
;;     (doom-themes-neotree-config)
;;     ;; or for treemacs users
;;     ;; (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;     ;; (doom-themes-treemacs-config)
;;     ;; Corrects (and improves) org-mode's native fontification.
;;     (doom-themes-org-config))

;; (load-theme 'doom-material t)

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

;; (use-package rust-mode :straight t :ensure t)
(use-package rustic
    :straight t
    :ensure t
    :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
    :config
    ;; uncomment for less flashiness
    ;; (setq lsp-eldoc-hook nil)
    ;; (setq lsp-enable-symbol-highlighting nil)
    ;; (setq lsp-signature-auto-activate nil)

    ;; comment to disable rustfmt on save
    (setq rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
    )

(defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
	(setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))




;; flycheck stuff
(use-package flycheck :straight t :ensure t
    :init (global-flycheck-mode))

(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)

(use-package lsp-mode
    :straight t
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
              ;; (python-mode . lsp)

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
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :custom
    (lsp-keymap-prefix "C-c l"))

;; optionally
(use-package lsp-ui
    :straight t
    :ensure
    :commands lsp-ui-mode
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil))

;; if you are helm user
(use-package helm-lsp :straight t :commands helm-lsp-workspace-symbol)
;; (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(global-set-key (kbd "C-c s") #'helm-lsp-workspace-symbol)

(use-package lsp-pyright
    :straight t
    :ensure t
    :hook (python-mode . (lambda ()
                             (require 'lsp-pyright)
                             (lsp))))  ; or lsp-deferred


;; ;; if you are ivy user
;; (use-package lsp-ivy :straight t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :straight t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; lsp-python-ms
;; (use-package lsp-python-ms
;;     :straight t
;;     :ensure t
;;     :hook (python-mode . (lambda ()
;; 			     (require 'lsp-python-ms)
;; 			     (lsp)))
;;     :config
;;     (setq lsp-python-ms-auto-install-server t)
;;     (setq ls-python-ms-executable (executable-find "python-language-server"))
;;     )
;; (setq lsp-python-ms-dir "/home/abhatem/.emacs.d/straight/build/lsp-python-ms/mspyls")
;; (setq lsp-python-ms-executable "/home/abhatem/.emacs.d/straight/build/lsp-python-ms/mspyls/Microsoft.Python.LanguageServer")
;; (setq lsp-python-ms-extra-paths '("/home/abhatem/.emacs.d/straight/build/lsp-python-ms/mspyls"))
;; (setq lsp-python-ms-python-executable-cmd "python3")
;; (setq lsp-python-ms-python-executable-cmd))

;; debugging python
(use-package dap-mode
    :after lsp-mode
    :commands dap-debug
    :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
    :config
    (require 'dap-python)
    (setq dap-python-debugger 'debugpy)
    (defun dap-python--pyenv-executable-find (command)
	(with-venv (executable-find "python")))

    (add-hook 'dap-stopped-hook
        (lambda (arg) (call-interactively #'dap-hydra))))


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
;; if command line args < 2 then open dashboard
;; (if (< (length command-line-args) 2)
;; 	(setq inhibit-startup-screen t))

(use-package dashboard
    :ensure t
    :if (< (length command-line-args) 2)
    :config
    (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "abhatem emacs")
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)

(setq dashboard-startup-banner "~/emacs/gears-5908_256.gif")


(if (< (length command-line-args) 2)
    (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; (setq dashboard-set-heading-icons t)
;; (setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-projects-backend 'projectile)
(setq dashboard-items '(
                           ;; (bookmarks . 5)
                           (projects  . 10)
                           (agenda    . 5)
                           (registers . 5)))

(setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package


;; react stuff
(use-package rjsx-mode :straight t :ensure t)

;; typescript stuff
(use-package typescript-mode :straight t :ensure t)

(use-package json-mode
    :straight t
	:ensure t)


;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)



(add-hook `emacs-lisp-mode-hook
    (lambda ()
	(company-mode)))

;; indent 4 spaces width (to avoid erros)
(setq lisp-indent-offset 4)
;; c indent 4 spaces width
(setq c-basic-offset 4)


;; helm icons
(when (display-graphic-p)
    (use-package helm-icons :straight t :ensure t)
    (helm-icons-enable)
    )
;; (use-package all-the-icons
;; 	:straight t
;; 	:ensure t)


;; for editing dir locals using projectile
 (define-skeleton projectile-skel-dir-locals
   "Insert a .dir-locals.el template."
   nil
   "((nil . ("
-  ("" '(projectile-skel-variable-cons) \n)
+  ("Value: "
+   "("
+   (let ((var-name (projectile-read-variable)))
+                   (if (string-empty-p var-name)
+                       ; Stop the sub-skeleton iteration on empty variable name.
+                       (signal 'quit t)
+                     var-name))
+   " . " str ")" \n)
   resume:
   ")))")


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

(defun my/abhatem ()
    "load up blog stuff"
    (interactive)
    (load-file "~/emacs/abhatem.el")
    )

;; open dot file
(defun my/open-init ()
    "open dot file"
    (interactive)
    (find-file "~/.emacs")
    )

(windmove-default-keybindings)

;; read .dir-locals.el from parent directories
;; (defun apply-dir-locals-from-parent-dirs ()
;;   "Apply .dir-locals.el from current directory up to the root."
;;   (let ((dir (expand-file-name default-directory)))
;;     (while (and dir (not (string-equal dir "/")))
;;       (let ((parent (file-name-directory (directory-file-name dir))))
;;         (when (and parent (not (equal parent dir)))
;;           (let ((locals-file (expand-file-name ".dir-locals.el" parent)))
;;             (when (file-exists-p locals-file)
;;               (message "Loading dir-locals from: %s" locals-file)
;;               (load-file locals-file))))
;;         (setq dir parent)))))

;; ;; Add the custom function to the find-file-hook
;; (add-hook 'find-file-hook 'apply-dir-locals-from-parent-dirs)


