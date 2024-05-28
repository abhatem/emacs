;; https://www.reddit.com/r/emacs/comments/ejc1az/lspmode_select_python_interpreter_virtual/ for poetry venvs stuff 
(use-package poetry
	:straight t
	:ensure t)

;; csharp stuff
(setenv "FrameworkPathOverride" "/lib/mono/4.5")
(use-package csharp-mode
	:ensure t
	:init
	(defun my/csharp-mode-hook ()
		(setq-local lsp-auto-guess-root t)
		(lsp))
	(add-hook 'csharp-mode-hook #'my/csharp-mode-hook))

;; unity stuff
(straight-use-package
	'(unity :type git :host github :repo "elizagamedev/unity.el"))
(add-hook 'after-init-hook #'unity-mode)

;; for dap configuration
;; (use-package dap-python :straight t :ensure t)
;; (use-package dap-csharp :straight t :ensure t)

(setq lsp-csharp-solution-file "/home/abhatem/workspace/RoboLensLinux/RoboLensLinux.sln")
(setq csharp-ts-mode-indent-level 4)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "linux")

;; javascript indent width


(defun my/format-buffer-if-lsp-active ()
	"Formats the current buffer with lsp-format-buffer if lsp-mode is active."
	(when (and (bound-and-true-p lsp-mode) (functionp 'lsp-format-buffer))
		(lsp-format-buffer)))

(add-hook 'before-save-hook 'my/format-buffer-if-lsp-active)

;; latex stuff
(use-package auctex :straight t :ensure t)

;; debugging stuff
(use-package dap-mode
	:straight t
	:ensure t
	:config
	(dap-mode 1)
    (dap-ui-mode 1))


;; Use pyvenv package for managing Python virtual environments
(use-package pyvenv
	:straight t
	:ensure t
	:init
	(setenv "WORKON_HOME" "~/.pyenv/versions")
	:hook (python-mode . my-python-mode-hook)
	:config

	;; Custom Python mode hook
	(defun my-python-mode-hook ()
		;; Locating the virtual environment file (.python-version) in the directory tree
		;; and extracting its contents to use for setting up the virtual environment
		(let* ((root (locate-dominating-file
						 (or (buffer-file-name) default-directory)
						 ".python-version"))
				  (venv (and root
							(with-temp-buffer
								(insert-file-contents
									(expand-file-name ".python-version" root))
								(string-trim (buffer-string))))))
			;; If a virtual environment is found, enable pyvenv mode,
			;; activate the virtual environment and restart lsp workspace
			(when venv
				(pyvenv-mode 1)
				(pyvenv-workon venv)
				(lsp-restart-workspace)))))



(use-package dap-python)
(setq dap-python-debugger 'debugpy)



(treemacs-do-switch-workspace "thesis")


(treemacs)
