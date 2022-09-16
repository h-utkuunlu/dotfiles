;;; package --- Utku Emacs configuration

;;; Commentary:
;; Evolving over time.  Provided as-is

;;; Code:

;; Hide top bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Don't show startup
(setq inhibit-startup-screen t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Show line / column numbers
(global-linum-mode 1)
(column-number-mode 1)

;; Wrap line around
(global-visual-line-mode 1)

;; Show empty lines
(set-default 'indicate-empty-lines t)

;; Use clipboard
(setq select-enable-clipboard t)

;; Set theme
(load-theme 'misterioso t)

;; Display 88-col fill indicator (following python black)
(set-fill-column 88)
(global-display-fill-column-indicator-mode)

;; Suppress comp warnings
(setq warning-suppress-types '((comp)))

;; Don't clutter up directories with files~ and #files#
;; Backups saved to emacs.d/backup, autosaves to emacs.d/autosave
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      delete-by-moving-to-trash t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200
      version-control t
      )

;; Install & configure straight-use-package
;; (https://github.com/radian-software/straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
       user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Make use-package use straight by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; Helm mode from LSP Mode tutorials. Makes searches / navigation much easier
;; Display helm output at the center, in a separate frame
;; https://www.reddit.com/r/emacs/comments/jj269n/display_helm_frames_in_the_center_of_emacs/
(use-package helm
  :config
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini)
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package helm-xref)

;; Highlights the current line
(use-package hl-line
  :init
  (global-hl-line-mode 1)
  :custom-face
  (hl-line ((t (:background "#555555")))))

;; Magit - Git interface
(use-package magit)

;; Comp(lete)any package for completions
(use-package company
  :init
  (global-company-mode 1)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1))

;; Yasnippet provides easy insertion of boilerplate snippets
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/dotfiles/emacs/yasnippets")))

;; Support for YAML syntax
(use-package yaml-mode
  :hook
  ((yaml-mode . (lambda ()
		  (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

;; Support for Dockerfile syntax
(use-package dockerfile-mode)

;; Which-key shows available commands after a key press
(use-package which-key
  :config
  (which-key-mode 1))

;; Language Server Protocol - comprehending the code & showing potential errors
(use-package lsp-mode
  :hook (python-mode c-mode c++-mode)
  :config
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=2" "-background-index"))
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-idle-delay 0.1)
  (setq read-process-output-max (* 1024 1024)) ;; Emacs default (4K) too low for LSP
  (setq gc-cons-threshold (* 100 1024 1024)) ;; Emacs default low for LSP
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Syntax checking for any code
(use-package flycheck
  :hook ((python-mode . flycheck-mode)
	 (c-mode . flycheck-mode)
	 (c++-mode . flycheck-mode)))

;; Show file structure
(use-package treemacs
  :config
  (setq treemacs-space-between-root-nodes nil))

;; Consider adding projectile

;; Use google c-style
(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

;; Clang Format to format c/c++ code
(use-package clang-format
  :init
  (setq clang-format-style "file")
  (setq clang-format-fallback-style "llvm")
  :config
  (add-hook 'c-common-mode-hook
	    (lambda ()
	      (add-hook (make-local-variable 'before-save-hook)
			'clang-format-buffer))))

;; Python LSP interface
(use-package lsp-jedi)

;; Python Black formatter
(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode)
  :config
  (setq python-black-command "/usr/local/bin/black"))
  ;; (setq python-black-extra-args '("--line-length=100")))

;; Allows opening terminals in the folder associated with buffer
(use-package terminal-here
  :bind ("C-c t" . terminal-here-launch))
