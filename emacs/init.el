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

;; Org mode
(use-package org
  :hook (org-mode . auto-fill-mode)
  :config
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files (list "~/cloud/org/agenda/work.org"
			       "~/cloud/org/agenda/home.org")))

;; Bibtex
(use-package bibtex
  :config
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5))

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

(use-package helm-bibtex
  :config ;; Obtained from org-ref page
  (setq bibtex-completion-bibliography '("~/cloud/org/references/articles.bib")
	bibtex-completion-library-path '("~/cloud/org/references/pdfs")
	bibtex-completion-notes-path '("~/cloud/org/references/notes")
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath))))

;; org-ref to make it easier to track references in emacs org-mode
(use-package org-ref)

;; Note taking on pdfs in org mode
(use-package interleave)

;; ;; Highlights the current line
;; (use-package hl-line
;;   :init
;;   (global-hl-line-mode 1)
;;   :custom-face
;;   (hl-line ((t (:background "#555555")))))

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
  (which-key-mode))

;; Language Server Protocol - comprehending the code & showing potential errors
(use-package lsp-mode
  :hook ((python-mode . lsp-mode)
	 (c-mode-common . lsp-mode))
  :config
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=2" "-background-index" "--query-driver=/usr/bin/c++"))
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-idle-delay 0.1)
  (setq read-process-output-max (* 1024 1024)) ;; Emacs default (4K) too low for LSP
  (setq gc-cons-threshold (* 100 1024 1024)) ;; Emacs default low for LSP
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (define-key lsp-mode-map (kbd "C-c s") lsp-command-map))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-cursor t))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; Syntax checking for any code
(use-package flycheck
  :hook ((python-mode . flycheck-mode)
	 (c-mode-common . flycheck-mode)))

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
