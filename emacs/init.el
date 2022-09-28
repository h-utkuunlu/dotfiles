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
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; Wrap line around
(global-visual-line-mode 1)

;; Show empty lines
(set-default 'indicate-empty-lines t)

;; Use clipboard
(setq select-enable-clipboard t)

;; Display 88-col fill indicator (following python black)
(setq-default fill-column 88)
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
  :hook (org-mode . auto-fill-mode)  ;; Adjust the text length
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

;; Fuzzy search functionality (ivy - counsel - swiper)
(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)  ;; Doesn't work with swiper & prescient together
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel
  :config
  (counsel-mode)
  (global-set-key (kbd "C-x l") 'counsel-locate)) ;; By default, C-x l is for page lines

(use-package swiper)

;; Display output in a posframe
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode))

(use-package ivy-bibtex
  :bind
  (:map global-map
	("C-c i" . ivy-bibtex)
	("C-c r" . ivy-resume))
  :config ;; Obtained from org-ref page
  (setq bibtex-completion-bibliography '("~/cloud/org/references/articles.bib")
	bibtex-completion-library-path '("~/cloud/org/references/pdfs/")
	bibtex-completion-notes-path "~/cloud/org/references/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

;; async is a dependency for org-ref to be able to download files asynchronously
(use-package async)

;; org-ref to make it easier to track references in emacs org-mode
(use-package org-ref
  :bind
  (:map org-mode-map
	("C-c ]" . org-ref-insert-link)))

;; pdf-tools for better pdf management / viewing
(use-package pdf-tools
  :hook
  ((pdf-view-mode . (lambda ()
		      (display-line-numbers-mode -1))))
  :config
  (pdf-tools-install))

;; org-noter: Taking notes on pdf files
;; additional setup obtained from https://github.com/fuxialexander/org-pdftools
(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("~/cloud/org/references/notes/")))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; Magit - Git interface
(use-package magit
  :config
  (global-set-key (kbd "C-c m") 'magit-status))

;; Comp(lete)any package for completions
(use-package company
  :init
  (global-company-mode 1)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1))

;; Prescient - different, predictive sorting / finding algorithm
(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :config
  (company-prescient-mode))

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

;; Support for cmake syntax
(use-package cmake-mode)

;; Which-key shows available commands after a key press
(use-package which-key
  :config
  (which-key-mode))

;; Language Server Protocol - comprehending the code & showing potential errors
(use-package lsp-mode
  :hook ((python-mode . lsp-mode)
	 (c-mode-common . lsp-mode))
  :config
  ;; query-driver option fetches the compilation symbols from the used compiler
  ;; (No need to retain 2 copies of standard libraries if using gcc)
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=2" "-background-index" "--query-driver=/usr/bin/c++"))
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-idle-delay 0.1)
  (setq read-process-output-max (* 1024 1024)) ;; Emacs default (4K) too low for LSP
  (setq gc-cons-threshold (* 100 1024 1024)) ;; Emacs default low for LSP
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (define-key lsp-mode-map (kbd "C-c s") lsp-command-map))

;; Python LSP interface
(use-package lsp-jedi)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-cursor t))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; Syntax checking for any code
(use-package flycheck
  :hook ((python-mode . flycheck-mode)
	 (c-mode-common . flycheck-mode)))

;; Formatting code
;; Install externally to run formatting on:
;; - C/C++ - clang-format
;; - Python - black
;; - HTML/XML - tidy
;; Formatter for many programming languages
(use-package format-all
  :bind ("C-c f" . format-all-buffer)
  :hook ((prog-mode . format-all-mode)
	 (format-all-mode . format-all-ensure-formatter)))

;; Show file structure
(use-package treemacs
  :bind
  (:map global-map
	([f8] . treemacs)  ;; Open / close treemacs
	("C-<f8>" . treemacs-select-window))  ;; Switch to the side window
  :config
  (setq treemacs-is-never-other-window t))  ;; C-x o does not include treemacs window

;; Projectile - project interaction library to access files / buffers
(use-package projectile)

;; Counsel (Ivy) projectile integration
(use-package counsel-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode))

;; Treemacs - projectile integration
(use-package treemacs-projectile)

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

;; Allows opening terminals in the folder associated with buffer
(use-package terminal-here
  :bind ("C-c t" . terminal-here-launch))

;; Ace window - easier window navigation
(use-package ace-window
  :bind ("M-o" . ace-window))

;; Text folding for easier code navigation
(use-package origami
  :bind
  (:map origami-mode-map
	("<backtab>" . origami-recursively-toggle-node)
	("C-<tab>" . origami-toggle-all-nodes))
  :hook (prog-mode . origami-mode))

;; Set theme the last (So that errors are caught)
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))
