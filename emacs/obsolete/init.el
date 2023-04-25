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

;; Adjust font size (10pt)
(set-face-attribute 'default nil :height 100)

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
(setq auto-save-list-file-prefix autosave-dir
      auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(setq backup-directory-alist `(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      delete-by-moving-to-trash t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200
      version-control t)

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

;; Make use-package use straight by default & optimize vc filesizes
(use-package straight
  :config
  (setq straight-use-package-by-default t
	straight-vc-git-default-clone-depth '(1 single-branch)))

;; Guru mode - practice avoiding arrow keys
(use-package guru-mode
  :config
  (guru-global-mode))

;; Org mode
;; Check https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html for GTD
(use-package org
  :bind
  (:map global-map
	("C-c s" . org-store-link)
	("C-c C-s" . org-insert-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture))
  :hook (org-mode . auto-fill-mode)  ;; Adjust the text length
  :config
  (setq org-log-done t
	org-agenda-files (list "~/org/agenda/work.org"
			       "~/org/agenda/home.org"
			       "~/org/gtd/inbox.org"
			       "~/org/gtd/gtd.org"
			       "~/org/gtd/tickler.org")
	org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline "~/org/gtd/inbox.org" "Tasks")
				 "* TODO %i%?")
				("T" "Tickler" entry
				 (file+headline "~/org/gtd/tickler.org" "Tickler")
				 "* %i%? \n %U")
				("n" "Note" item
				 (file+headline "~/org/notes/notes.org" "Notes (Uncategorized)")
				 "- %?" :empty-lines-before 1))
	org-refile-targets '(("~/org/gtd/gtd.org" :maxlevel . 3)
                             ("~/org/gtd/someday.org" :level . 1)
                             ("~/org/gtd/tickler.org" :maxlevel . 2))
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

;; org-ref to make it easier to track references in emacs org-mode
(use-package org-ref
  :bind
  (:map org-mode-map
	("C-c ]" . org-ref-insert-link))
  (:map global-map
	("C-c ;" . doi-utils-add-entry-from-crossref-query)))

;; org-roam: information linking
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

;; pdf-tools for better pdf management / viewing
;; hook: see https://github.com/vedang/pdf-tools#display-line-numbers-mode
(use-package pdf-tools
  :hook
  ((pdf-view-mode . (lambda ()
		      (display-line-numbers-mode -1)
		      (setq pdf-view-display-size 'fit-page))))
  :config
  (pdf-tools-install))

;; org-noter: Taking notes on pdf files
;; additional setup obtained from https://github.com/fuxialexander/org-pdftools
(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("~/org/references/notes/")
	org-noter-always-create-frame nil
	org-noter-kill-frame-at-session-end nil))

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

;; Tools to process web content as org documents
;; https://github.com/alphapapa/org-web-tools
;; Depends on pandoc to be installed in your system
;; Command of use: org-web-tools-read-url-as-org
(use-package org-web-tools)

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
  :bind
  (:map global-map
	("C-s" . swiper)
	("C-c C-r" . ivy-resume)
	("<f6>" . ivy-resume))
  (:map ivy-minibuffer-map
	("<return>" . ivy-alt-done))
  (:map minibuffer-local-map
	("C-r" . counsel-minibuffer-history))
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)  ;; Doesn't work with swiper & prescient together

(use-package counsel
  :bind
  (:map global-map
	("C-x l" . counsel-locate) ;; By default, C-x l is for page lines
	("M-x" . counsel-M-x))
  :config
  (counsel-mode))

(use-package swiper)

;; ;; Ranger - dired but more (or maybe not as much)
;; (use-package ranger
;;   :bind
;;   (:map global-map
;; 	("C-x d" . ranger))
;;   :config
;;   (ranger-override-dired-mode t))

;; Display output in a posframe
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode))

(use-package ivy-bibtex
  :demand t
  :bind
  (:map global-map
	("C-c i" . ivy-bibtex))
  :config ;; Obtained from org-ref page
  (setq bibtex-completion-bibliography '("~/org/references/articles.bib")
	bibtex-completion-library-path '("~/org/references/pdfs/")
	bibtex-completion-notes-path "~/org/references/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

;; async is a dependency for org-ref to be able to download files asynchronously
(use-package async)

;; Magit - Git interface
(use-package magit
  :bind
  (:map global-map
	("C-c m" . magit-status))
  :config
  (setq git-commit-summary-max-length 50)
  :hook
  (git-commit-mode . (lambda ()
		       (setq fill-column 72))))

;; Comp(lete)any package for completions
(use-package company
  :init
  (global-company-mode 1)
  :config
  ;; Backend-relevant
  ;; :separate retains the order of results from the list of backends provided here.
  ;; So, yasnippet results are prioritized
  (setq company-backends '((:separate company-yasnippet company-capf company-dabbrev-code company-files company-dabbrev))
	company-dabbrev-other-buffers nil ;; Do not use other buffers for dabbrev
	company-dabbrev-downcase nil) ;; Do not replace text with lowercase versions

  (setq company-idle-delay 0.0
	company-minimum-prefix-length 1
	company-tooltip-limit 20))

;; Prescient - different, predictive sorting / finding algorithm
(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

;; (use-package company-prescient
;;   :config
;;   (company-prescient-mode))

(use-package company-jedi
  :commands (company-jedi)
  :after (company python-mode))

;; Yasnippet provides easy insertion of boilerplate snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/dotfiles/emacs/yasnippets"))
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode))

;; Support for YAML syntax
(use-package yaml-mode
  :bind
  (:map yaml-mode-map
	("C-m" . newline-and-indent)))

;; Support for Dockerfile syntax
(use-package dockerfile-mode)

;; Support for cmake syntax
(use-package cmake-mode)

;; Which-key shows available commands after a key press
(use-package which-key
  :config
  ;; To trigger manually, enable below settings
  ;; (setq which-key-show-early-on-C-h t
  ;; 	which-key-idle-delay 10000
  ;; 	which-key-idle-secondary-delay 0.05)
  (setq which-key-side-window-location 'right)
  (which-key-mode))


;; Language Server Protocol - comprehending the code & showing potential errors
(use-package lsp-mode
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :hook ((python-mode . lsp-mode)
	 (c-mode-common . lsp-mode)
	 (lsp-mode . (lambda ()
		       (let ((lsp-keymap-prefix "C-c l"))
                         (lsp-enable-which-key-integration)))))
  :config
  ;; query-driver option fetches the compilation symbols from the used compiler
  ;; (No need to retain 2 copies of standard libraries if using gcc)
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=2" "-background-index" "--query-driver=/usr/bin/c++")
	lsp-clangd-binary-path "/usr/bin/clangd"
	lsp-idle-delay 0.1
	read-process-output-max (* 1024 1024) ;; Emacs default (4K) too low for LSP
	gc-cons-threshold (* 100 1024 1024))) ;; Emacs default low for LSP

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
  :hook ((prog-mode . (lambda ()
			(unless (eq major-mode 'csharp-mode)
			  (format-all-mode 1))))
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
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :config
  (counsel-projectile-mode))

;; Treemacs - projectile integration
(use-package treemacs-projectile)

;; Use google c-style
(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

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

;; ;; Small overview of the code on the side (Deactivated. Didn't find it too useful)
;; (use-package minimap)

;; C# support
(use-package csharp-mode)

;; GLSL support
(use-package glsl-mode)

;; Use shell environment
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Cleans whitespaces on modified lines
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; Ask for confirmation when using C-c C-x to kill a session
(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-emacs)
    (message "Canceled frame close")))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; Hide scratch buffer when opening emacs
(add-hook 'emacs-startup-hook (lambda ()
                                (when (get-buffer-window "*scratch*")
                                  (bury-buffer "*scratch*"))))

;; Show agenda as the first thing
(setq initial-buffer-choice (lambda ()
			      (org-agenda-list)
			      (get-buffer "*Org Agenda*")))

;; Set theme the last (So that errors are caught)
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))