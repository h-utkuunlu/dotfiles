;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "H. Utku Unlu"
      user-mail-address "h.utkuunlu@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night-bright)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Splash screen
(setq fancy-splash-image (concat doom-user-dir "splash.png"))

;; Font size
(set-face-attribute 'default nil :height 100)

;; Display 88-col fill indicator (following python black)
(setq-default fill-column 88)
(global-display-fill-column-indicator-mode)
;; (add-hook '+doom-dashboard-mode-hook (lambda ()
;;                                        (setq-local display-fill-column-indicator nil)))

;; Tab always indents
(setq tab-always-indent t)

;; Which-key buffer on the right side
(setq which-key-side-window-location 'right)

;; Configure Ivy to enter a directory
(use-package! ivy
  :bind
  (:map ivy-minibuffer-map
        ("<return>" . ivy-alt-done)
        ("M-<return>" . ivy-done)))

;; TRAMP optimization
(setq tramp-auto-save-directory "/tmp")
(setq remote-file-name-inhibit-cache nil)
(setq remote-file-name-inhibit-locks t)
(setq tramp-verbose 1)

;; Apheleia formatting
;; "local" option runs the local formatter, as long as the whole file is not needed
(setq apheleia-remote-algorithm "local")

;; Encryption
(org-crypt-use-before-save-magic)
(setq org-crypt-key "E0F2D5B50EDB3FC7")
(setq org-crypt-disable-auto-save t)

;; Additional org config
(add-hook! org-mode 'auto-fill-mode)
(setq org-agenda-files (list "~/org/agenda/work.org"
                             "~/org/agenda/home.org"
                             "~/org/gtd/tickler.org"))

;; Org journal
(setq org-journal-dir (file-truename "~/org/journal")
      org-journal-enable-encryption t
      org-journal-file-format "%Y-%m-%d")

;; Org-roam: information linking
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "
* Goals

%?

* Tasks

** TODO Add initial tasks

* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("n" "paper-note" plain ""
      :if-new (file+head "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
                         "
#+TITLE: ${citar-title}
#+AUTHOR: ${citar-author}
* [[file:%(car citar-bibliography)::${citar-citekey}][${citar-citekey}]]
:PROPERTIES:
:NOTER_DOCUMENT: %(car citar-library-paths)${citar-citekey}.pdf
:END:")
      :unnarrowed t)
     ("b" "book notes" plain
      "
* Source

Author: %^{Author}
Title: ${title}
Year: %^{Year}

* Summary

%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

;; citar: citation & notes
(use-package! citar
  :custom
  (citar-bibliography '("~/org/references/articles.bib"))
  (org-cite-global-bibliography citar-bibliography)
  (citar-notes-paths '("~/org/roam/"))
  (citar-library-paths '("~/org/references/pdfs/")))

;; citar-org-roam: integrate citar with roam
(use-package! citar-org-roam
  :after (:any citar org-roam)
  :custom
  (citar-org-roam-note-title-template "${author}-${title}")
  (citar-org-roam-capture-template-key "n")
  :config
  (citar-org-roam-mode))

;; org-noter: Taking notes on pdf files
(setq org-noter-always-create-frame nil
      org-noter-kill-frame-at-session-end nil
      org-noter-notes-search-path '("~/org/roam/"))

;; biblio: look-up papers from databases
(setq biblio-arxiv-bibtex-header "article"
      biblio-bibtex-use-autokey t
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-year-length 4
      bibtex-autokey-titlewords 2
      bibtex-autokey-titleword-length 5 ;; -1 means exactly one
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titleword-case-convert 'downcase)

;; Functions to add the entry at the end of the file
(defun my/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
  (with-current-buffer (find-file-noselect (car citar-bibliography))
    (goto-char (point-max))
    (insert "\n" bibtex))
  (message "Inserted bibtex entry for %S."
           (biblio--prepare-title (biblio-alist-get 'title entry))))
(defun ans/biblio-selection-insert-end-of-bibfile ()
  "Insert BibTeX of current entry at the end of user-specified bibtex file."
  (interactive)
  (biblio--selection-forward-bibtex #'my/biblio--selection-insert-at-end-of-bibfile-callback))
(map! :map biblio-selection-mode-map "a" #'ans/biblio-selection-insert-end-of-bibfile)

;; Modify downloading options
(setq biblio-download-directory (car citar-library-paths))
(defun my/biblio-download--action (record)
  "Retrieve a RECORD from Dissemin, and display it.
RECORD is a formatted record as expected by `biblio-insert-result'."
  (let-alist record
    (bibtex-generate-autokey)
    ;; (if .direct-url
    ;;     (let* ((fname (concat .identifier ".pdf"))
    ;;            (target (read-file-name "Save as (see also biblio-download-directory): "
    ;;                                    biblio-download-directory fname nil fname)))
    ;;       (url-copy-file .direct-url (expand-file-name target biblio-download-directory)))
    ;;   (user-error "This record does not contain a direct URL (try arXiv or HAL)"))
    ))

(advice-add 'biblio-download--action :override #'my/biblio-download--action)

;; Magit - Git interface
(use-package! magit
  :bind
  (:map global-map
        ("C-c m" . magit-status))
  :config
  (setq git-commit-summary-max-length 50)
  :hook
  (git-commit-mode . (lambda ()
                       (setq fill-column 72))))

;; Comp(lete)any package for completions
;; :separate retains the order of results from the list of backends provided here.
;; So, yasnippet results are prioritized
(setq company-backends '((:separate company-yasnippet company-capf company-dabbrev-code company-files company-dabbrev))
      company-dabbrev-other-buffers nil ;; Do not use other buffers for dabbrev
      company-dabbrev-downcase nil ;; Do not replace text with lowercase versions
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      company-tooltip-limit 20)

;; Language Server Protocol - comprehending the code
(use-package! lsp-mode
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
        +format-with-lsp nil
        lsp-idle-delay 0.1))

;; Show file structure
(use-package! treemacs
  :commands (treemacs)
  :bind
  (:map global-map
        ([f8] . treemacs)  ;; Open / close treemacs
        ("C-<f8>" . treemacs-select-window))  ;; Switch to the side window
  :config
  (setq treemacs-is-never-other-window t))  ;; C-x o does not include treemacs window

;; Ace window - easier window navigation
(use-package! ace-window
  :commands (ace-window)
  :bind ("M-o" . ace-window))

;; Allows opening terminals in the folder associated with buffer
(use-package! terminal-here
  :commands (terminal-here-launch)
  :bind ("C-c <return>" . terminal-here-launch))

;; Associate .launch files with xml
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Nov.el to read epub from emacs
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")
        nov-text-width 88))

(after! nov-mode
  (require 'org-noter-nov))

;; cuda to use clang-format
(after! cuda-mode
  (setq-local +format-with 'clang-format))
