;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "H. Utku Unlu"
      user-mail-address "utku@utkuunlu.com")

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
(setq tab-always-indent 'complete)

;; Yasnippets (file-templates directory)
(setq +file-templates-dir "~/dotfiles/emacs/yasnippets")

;; Org mode keybinding for going back
(map! :after org
      :map org-mode-map
      "C-c M-o" #'org-mark-ring-goto)

;; Tool to help with executing commands inside Emacs
(use-package! which-key
  :config
  (setq which-key-side-window-location 'right
        which-key-idle-delay 0.3))

;; Vertico (Ivy substitution)
(use-package! vertico
  :bind (:map global-map
              ("C-x b" . consult-buffer)))

;; Directory navigation
(use-package! vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("<backspace>" . vertico-directory-delete-char)
              ("C-<backspace>" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ;; Mouse use in minibuffer
;; (use-package! vertico-mouse
;;   :after vertico
;;   :config
;;   (vertico-mouse-mode))

;; Prescient in vertico
(use-package! vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode))

;; TRAMP optimization
(setq tramp-auto-save-directory "/tmp")
(setq remote-file-name-inhibit-cache nil)
(setq remote-file-name-inhibit-locks t)
(setq tramp-verbose 1)

;; Apheleia formatting
;; "local" option runs the local formatter, as long as the whole file is not needed
(setq apheleia-remote-algorithm "local")

;; Additional org config
(add-hook! org-mode 'auto-fill-mode)

;; Org-roam: information linking
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> %?"
      :target (file+head "%<%Y-%m-%d>-inbox.org" "")
      :empty-lines 1)
     ("j" "journal" entry "* %<%H:%M> %?"
      :target (file+head "%<%Y-%m-%d>-journal.org.gpg"
                         ":PROPERTIES:
:DATE: %t
:ROAM_TAGS:
:END:\n")
      :empty-lines 1)))
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(
     ("a" "article" plain "%?"
      :target (file+head "%<%Y-%m-%d>-${slug}.org"
                         ":PROPERTIES:
:AUTHOR: %^{Author}
:DATE: %^{Publish date}t
:ROAM_REFS: %^{URL}
:ROAM_TAGS: [[id:2220e684-7773-4a12-af45-d092f49f99e9][article]]
:END:
#+TITLE: ${title}\n")
      :unnarrowed t)
     ("d" "default" plain "%?"
      :target (file+head "%<%Y-%m-%d>-${slug}.org" "#+TITLE: ${title}\n#+ROAM_TAGS:\n")
      :unnarrowed t)
     ("c" "category" plain "%?"
      :target (file+head "${slug}.org" "#+TITLE: ${title}\n#+ROAM_TAGS:\n")
      :immediate-finish t
      :unnarrowed t)
     ("b" "book" plain "%?"
      :target (file+head "%<%Y-%m-%d>-${slug}.org"
                         ":PROPERTIES:
:AUTHOR: %^{Author}
:DATE: %^{Publish date}t
:ROAM_TAGS:
:END:
#+TITLE: ${title}\n")
      :unnarrowed t)
     ("n" "literature note" plain "%?"
      :target (file+head
               "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
               ":PROPERTIES:
:AUTHOR: ${citar-author}
:NOTER_DOCUMENT: %(car citar-library-paths)${citar-citekey}.pdf
:END:
#+TITLE: ${citar-title}

* [[file:%(car citar-bibliography)::${citar-citekey}][${citar-citekey}]]\n")
      :unnarrowed t)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (org-roam-db-autosync-mode))

;; citar: citation & notes
(use-package! citar
  :custom
  (org-cite-global-bibliography '("~/org/references/articles.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/org/roam"))
  (citar-library-paths '("~/Documents/academics/pdfs/" "~/Documents/academics/books/")))

;; ;; citar-org-roam: integrate citar with roam
;; (use-package! citar-org-roam
;;   :after (:any citar org-roam)
;;   :custom
;;   (citar-org-roam-note-title-template "${author}-${title}")
;;   (citar-org-roam-capture-template-key "n")
;;   :config
;;   (citar-org-roam-mode))

;; org-noter: Taking notes on pdf files
(use-package! org-noter
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-notes-search-path '("~/org/roam/"))
  :config
  (org-noter-enable-org-roam-integration))

;; biblio: look-up papers from databases
(use-package! biblio
  :custom
  (biblio-arxiv-bibtex-header "article")
  (biblio-bibtex-use-autokey t)
  ;; Autokey formatting
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-titlewords 1)
  (bibtex-autokey-titleword-length -1) ;; -1 means exactly one
  (bibtex-autokey-titlewords-stretch 0)
  (bibtex-autokey-titleword-case-convert 'downcase)
  ;; Download directory
  (biblio-download-directory "~/Documents/academics/pdfs/"))

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

;; ;; Comp(lete)any package for completions
;; ;; :separate retains the order of results from the list of backends provided here.
;; ;; So, yasnippet results are prioritized
;; (setq company-backends '((:separate company-capf company-dabbrev-code company-files company-dabbrev))
;;       company-dabbrev-other-buffers nil ;; Do not use other buffers for dabbrev
;;       company-dabbrev-downcase nil ;; Do not replace text with lowercase versions
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       company-tooltip-limit 20)

;; Corfu - company but new
(use-package! corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-echo-delay 0.2)
  (corfu-quit-at-boundary nil)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)
  (corfu-preview-current #'insert)
  :bind (:map corfu-map
              ("<return>" . nil) ;; ENTER does not complete
              ("<tab>" . nil) ;; Tab does not complete
              ("M-SPC" . corfu-insert-separator)
              ("S-<return>" . corfu-insert))
  ;; ("TAB" . corfu-next)
  ;; ([tab] . corfu-next)
  ;; ("S-TAB" . corfu-previous)
  ;; ([backtab] . corfu-previous)
  ;; ("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-echo-mode)
  (corfu-history-mode))

(use-package! nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape - more capf for corfu
(use-package! cape)

;; Yasnippets capf
(use-package! yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; ;; Language Server Protocol - comprehending the code
;; (use-package! lsp-mode
;;   :hook ((python-mode . lsp-mode)
;;          (c-mode-common . lsp-mode)
;;          (lsp-mode . (lambda ()
;;                        (let ((lsp-keymap-prefix "C-c l"))
;;                          (lsp-enable-which-key-integration)))))
;;   :config
;;   ;; query-driver option fetches the compilation symbols from the used compiler
;;   ;; (No need to retain 2 copies of standard libraries if using gcc)
;;   (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=2" "-background-index" "--query-driver=/usr/bin/c++")
;;         lsp-clangd-binary-path "/usr/bin/clangd"
;;         +format-with-lsp nil
;;         lsp-idle-delay 0.1))

;; Eglot for LSP backend
(use-package! eglot
  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd"
                                                    "--header-insertion-decorators=0"
                                                    "-j=2"
                                                    "--background-index"
                                                    "--query-driver=/usr/bin/c++"))))

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
