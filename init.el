;; GENERAL EMACS SETTINGS
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;; hide scrollbar
(tool-bar-mode -1) ;; hide toolbar
(tooltip-mode -1) ;; disable tooltips
(set-fringe-mode 10)
(menu-bar-mode -1) ;; disable menubar
(setq visible-bell t) ;; unset sound bell

;; display line number
(column-number-mode)
(global-display-line-numbers-mode t) 
(setq display-line-numbers-type 'visual)

;; hide line numbers in certain modes
(dolist
  (mode '(term-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(load-theme 'modus-operandi)

;; FONT
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font")
(set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font")
(set-face-attribute 'line-number nil :family "JetBrainsMono Nerd Font")
(set-face-attribute 'variable-pitch nil :family "Inter Nerd Font" :height 1.18)

;; make sure use-package macro is present and usable
(require 'use-package)
(setq use-package-always-ensure t)

;; start emacs server
(server-start)
(require 'org-protocol)

;; CUSTOM PACKAGES

;; BETTER COMPLETION: vertico
(use-package vertico
  :ensure t
  :bind (:map vertico-map
    ("C-j" . vertico-next)
    ("C-k" . vertico-previous)
    ("C-f" . vertico-exit)
  :map minibuffer-local-map
    ("M-h" . backward-kill-word))
  :custom
    (vertico-cycle t)
  :init
    (vertico-mode))

;; MARGINALIA: Enable rich annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
          ("M-A" . marginalia-cycle))
  :init
    (marginalia-mode))

;; ORDERLESS: Add fuzzy-find-like functionality
(use-package orderless
  :ensure t
  :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))

;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; TYPESCRIPT MODE
(use-package typescript-mode)

;; NIX MODE
(use-package nix-mode)

;; TREE-SITTER
(setq major-mode-remap-alist
 '((bash-mode . bash-ts-mode)
   (bibtex-mode . bibtex-ts-mode)
   (css-mode . css-ts-mode)
   (dockerfile-mode . dockerfile-ts-mode)
   (elisp-mode . elisp-ts-mode)
   (html-mode . html-ts-mode)
   (json-mode . json-ts-mode)
   (latex-mode . latex-ts-mode)
   (ledger-mode . ledger-ts-mode)
   (lua-mode . lua-ts-mode)
   (markdown-mode . markdown-ts-mode)
   (python-mode . python-ts-mode)
   (sql-mode . sql-ts-mode)
   (typescript-mode . typescript-ts-mode)))

;; ORG-MODE
(setq org-default-notes-file "~/wiki/org/inbox.org")
(setq org-agenda-files '("~/wiki/org/projects.org"))
(setq org-refile-targets
  `((nil :maxlevel . 3)
	(,(directory-files-recursively "~/wiki/org/" "^[a-z]*.org$") :maxlevel . 3))) ;; 
(defvar custom-capture-templates
  '(("t" "Todo" entry (file+headline "~/wiki/org/inbox.org" "Tasks")
     "** TODO %? \n")
    ("n" "Plain note" entry (file+headline "~/wiki/org/inbox.org" "Notes")
     "** %? \n")
    ("p" "Protocol" entry (file+headline "~/wiki/org/inbox.org" "Notes")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
    ("L" "Protocol Link" entry (file+headline "~/wiki/org/inbox.org" "Notes")
        "* %? [[%:link][%:description]] \nCaptured On: %U")))

(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c j" 'org-store-link)

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROG" "WAIT" "|" "DONE(@)" "CANCELLED(@)")))
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-use-sub-superscripts "{}")
(require 'ox-md)

;; ORG EXTRAS
;; Resize Org headings
(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Inter Nerd Font" :weight 'bold :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Inter Nerd Font" :weight 'bold :height 1.8)

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(set-face-attribute 'org-block nil            :foreground nil :inherit 'fixed-pitch :height 0.85)
(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(plist-put org-format-latex-options :scale 2)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
	  org-ellipsis "  ·")

(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook 'visual-line-mode)

(use-package olivetti
             :diminish
             :commands olivetti-mode
             :config
             (setq olivetti-body-width 0.50)
             (setq olivetti-minimum-body-width 100))

(add-hook 'org-mode-hook 'olivetti-mode)

(use-package undo-tree)
(use-package undo-fu)
(global-undo-tree-mode)

;; start customization
(require 'lilypond-mode)
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(setq org-agenda-custom-commands
      '(("n" "Próximas tareas"
         (todo "PROG"
               ((org-agenda-overriding-header "Tareas en curso")))
         (todo "WAIT"
               ((org-agenda-overriding-header "Tareas en espera")))
         (todo "TODO"
                ((org-agenda-overriding-header "Tareas por empezar"))))))
;; end customization

(setq org-capture-templates
      custom-capture-templates)
