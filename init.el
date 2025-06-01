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
(setq display-line-numbers-type 'relative)
;; hide line numbers in certain modes
(dolist
  (mode '(term-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; FONT
(set-face-attribute 'default nil :font "JetBrainsMono NF 10")

;; setup package repositories
(require 'package)
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.helpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; make sure use-package macro is present and usable
(require 'use-package)
(setq use-package-always-ensure t)

;; THEME: catppuccin mocha
(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'mocha)

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

;; MAGIT
(use-package magit)

;; NIX MODE
(use-package nix-mode)

;; ORG-MODE
(setq org-default-notes-file "~/wiki/org/inbox.org")
(setq org-agenda-files '("~/wiki/org"))
(setq org-refile-targets
  `((nil :maxlevel . 3)
	(,(directory-files-recursively "~/wiki/org/" "^[a-z]*.org$") :maxlevel . 3))) ;; 
(defvar custom-capture-templates
  '(("t" "Todo" entry (file "~/wiki/org/inbox.org")
     "* TODO %? \n")
    ("n" "Plain note" entry (file "~/wiki/org/inbox.org")
     "* %? \n")))
(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CANCELLED")))

;; EVIL MODE
;; required packages
(use-package goto-chg)
(use-package undo-tree)
(use-package undo-fu)
;; evil settings
(use-package evil
  :ensure t
  :init
    (setq evil-search-module 'evil-search)
    (setq evil-ex-complete-emacs-commands nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-shift-round nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-keybinding nil)
  :config
    (evil-mode)
    (evil-set-leader nil (kbd "SPC"))
    (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-capture)
    (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)
    (evil-define-key 'normal 'global (kbd "<leader>ol") 'org-store-link)
    (evil-define-key 'normal 'global (kbd "<leader>j") 'jump-to-register)
    (evil-define-key 'normal org-mode-map
                     (kbd "TAB") 'org-cycle
                     (kbd "<leader>>") 'org-shiftmetaright
                     (kbd "<leader><") 'org-shiftmetaleft))
    ;; example how to map a command in normal mode (called 'normal state' in evil)
    ;; (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

;; EVIL-COLLECTION
(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init '(dired org-mode magit)))

;; Shortcuts to several files (to use with <leader>j)
(set-register ?p (cons 'file "~/wiki/org/projects.org"))
(set-register ?i (cons 'file "~/wiki/org/inbox.org"))

;; start customization
;; end customization

(setq org-capture-templates
      custom-capture-templates)
