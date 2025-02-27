(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; hide scrollbar
(tool-bar-mode -1) ;; hide toolbar
(tooltip-mode -1) ;; disable tooltips
(set-fringe-mode 10)

(menu-bar-mode -1) ;; disable menubar

(setq visible-bell t) ;; unset sound bell

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 110)

;; setup package repositories
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Add fuzzy-find-like functionality
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package typescript-mode)

(use-package nix-mode)

;; tree-sitter modes remap
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

;; org-mode settings
(setq org-default-notes-file "~/wiki/org/refile.org")
(setq org-agenda-files '("~/wiki/org"))

;; keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq load-path (append (list (expand-file-name "/nix/store/jma0nckl9ladm4n4j5ixrbf33vm44q9z-lilypond-2.24.4/share/emacs/site-lisp")) load-path))

(require 'LilyPond-mode)
