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
(when (member "JetBrainsMono NF 10" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrainsMono NF 10")
  (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono NF 10"))

(when (member "Inter Nerd Font" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Inter Nerd Font" :height 1.18))

(custom-set-faces '(line-number ((t 'default))))

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
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT" "PROG" "WAIT" "|" "DONE(@)" "CANCELLED(@)")))
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
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

(set-face-attribute 'line-number nil :font "JetBrainsMono NF 10")

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

;; EVIL MODE
;; required packages
(use-package goto-chg)
(use-package undo-tree)
(use-package undo-fu)
(global-undo-tree-mode)

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
    (setq evil-undo-system 'undo-tree)
  :config
    (evil-mode)
    (evil-set-leader nil (kbd "SPC"))
    (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-capture)
    (evil-define-key 'normal 'global (kbd "<leader>otc") 'org-todo)
    (evil-define-key 'normal 'global (kbd "<leader>ost") 'org-set-tags-command)
    (evil-define-key 'normal 'global (kbd "<leader>pv") 'find-file)
    (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)
    (evil-define-key 'normal 'global (kbd "<leader>ol") 'org-store-link)
    (evil-define-key 'normal 'global (kbd "<leader>oo") 'org-open-at-point)
    (evil-define-key 'normal 'global (kbd "<leader>ons") 'org-narrow-to-subtree)
    (evil-define-key 'normal 'global (kbd "<leader>onw") 'widen)
    (evil-define-key 'normal 'global (kbd "<leader>j") 'jump-to-register)
    (evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)
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
(evil-define-key 'normal 'global (kbd "<leader>oti") 'org-clock-in)
(evil-define-key 'normal 'global (kbd "<leader>oto") 'org-clock-out)
(defvar capture-extra-templates '())
(setq org-agenda-custom-commands
      '(("n" "Next tasks"
         ((tags-todo "+dm+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Device Management")))
          (tags-todo "+ot+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Oficina técnica")))
          (tags-todo "-{.*}+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Sin clasificar")))
        ))))
;; end customization

(setq org-capture-templates
      custom-capture-templates)
