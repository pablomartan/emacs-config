;; General emacs settings
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

;; set up package management
(require 'package)
(package-initialize)

;; nano-theme
(use-package nano-theme
  :vc (:url "https://github.com/rougier/nano-theme.git"
            :main-file "nano-theme.el"
            :rev :newest
            :branch "rewrite"))

(load-theme 'nano t)

;; nano-layout
(use-package nano-modeline
  :vc (:url "https://github.com/rougier/nano-modeline.git"
            :rev :newest
            :branch "master"))

(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

(setq-default initial-major-mode 'text-mode   ; Initial mode is text
              default-major-mode 'text-mode)  ; Default mode is text

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'light
                    :height 110)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'regular)

(set-face-attribute 'italic nil
                    :family "Victor Mono"
                    :weight 'semilight
                    :slant 'italic)

(set-face-attribute 'fixed-pitch nil
                    :family "FiraCode Nerd Font"
                    :weight 'light)

;; org mode
(use-package org
  :ensure nil
  :custom
  (org-directory "~/wiki/org")
  (org-ellipsis " …")              ; Nicer ellipsis
  (org-tags-column 1)              ; Tags next to header title
  (org-hide-emphasis-markers t)    ; Hide markers
  (org-cycle-separator-lines 2)    ; Number of empty lines between sections
  (org-use-tag-inheritance nil)    ; Tags ARE NOT inherited 
  (org-use-property-inheritance t) ; Properties ARE inherited
  (org-indent-indentation-per-level 2) ; Indentation per level
  (org-link-use-indirect-buffer-for-internals t) ; Indirect buffer for internal links
  (org-fontify-quote-and-verse-blocks t) ; Specific face for quote and verse blocks
  (org-return-follows-link nil)    ; Follow links when hitting return
  (org-image-actual-width nil)     ; Resize image to window width
  (org-indirect-buffer-display 'other-window) ; Tab on a task expand it in a new window
  (org-outline-path-complete-in-steps nil) ; No steps in path display
  (org-agenda-files (list "inbox.org" "projects.org" "agenda.org"))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT" "PROG" "WAIT(@)" "|" "DONE(@)" "CANCELLED(@)")))
  (org-capture-templates
   `(("i" "Inbox" entry (file "inbox.org")
      ,(concat "* %?\n" "- Fecha: %U"))
     ("m" "Reunión" entry (file+headline "agenda.org" "Reuniones")
      "* Reunión %?\n%^T")
     ("q" "Quedada" entry (file+headline "agenda.org" "Social")
      "* Quedada con %?\n%^T")
     ("n" "Nota de reunión" entry (file "inbox.org")
      "* Notas (%a)\nFecha introducida: %U\n%?")))
  (org-agenda-custom-commands
   '(("t" "Today"
      ((agenda "")
       (todo "PROG"
             ((org-agenda-overriding-header "Ongoing tasks")))
       (todo "WAIT"
             ((org-agenda-overriding-header "Blocked tasks")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next tasks")))))))
  (org-refile-use-outline-path 'file)
  (org-refile-targets
   '(("agenda.org" . (:level . 1))
     ("projects.org" . (:maxlevel . 2))
     ("someday.org" . (:maxlevel . 3))))
  
  :custom-face
  (org-block ((nil (:foreground nil :inherit 'fixed-pitch))))
  (org-code ((nil (:inherit (shadow fixed-pitch)))))
  (org-indent ((nil (:inherit (org-hide fixed-pitch)))))
  (org-verbatim ((nil (:inherit (shadow fixed-pitch)))))
  (org-special-keyword ((nil (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-meta-line ((nil (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-checkbox ((nil (:inherit 'fixed-pitch))))

  :hook nano-modeline-org)

(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c a") 'org-agenda)

(require 'frame)

;; Default frame settings
(setq default-frame-alist '((min-height . 1)  '(height . 45)
                            (min-width  . 1)  '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 16)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 1)))

;; Default frame settings
(setq initial-frame-alist default-frame-alist)

(setq-default window-divider-default-right-width 2
              window-divider-default-places 'right-only
              left-margin-width 0
              right-margin-width 0)
              ;; window-combination-resize nil) ; Do not resize windows proportionally

(window-divider-mode 1)

; highlight current line
(require 'hl-line)
(global-hl-line-mode)

; for correct alignment on mixed fixed and variable pitch fonts
(use-package org-indent
  :ensure nil
  :custom (set-face-attribute 'org-indent nil
			      :inherit '(org-hide fixed-pitch))
  :hook org-mode)

(use-package nix-mode)

(use-package typescript-mode)

(add-to-list 'major-mode-remap-alist
	     '((python-mode . python-ts-mode)
	       (typescript-mode . typescript-ts-mode)))

;; mini-buffer goodies
(use-package vertico
  :hook (after-init . vertico-mode))

(use-package orderless
  :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion))))
    (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :hook (after-init . marginalia-mode))

;; projectile
(use-package projectile
  :custom (projectile-project-search-path '("~/Documentos/Programas" "~/.config/home-manager"))
  :bind-keymap (("C-c C-p" . projectile-command-map)
                ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode))
