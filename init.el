(use-package emacs
  :custom
  (inhibit-startup-message t)
  (visible-bell t)
  (display-line-numbers-type 'visual)
  (default-frame-alist '((min-height . 1)'(height . 45)
                         (min-width  . 1)  '(width  . 81)
                         (vertical-scroll-bars . nil)
                         (internal-border-width . 16)
                         (left-fringe . 0)
                         (right-fringe . 0)
                         (tool-bar-lines . 0)))
  (initial-frame-alist default-frame-alist)
  (global-hl-line-mode t)
  :bind (("C-c c" . 'org-capture)
         ("C-c a" . 'org-agenda))
  :custom-face
  (default ((nil (:family "Roboto Mono" :weight light :height 110))))
  (bold ((nil (:family "Roboto Mono" :weight regular))))
  (italic ((nil (:family "Victor Mono" :weight semilight :slant italic))))
  (fixed-pitch ((nil (:family "FiraCode Nerd Font" :weight light))))
  :hook
  (after-make-frame-functions . (lambda (f)
                                  (with-selected-frame f
                                    ((window-divider-default-right-width 2)
                                     (window-divider-default-places 'right-only)
                                     (left-margin-width 0)
                                     (right-margin-width 0)))))
  (after-init . (lambda ()
                  (scroll-bar-mode -1)
                  (tool-bar-mode -1)
                  (tooltip-mode -1)
                  (menu-bar-mode -1)
                  (set-fringe-mode 10)
                  (column-number-mode)
                  (global-display-line-numbers-mode t))))

;; nano-theme
(use-package nano-theme
  ;; :vc (:url "https://github.com/rougier/nano-theme"
  ;;           :branch "rewrite"
  ;;           :rev :newest)
  :hook
  (after-make-frame-functions . (lambda (frame)
                                  (with-selected-frame frame
                                    (load-theme 'nano-light t))))
  (after-init . (lambda ()
                  (load-theme 'nano-light t))))

;; nano-layout
(use-package nano-modeline
  ;; :vc (:url "https://github.com/rougier/nano-modeline"
  ;;           :rev :newest)
  :hook
  ((prog-mode . nano-modeline-prog-mode)
   (text-mode . nano-modeline-text-mode)
   (org-capture-mode . nano-modeline-org-capture-mode)
   (org-agenda-mode . nano-modeline-org-agenda-mode)))

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
      ((agenda "" ((org-agenda-span 1)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next tasks")))
       (todo "PROG"
             ((org-agenda-overriding-header "Ongoing tasks")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Waiting tasks")))))
     ("d" "Daily review"
      ((todo "NEXT"
	     ((org-agenda-overriding-header "Next tasks")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Waiting tasks")))
       (todo "PROG"
	     ((org-agenda-overriding-header "Ongoing tasks")))
       (todo "TODO"
	     ((org-agenda-overriding-header "Todo tasks")))))
     ("w" "Weekly review"
      ((agenda "" ((org-agenda-start-on-weekday 0)))
       (todo "WAIT"
             ((org-agenda-overriding-header "Waiting tasks")))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Next tasks")))
       (todo "PROG"
	     ((org-agenda-overriding-header "Ongoing tasks")))
       (todo "TODO"
	     ((org-agenda-overriding-header "Todo in projects and inbox")))
       (todo "TODO"
	     ((org-agenda-files '("~/wiki/org/someday.org"))
	      (org-agenda-overriding-header "Todo in 'Someday'")))))))
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

					; for correct alignment on mixed fixed and variable pitch fonts
(use-package org-indent
  :ensure nil
  :custom (set-face-attribute 'org-indent nil
                              :inherit '(org-hide fixed-pitch))
  :hook org-mode)

(use-package nix-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx\\'" . tsx-ts)
  :hook typescript-ts)

(use-package python-mode
  :ensure nil
  :hook python-ts)

;; mini-buffer goodies
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;; projectile
(use-package projectile
  :custom
  (projectile-project-search-path
   '("~/Documentos/Programas" "~/.config/home-manager"))
  :bind-keymap
  (("C-c C-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :hook
  (after-init . projectile-mode))

;; olivetti mode for org-mode
(use-package olivetti
  :custom ((olivetti-body-width 0.75)
	   (olivetti-style 'margins))
  :hook org-mode)

;; start customization
(require 'lilypond-mode)
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
