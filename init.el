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
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

;; ORG MODE
(setq org-directory "~/wiki/org")
(setq org-agenda-files (list "inbox.org" "projects.org" "agenda.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT" "WAIT(@)" "|" "DONE(@)" "CANCELLED(@)")))

(setq org-capture-templates
      `(("i" "Inbox" entry (file "inbox.org")
	 ,(concat "* %?\n" "- Fecha: %U"))
	("m" "Reunión" entry (file+headline "agenda.org" "Reuniones")
           "* Reunión %?\n%^T")
        ("q" "Quedada" entry (file+headline "agenda.org" "Social")
         "* Quedada con %?\n%^T")
	("n" "Nota de reunión" entry (file "inbox.org")
	 "* Notas (%a)\nFecha introducida: %U\n%?")))

(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-agenda-custom-commands
      '(("t" "Today"
	 ((agenda "")
	  (todo "PROG"
		(org-agenda-overriding-header "\nOngoing tasks\n"))
	  (todo "WAIT"
		(org-agenda-overriding-header "\nBlocked tasks\n"))))))

(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
      '(("agenda.org" . (:level . 1))
	("projects.org" . (:level . 1))
	("someday.org" . (:maxlevel . 3))))
