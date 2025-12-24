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
