(progn
  (setq inhibit-startup-message t) ; don't show startup message
  (show-paren-mode t) ; hilight matching parens
  (global-font-lock-mode t) ; use font-lock globally
  (setq visible-bell 1) ; use visible bell
                                        ;(if window-system (menu-bar-mode -1))
                                        ;(if window-system (tool-bar-mode -1))
                                        ;(if window-system (scroll-bar-mode -1))
  (if window-system
      (progn (tool-bar-mode -1) ; turn off toolbar
             (scroll-bar-mode -1) ; turn off scrillbars
             (menu-bar-mode 1)) ; show menubar
    (menu-bar-mode -1)) ; turn off menubar in terminal mode

  (fset 'yes-or-no-p 'y-or-n-p) ; use y or n instead of yes or no
  (setq-default indent-tabs-mode nil) ; don't use tabs for indentation
  (setq-default tab-width 4) ; default tab width
  (setq save-abbrevs nil) ;
  (global-hl-line-mode 1) ; hilight current line globally
  (line-number-mode t) ; show line number in statusbar
  (column-number-mode t)) ; show column number in statusbar

(provide 'personal)
