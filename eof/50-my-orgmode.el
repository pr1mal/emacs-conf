;;; org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '())
(add-to-list 'org-agenda-files "~/Dropbox/org/")
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")))
