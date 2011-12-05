;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- elisp -*-

; define load path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

; load config pieces
(add-to-list 'load-path "~/.emacs.d/conf.d")
(mapc 'load (directory-files "~/.emacs.d/conf.d" nil "^[^#].*el$"))

(setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(setq write-file-functions nil)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blank-line-length 120)
 '(column-number-mode t)
 '(ido-mode (quote buffer) nil (ido))
 '(magit-git-executable "/usr/local/bin/git")
 '(org-agenda-files (quote ("/Users/evgenysokolov/Documents/org-notes/dsb-enh-spec.org" "/Users/evgenysokolov/Documents/org-notes/migrate-to-rails3-pros.org" "/Users/evgenysokolov/Documents/org-notes/rails.org" "/Users/evgenysokolov/Documents/org-notes/students.org" "/Users/evgenysokolov/Documents/org-notes/tasks.org" "/Users/evgenysokolov/Documents/org-notes/tl-training.org" "/Users/evgenysokolov/Documents/org-notes/work.org")))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Envy Code R"))))
 '(cursor ((t (:background "black" :inverse-video nil))))
 '(diff-added ((t (:foreground "light green"))))
 '(diff-added-face ((t (:background "light green"))) t)
 '(diff-removed ((t (:foreground "light pink"))))
 '(diff-removed-face ((t (:background "light pink"))) t)
 '(magit-diff-add ((((class color) (background light)) (:foreground "green4"))))
 '(magit-diff-file-header ((t (:inherit magit-header :background "lightgreen"))))
 '(region ((t (:background "khaki3" :foreground "black")))))
