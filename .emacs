;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- elisp -*-

; define load path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(setq inhibit-startup-message t)
(show-paren-mode t)
(global-font-lock-mode t)
(setq visible-bell 1)
(menu-bar-mode -1)
;(tool-bar-mode -1)
;(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq save-abbrevs nil)
(global-hl-line-mode 1)
(line-number-mode t)
(column-number-mode t)

;; MaGit
(require 'magit)
(require 'magit-svn)

;;;/-----------------
;;; Functions

(defun my-reload-dot-emacs ()
  "reloads .emacs"
  (interactive)
  (load-file "~/.emacs"))

(defun my-kill-whole-line ()
   "Kill an entire line, including trailing newline"
   (interactive)

   (beginning-of-line)
   (kill-line 1))

(defun my-move-line-up ()
  "Move a line up a single position."
  ;; @@ Moves lines without a newline at the end, too
  (interactive)

  (if (> (count-lines 1 (point)) 0)
      (let ((my-previous-column (current-column)))
        (progn
          (my-kill-whole-line)
          (previous-line 1)
          (beginning-of-line)
          (yank)
          (previous-line 1)
          (move-to-column my-previous-column)))))

(defun my-move-line-down ()
  "Move a line down a single position."
  (interactive)

  (if (< (count-lines 1 (point)) (1- (count-lines 1 (point-max))))
      (let ((my-previous-column (current-column)))
        (progn
          (my-kill-whole-line)
          (next-line 1)
          (beginning-of-line)
          (yank)
          (previous-line 1)
          (move-to-column my-previous-column)))))

(defun my-untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;;;\-----------------

;;;/-----------------
;;; Keybindings

(define-key global-map (kbd "C-c p") ; open magit status buffer
  'magit-status)

(define-key global-map (kbd "C-c C-c u") 'blank-mode)

(define-key global-map (kbd "C-x k")             ; kill current buffer
  (lambda () (interactive) (kill-buffer (current-buffer))))

(define-key global-map (kbd "C-A") ; move to the first non-space character on the line
  'back-to-indentation)
(define-key global-map (kbd "RET")   ; auto-indent on pressing <enter>
  'newline-and-indent)

;; move current line up and down
(define-key global-map (kbd "C-c k") 'my-move-line-up)
(define-key global-map (kbd "C-c j") 'my-move-line-down)

;; kill whole line
(define-key global-map (kbd "C-c d") 'my-kill-whole-line)

;; switch between emacs windows in reverse direction
(global-set-key (kbd "C-x O")   (lambda () (interactive) (other-window -1)))

;; reload .emacs
(global-set-key (kbd "C-c C-r e") 'my-reload-dot-emacs)

;;;\-----------------

;;;/-----------------
;;; C++-Mode options

(defun my-c++-mode-hook ()
  (c-set-style "stroustrup")
  (font-lock-mode)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'case-label 4)
  (c-set-offset 'arglist-intro 2)
  (c-set-offset 'arglist-close 0)
  (setq tab-width 4)
  (setq indent-tabs-mode t))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; CC-Mode options
;; ---------------
(defun my-c-mode-hook ()
  (c-set-style "stroustrup")
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'case-label 4)
  (c-set-offset 'arglist-intro 2)
  (c-set-offset 'arglist-close 0)
  (setq tab-width 4)
  (setq indent-tabs-mode t))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;;;\-----------------

;;;/-----------------
;;; Javascript settings

; use js-mode because it handles indentation better
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;;;\-----------------

;;;/-----------------
;;; Ruby settings

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "C-x C-e") ; toggle ruby-electric mode
              (ruby-electric-mode))))

;;;\-----------------

;;;/-----------------
;;; iBuffer

;; default groups for ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("C-source" (or (mode . c-mode)
                               (mode . c++-mode)))
               ("Ruby-source" (mode . ruby-mode))
               ("ERB-source" (or (name . ".*\\.erb")
                                 (name . ".*\\.rhtml")))
               ("Javascript" (or
                              (mode . js2-mode)
                              (mode . js-mode)))
               ("Shell-source" (mode . sh-mode))
               ("Makefiles" (or (mode . makefile-mode)
                                (mode . GNUmakefile)
                                (name . "Makefile")))
               ("Java-source" (mode . java-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\.emacs$")))
       ;;         ("Gnus" (or
;;                         (name . "^\\*Article\\*$")
;;                         (name . "^\\*Summary.*$")
;;                         (name . "^\\*Group\\*$")))
               ("VC" (or
                      (name . "^\\*svn-.*\\*$")
                      (name . "^\\*git-.*\\*$")
                      (name . "^\\*magit.*$")
                      (name . "^\\*cvs.*\\*$")
                      (name . "^\\*[Vv][Cc].*\\*$")))
               ("ERC" (mode . erc-mode))
           ))))

;; ibuffer, I like my buffers to be grouped
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups
             "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;\---------------------

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '())
(add-to-list 'org-agenda-files "~/Documents/org-notes/")

;;;/-----------------
;;; ERC settings

(setq erc-server-conding-system '(windows-1251 . windows-1251))
      ;;;erc-encoding-coding-alist '(("#test" . windows-1251)))

;;;\----------------

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
 '(ido-mode (quote buffer) nil (ido)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
