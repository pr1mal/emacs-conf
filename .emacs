;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- elisp -*-

; define load path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-rails")
(add-to-list 'load-path "~/share/emacs/site-lisp")

;(load-file ".gnus")

(setq inhibit-startup-message t)
(show-paren-mode t)
(global-font-lock-mode t)
(setq visible-bell 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'git)

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

(defun todoo-or-close-todo ()
  "Call todoo buffer and close it if already in it"
  (interactive)
  (if (eq major-mode 'todoo-mode)
      (call-interactively 'todoo-save-and-exit)
    (call-interactively 'todoo)))

;;;\-----------------

(fset 'yes-or-no-p 'y-or-n-p)

;;;/-----------------
;;; Keybindings

(define-key global-map [f12] ; call or close todoo buffer
  'todoo-or-close-todo)

(define-key global-map [f8]             ; kill current buffer
  (lambda () (interactive) (kill-buffer (current-buffer))))

;;(define-key global-map [home] ; move to the first non-space character on the line
;;  'back-to-indentation)
(define-key global-map "\r"   ; auto-indent on pressing <enter>
  'newline-and-indent)

;; move current line up and down
(define-key global-map "\C-ck" 'my-move-line-up)
(define-key global-map "\C-cj" 'my-move-line-down)

;; kill whole line
(define-key global-map "\C-c\C-dl" 'my-kill-whole-line)

;; switch between emacs windows up and down
(global-set-key "\C-c\C-wk"   (lambda () (interactive) (other-window -1)))
(global-set-key "\C-c\C-wj"   (lambda () (interactive) (other-window  1)))

;; switch between ElScreen's tabs
(global-set-key "\C-c[" (lambda () (interactive) (elscreen-previous)))
(global-set-key "\C-c]" (lambda () (interactive) (elscreen-next)))

(global-set-key "\C-c\C-re" 'my-reload-dot-emacs)

(define-key global-map "\^h\^h" 'ruby-visit-source)

;;;\-----------------

;;;/-----------------
;;; C++-Mode options

(setq c++-mode-hook
      (function (lambda ()
                  (c-set-style "stroustrup")
                  (font-lock-mode)
                  (c-set-offset 'substatement-open 0)
                  (c-set-offset 'statement-case-open 0)
                  (c-set-offset 'case-label 4)
                  (c-set-offset 'arglist-intro 2)
                  (c-set-offset 'arglist-close 0)
                  (setq tab-width 4)
                  )))
;; CC-Mode options
;; ---------------
(setq c-mode-hook
      (function (lambda ()
                  (c-set-style "stroustrup")
                  (abbrev-mode 1)
                  (auto-fill-mode 1)
                  (c-set-offset 'substatement-open 0)
                  (c-set-offset 'statement-case-open 0)
                  (c-set-offset 'case-label 4)
                  (c-set-offset 'arglist-intro 2)
                  (c-set-offset 'arglist-close 0)
                  (setq tab-width 4)
                  )))

;;;\-----------------

;;;/-----------------
;;; Ruby settings

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-x\C-e" ; toggle ruby-electric mode
              (ruby-electric-mode))))

;; enable auto insert matching braces, "end"s etc
(require 'ruby-electric)

;; Jump to error line in ruby source from backtrace
(defun ruby-visit-source ()
  "If the current line contains text like '../src/program.rb:34', visit
that file in the other window and position point on that line."
  (interactive)
  (let* ((start-boundary (save-excursion (beginning-of-line) (point)))
         (regexp (concat "\\([ \t\n\r\"'([<{]\\|^\\)" ; non file chars or
                                        ; effective
                                        ; beginning of file
                         "\\(.+\\.rb\\):\\([0-9]+\\)")) ; file.rb:NNN
         (matchp (save-excursion
                   (end-of-line)
                   ;; if two matches on line, the second is most likely
                   ;; to be useful, so search backward.
                   (re-search-backward regexp start-boundary t))))
    (cond (matchp
           (let ((file (buffer-substring (match-beginning 2)
                                         (match-end 2)))
                 (line (buffer-substring (match-beginning 3)
                                         (match-end 3))))
                                        ; Windows: Find-file doesn't seem to work with Cygwin
                                        ; //<drive>/ format or the odd /cygdrive/<drive>/ format
             (if (or (string-match "//\\(.\\)\\(.*\\)" file)
                     (string-match "/cygdrive/\\(.\\)\\(.*\\)" file))
                 (setq file
                       (concat (substring file
                                          (match-beginning 1)
                                          (match-end 1))
                               ":"
                               (substring file
                                          (match-beginning 2)
                                          (match-end 2)))))

             (find-file-other-window file)
             (goto-line (string-to-int line))))
          (t
           (error "No ruby location on line.")))))

;; Turn on hilight on YAML files
(add-to-list 'auto-mode-alist '("\\.yaml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . ruby-mode))

;;; Ruby on Rails
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))
(require 'rails)

; hilight for .rhtml files
(require 'arorem-rhtml)

;;;\-----------------

;;;/-----------------
;;; iBuffer

;; default groups for ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("C-source" (mode . c-mode))
               ("C++-source" (mode . c++-mode))
               ("Ruby-source" (mode . ruby-mode))
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
                       (name . "^\\*cvs.*\\*$")
                       (name . "^\\*[Vv][Cc].*\\*$")))
              ))))

;; ibuffer, I like my buffers to be grouped
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups
             "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;\---------------------

(load-file "/usr/share/emacs/site-lisp/xcscope.el")
(require 'xcscope)

;;;/-----------------
;;; ERC settings

(setq erc-server-conding-system '(windows-1251 . windows-1251))
      ;;;erc-encoding-coding-alist '(("#test" . windows-1251)))

;;;\----------------

(setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(highlight-current-line-globally t nil (highlight-current-line))
 '(highlight-current-line-whole-line t)
 '(ibuffer-enable t)
 '(mail-archive-file-name "~/Mail/sent")
 '(mail-default-directory "~/Mail")
 '(mail-from-style (quote angles))
 '(mail-host-address "c71.sam-solutions.net")
 '(mail-source-primary-source (quote nnml))
 '(mail-user-agent (quote gnus-user-agent))
 '(read-mail-command (quote gnus))
 ;;'(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tabbar-mode nil nil (tabbar))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(user-full-name "Evgeny Sokolov")
 '(user-mail-address "e.sokolov@sam-solutions.net"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "grey20" :foreground "grey80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "xos4-terminus"))))
 '(cscope-line-number-face ((((class color) (background dark)) (:foreground "red1"))))
 '(cursor ((t (:background "orange"))))
 '(custom-button ((((type x w32 mac) (class color)) (:background "grey70" :foreground "black" :box 1))))
 '(custom-button-mouse ((((type x w32 mac) (class color)) (:background "grey90" :foreground "black" :box 1))))
 '(custom-button-pressed ((((type x w32 mac) (class color)) (:background "white" :foreground "grey" :box 2))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(ediff-current-diff-A ((((class color) (min-colors 16)) (:background "gray30" :foreground "white"))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "gray40" :foreground "white"))))
 '(ediff-even-diff-A ((((class color) (min-colors 16)) (:background "white" :foreground "Black" :inverse-video t))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "black" :foreground "White"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "black" :foreground "White"))))
 '(ediff-even-diff-C ((((class color) (min-colors 16)) (:background "white" :foreground "Black" :inverse-video t))))
 '(ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "sky blue" :foreground "Navy" :inverse-video nil))))
 '(ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "skyblue3" :foreground "black"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "black" :foreground "light grey"))))
 '(ediff-odd-diff-B ((((class color) (min-colors 16)) (:background "light grey" :foreground "Black" :inverse-video t))))
 '(ediff-odd-diff-C ((((class color) (min-colors 16)) (:background "Black" :foreground "light grey"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background light)) (:foreground "Orchid4"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "#43d392"))))
 '(font-lock-constant-face ((nil (:foreground "palegreen3"))))
 '(font-lock-doc-face ((t (:foreground "steelblue2"))))
 '(font-lock-function-name-face ((nil (:foreground "lightblue" :weight bold))))
 '(font-lock-keyword-face ((nil (:foreground "#f1f1b1" :weight bold))))
 '(font-lock-string-face ((nil (:foreground "#fff6b5"))))
 '(highlight-current-line-face ((t (:background "grey25"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "grey55" :foreground "black" :box 1))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40")))))
 '(smerge-refined-change ((t (:background "yellow" :foreground "black"))))
 '(tooltip ((((class color)) (:background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "red1" :foreground "white"))))
 ;;;(custom-set-variables
 '(delete-selection-mode t)
 '(fill-column 90)
 '(kill-whole-line t)
 '(tab-always-indent t)
 '(indent-tabs-mode nil)
 '(transient-mark-mode t)
 '(tab-width 2)
 '(safe-local-variable-values (quote (
                                      (c-file-offsets (substatement-open . 0))
                                      (prompt-to-byte-compile)
                                      (c-indentation-style . k&r)
                                      (indent-tabs-mode . 1)
                                      (folded-file . t))))
 '(use-dialog-box nil)
 '(whitespace-global-mode nil)
 '(whitespace-modes (quote (awk-mode)))
 '(whitespace-silent t)
 '(show-paren-mode t)
 '(next-line-add-newlines t))
;;)