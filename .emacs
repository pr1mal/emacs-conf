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
(setq-default indent-tabs-mode t)
(setq save-abbrevs nil)

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

(defun todoo-or-close-todo ()
  "Call todoo buffer and close it if already in it"
  (interactive)
  (if (eq major-mode 'todoo-mode)
      (call-interactively 'todoo-save-and-exit)
    (call-interactively 'todoo)))

;;;\-----------------

;;;/-----------------
;;; Keybindings

(define-key global-map (kbd "C-c p") ; open magit status buffer
  'magit-status)

(define-key global-map [f8]             ; kill current buffer
  (lambda () (interactive) (kill-buffer (current-buffer))))

(define-key global-map [home] ; move to the first non-space character on the line
  'back-to-indentation)
(define-key global-map (kbd "RET")   ; auto-indent on pressing <enter>
  'newline-and-indent)

;; move current line up and down
(define-key global-map "\C-ck" 'my-move-line-up)
(define-key global-map "\C-cj" 'my-move-line-down)

;; kill whole line
(define-key global-map "\C-cd" 'my-kill-whole-line)

;; switch between emacs windows in reverse direction
(global-set-key (kbd "C-x O")   (lambda () (interactive) (other-window -1)))

;; reload .emacs
(global-set-key "\C-c\C-re" 'my-reload-dot-emacs)

(define-key global-map "\^h\^h" 'ruby-visit-source)

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
                  (setq indent-tabs-mode t)
                  )
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
                  (setq indent-tabs-mode t)
                  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

;;;\-----------------

;;;/-----------------
;;; Ruby settings

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-x\C-e" ; toggle ruby-electric mode
              (ruby-electric-mode))))

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
	       ("Javascript" (mode . js2-mode))
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
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
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
