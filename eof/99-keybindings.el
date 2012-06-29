;;;/-----------------
;;; Keybindings

(define-key global-map (kbd "C-c p") ; open magit status buffer
  'magit-status)

(define-key global-map (kbd "C-c C-c u") 'blank-mode)

(define-key global-map (kbd "C-x k")             ; kill current buffer
  (lambda () (interactive) (kill-buffer (current-buffer))))

(define-key global-map (kbd "C-S-a") 'move-beginning-of-line)
(define-key global-map (kbd "C-a") ; move to the first non-space character on the line
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

;; use ibuffer for managing buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; resize windows
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)

;; expand region selection
(global-set-key (kbd "C-@") 'er/expand-region)

(define-key global-map (kbd "C-c C-c f") 'format-patch)
