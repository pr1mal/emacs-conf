;;;/-----------------
;;; Functions

(defun my-reload-dot-emacs ()
  "reloads .emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

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


