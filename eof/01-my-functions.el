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

;; wrapper around my format-patch helper script
;; TODO implement `format-patch' ruby script in elisp
(setq format-patch-last-used-config "evm")
(setq format-patch-last-used-patch-name "work-in-progress")
(defun format-patch (config patch-name)
  (interactive (list
                (read-buffer "Use config: " format-patch-last-used-config)
                (read-buffer "Patch name: " format-patch-last-used-patch-name)))
  (setq format-patch-last-used-config config
        format-patch-last-used-patch-name patch-name)
  (let  ((cmd "/usr/local/bin/format-patch -c %s %s"))
    (compile (format cmd config patch-name))))

;; saves line under cursor into kill-ring
(defun my-copy-line-under-cursor ()
  (interactive)
  (back-to-indentation)
  (push-mark (point) nil)
  (move-end-of-line nil)
  (kill-ring-save (mark) (point))
  (message "current line saved in kill-ring"))
