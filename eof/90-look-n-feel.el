;; Solarized theme stuff

(setq current-theme 'light)

(defun toggle-solarized-dark-light ()
  "Toggle light and dark solarized themes"
  (interactive)
  (if (eq current-theme 'light)
    (progn 
      (color-theme-solarized-dark)
      (setq current-theme 'dark))
    (progn 
      (color-theme-solarized-light)
      (setq current-theme 'light))))
