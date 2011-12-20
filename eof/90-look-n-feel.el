;; Solarized

(setq current-theme 'light)
(eval-after-load "color-theme-solarized-autoloads"
  '(if window-system (progn (color-theme-solarized-light))))

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
