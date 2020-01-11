(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
;  starter-kit 
;  starter-kit-ruby
;  starter-kit-bindings
                      
(defvar my-packages '(magit
                      blank-mode
                      lua-mode
                      rspec-mode
                      ruby-compilation
                      yaml-mode
                      js2-mode
                      full-ack
                      color-theme
                      color-theme-solarized
                      expand-region)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; things required everywhere else are initialized/defined here

(cond ((and *is-a-mac* window-system)
       (set-frame-font "Envy Code R-15"))) ;; mac emacs doesn't honor :default font settings

(setq custom-file "~/.emacs.d/custom-file.el")
(if (file-exists-p custom-file) (load custom-file))
(put 'ido-exit-minibuffer 'disabled nil)

;; Now load all my configs
(defun load-directory (dir)
      (let ((load-it (lambda (f)
		       (load-file (concat (file-name-as-directory dir) f)))
		     ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/eof/")
