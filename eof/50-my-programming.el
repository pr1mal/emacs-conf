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
;; (require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("^Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^Gemfile$"  . ruby-mode))
;;;\-----------------

;;;/-----------------
;;; Erlang
(setq load-path (cons "/usr/local/Cellar/erlang/R14B04/lib/erlang/lib/tools-2.6.6.5/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/R14B04/lib/erlang")
(setq exec-path (cons "/usr/local/Cellar/erlang/R14B04/lib/erlang/bin" exec-path))
(require 'erlang-start)

