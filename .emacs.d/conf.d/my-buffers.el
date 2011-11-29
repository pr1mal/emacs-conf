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
