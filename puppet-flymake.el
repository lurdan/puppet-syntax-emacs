;;;;;;;;;;;;;;;;;;;;;
;; flymake for puppet
;;

(require 'flymake)

(defcustom flymake-puppet-err-line-patterns
  '(("err: Could not parse for environment .+: \\(.+\\) at \\(.+\\):\\([0-9]+\\)" 2 3 nil 1))
  "Regexp matching Puppet error messages.")

(defconst flymake-allowed-puppet-file-name-masks
  '(("\\.pp$" flymake-puppet-init))
  "Filename extensions that switch on flymake-puppet mode syntax checks.")

(defun flymake-puppet-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "puppet" (list "parser" "validate" local-file))))

(defun flymake-puppet-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks flymake-allowed-puppet-file-name-masks)
  (setq flymake-err-line-patterns flymake-puppet-err-line-patterns)
  (flymake-mode t))

(add-hook 'puppet-mode-hook 'flymake-puppet-load)

(provide 'flymake-puppet)

