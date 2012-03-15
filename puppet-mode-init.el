;;
;; Setup puppet-mode for autoloading
;;
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")

(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; imenu indexer
(defun puppet-imenu-create-index ()
  (let (index)
    (goto-char (point-min))
    (while (re-search-forward "^\\(class\\|define\\|node\\)\s*\\([^\s\\({\n]+\\)[\s\\({]?" (point-max) t)
      (push (cons (match-string 2) (match-beginning 1)) index))
    (nreverse index)))

(eval-after-load "puppet-mode"
  '(progn
     (require 'puppet-flymake)

     (require 'puppet-doc)
     (require 'popwin-w3m)
     (defadvice puppet-doc (around puppet-doc-popwin-w3m activate)
       (let ((browse-url-browser-function
              'popwin:w3m-browse-url))
         ad-do-it))
     (push '("^http://docs.puppetlabs.com/references/latest/.*$" :height 0.4)
           popwin:w3m-special-display-config)
     (add-hook 'puppet-mode-hook
               '(lambda ()
                  (define-key puppet-mode-map (kbd "<C-f1>") 'puppet-doc)
                  (setq imenu-create-index-function 'puppet-imenu-create-index)
                  ))
     ))




