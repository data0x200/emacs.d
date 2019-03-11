(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json.jbuilder" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json.ruby" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(defun toggle-ruby-mode-set-encoding ()
  "set-encoding ruby-mode"
  (interactive)
  (setq ruby-insert-encoding-magic-comment (if ruby-insert-encoding-magic-comment nil t)))
(define-key ruby-mode-map "\C-ce" 'toggle-ruby-mode-set-encoding)
(setq ruby-insert-encoding-magic-comment nil)

;;========================================
;; Hooks
;;========================================
(add-hook 'ruby-mode-hook
          (lambda ()
            (progn
              (setq ruby-deep-indent-paren nil)
              (setq ruby-deep-indent-paren-style t)
              (make-local-variable 'ac-ignore-case)
              (setq ac-ignore-case nil)
              (setq ruby-insert-encoding-magic-comment nil))))
