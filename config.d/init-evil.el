;;========================================
;; evil
;;========================================
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; global-config
(setq-default evil-shift-width 2)

;; esc quits
(defun evil-escape-or-quit (&optional prompt)
  (interactive)
  (cond
   ((or (evil-normal-state-p)
        (evil-insert-state-p)
        (evil-visual-state-p)
        (evil-replace-state-p)
        (evil-visual-state-p)) [escape])
   (t "C-g")))
(define-key key-translation-map (kbd "C-[") 'evil-escape-or-quit)
(define-key evil-operator-state-map (kbd "C-[") 'evil-escape-or-quit)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-insert-state-map (kbd "C-[") [escape])

(define-key minibuffer-local-map (kbd "C-[") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "C-[") 'keyboard-quit)
(define-key minibuffer-local-completion-map (kbd "C-[") [escape])
(define-key minibuffer-local-must-match-map (kbd "C-[") [escape])
(define-key minibuffer-local-isearch-map (kbd "C-[") [escape])

(define-key minibuffer-local-map [escape] 'keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-quit)

(define-key isearch-mode-map (kbd "C-w") 'backward-kill-word)

;; key mappings
;; normal map
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-n") 'next-line)
(define-key evil-normal-state-map (kbd ";") nil)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-h") 'help)
(define-key evil-normal-state-map (kbd "C-q C-t") 'toggle-cleanup-spaces)
(define-key evil-normal-state-map (kbd "SPC") ctrl-q-map)
(eval-after-load 'helm
  '(progn
     (define-key evil-normal-state-map "\C-a\C-x" 'helm-M-x)
     (define-key evil-normal-state-map "\C-a\C-a" 'helm-for-files)
     (define-key evil-normal-state-map "\C-a\C-b" 'helm-buffers-list)
     (define-key evil-normal-state-map "\C-a\C-o" 'helm-imenu)
     (define-key evil-normal-state-map "\C-a\C-g" 'helm-ls-git-ls)))

;; insert map
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-x C-s") 'save-buffer)

;; visual-map
(define-key evil-visual-state-map (kbd ";") 'evil-ex)

(define-key minibuffer-local-isearch-map (kbd "C-w") 'backward-kill-word)

(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (interactive)
            (if current-input-method
                (progn
                  (deactivate-input-method)))))
(define-key evil-insert-state-map (kbd "C-i") 'company-complete)
