(define-key ctrl-q-map (kbd "C-g") 'helm-projectile)
(define-key ctrl-q-map (kbd "h g") 'helm-projectile)
(define-key ctrl-q-map (kbd "h t") 'helm-projectile)
(define-key ctrl-q-map (kbd "g r") 'helm-projectile)

(eval-after-load 'helm
  '(progn
     (require 'helm-locate)
     (define-key helm-generic-files-map (kbd "C-w") 'backward-kill-word)))
(projectile-global-mode)

(custom-set-variables
 '(helm-projectile-sources-list '(helm-source-projectile-buffers-list
                                  helm-source-projectile-recentf-list
                                  helm-source-projectile-files-list)))
