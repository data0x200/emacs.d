;;========================================
;; Helm
;;========================================

;; 候補を表示するまでの時間
;; default 0.5
(setq helm-idle-delay 0.1)
;; 候補の最大表示数
;; default 50
(setq helm-candidate-number-limit 100)
;; 候補が多いときの体感速度を早める
;; チカチカしないようにする
(setq helm-quick-update nil)

(setq helm-split-window-default-side 'right)

;; key mapping
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-h") 'delete-backward-char)
     (define-key helm-map (kbd "C-w") 'backward-kill-word)
     (define-key helm-map (kbd "C-v") 'helm-next-source)
     (define-key helm-map (kbd "M-v") 'helm-previous-source)))
(eval-after-load 'helm-files
  '(progn
     (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
     (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)
     (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)))

;; key mapping
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key ctrl-q-map (kbd "C-x") 'helm-M-x)
(define-key ctrl-q-map (kbd "hx") 'helm-M-x)
(define-key ctrl-q-map (kbd "C-a") 'helm-mini)
(define-key ctrl-q-map (kbd "ha") 'helm-mini)
(define-key ctrl-q-map (kbd "g g") 'helm-git-grep-at-point)
(define-key ctrl-q-map (kbd "g G") 'helm-git-grep)
(define-key ctrl-q-map (kbd "C-o") 'helm-semantic-or-imenu)
(define-key ctrl-q-map (kbd "ho") 'helm-semantic-or-imenu)
(define-key ctrl-q-map (kbd "C-f") 'helm-find-files)
(define-key ctrl-q-map (kbd "hf") 'helm-find-files)
