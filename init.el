;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib nil t)

;;========================================
;; Functions
;;========================================
(defun mac-os-p ()
  (member window-system '(mac ns)))
(defun linux-p ()
  (eq window-system 'x))
(defun windows-p ()
  (eq window-system 'w32))

;; .init.elを読み込む
(defun reload-dot-emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; copied from rails-lib.el
;; camelcase<->snakecase
(defun ik:decamelize (string)
  "Convert from CamelCaseString to camel_case_string."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2"
      (replace-regexp-in-string
       "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2"
       string)))))

(defun ik:camerize<->decamelize-on-region (s e)
  (interactive "r")
  (let ((buf-str (buffer-substring-no-properties s e))
        (case-fold-search nil))
    (cond
     ((string-match "_" buf-str)
      (let* ((los (mapcar 'capitalize (split-string buf-str "_" t)))
             (str (mapconcat 'identity los "")))
        ;; snake case to camel case
        (delete-region s e)
        (insert str)))
     (t
      (let ((str (ik:decamelize buf-str)))
        ;; snake case to camel case
        (delete-region s e)
        (insert str))))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun rename-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (or filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; 保存時に行末のスペースを削除
;; 行末のスペース + ファイル末尾の連続する改行の除去を行う
(defun my/cleanup-for-spaces ()
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))
(add-hook 'before-save-hook 'my/cleanup-for-spaces)

;; 保存時の行末のスペース削除設定を切り替える
(defvar my/current-cleanup-state "")
(defun toggle-cleanup-spaces ()
  (interactive)
  (cond ((memq 'my/cleanup-for-spaces before-save-hook)
         (setq my/current-cleanup-state
               (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
         (remove-hook 'before-save-hook 'my/cleanup-for-spaces))
        (t
         (setq my/current-cleanup-state "")
         (add-hook 'before-save-hook 'my/cleanup-for-spaces)))
  (force-mode-line-update))
(global-set-key (kbd "M-g M-d") 'toggle-cleanup-spaces)
(setq-default mode-line-format
              (cons '(:eval my/current-cleanup-state)
                    mode-line-format))

;; daily-memo
(defun my/memo ()
  (interactive)
  (find-file-other-window (expand-file-name "~/.memo/memo.org")))

;;========================================
;; General Config
;;========================================

(when (mac-os-p)
  (setq ssl-program-name "gnutls-cli"
        ssl-program-arguments '("-p" service host)
        ssl-certificate-verification-policy 1))

;; Set language to Japanese
(set-language-environment 'Japanese)

;; Use UTF-8
(prefer-coding-system 'utf-8)

;; User Settings
(setq user-full-name "Daichi Takamiya")
(setq user-mail-address "data0x200@gmail.com")

;; don't kill *scratch*
(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;; empty **Scratch** buffer
(setq initial-scratch-message "")

;; Mode line Settings
(setq-default header-line-format '("%f"))

;; font-lock-modeをonにする
(global-font-lock-mode t)

;; no message startup
(setq inhibit-startup-message t)

;; Don't create backup files
(setq make-backup-files nil)

;; Don't auto save
(setq auto-save-default nil)

;; use auto-revert-mode
(global-auto-revert-mode t)

;; highlight current line
(global-hl-line-mode t)

;; save history
(savehist-mode t)

;; show EOL
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; mode-line
(display-time)
(line-number-mode t)
(column-number-mode t)

;; region coloring
(transient-mark-mode t)

(setq gc-cons-threshold (* 10 gc-cons-threshold))

(setq message-log-max 10000)

(setq enable-recursive-minibuffers t)

(setq history-length 5000)

(setq large-file-warning-threshold (* 25 1024 1024))

(setq recentf-max-saved-items 10000)

(defalias 'yes-or-no-p 'y-or-n-p)

(blink-cursor-mode 0)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)

;; show trailing space
;; cf. http://www.clear-code.com/blog/2012/3/20.html
(when (require 'whitespace nil t)
  ;; (setq whitespace-line-column 80)
  (setq whitespace-style '(face
                           tabs
                           tab-mark
                           trailing
                           empty
                           ;; lines-tail
                           space-before-tab
                           space-after-tab))
  (set-face-foreground 'whitespace-tab "#333333")
  (set-face-background 'whitespace-tab 'nil)
  (set-face-underline  'whitespace-tab nil)
  (global-whitespace-mode 1))

;; 補完時の大文字小文字は無視
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; Indent Settings
;; Tabを使わず2文字のスペースを利用する
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; 改行コードを表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; 改行時にコメント文字も挿入
;; (global-set-key (kbd "C-j") 'indent-new-comment-line)
(global-set-key (kbd "C-m") 'newline-and-indent)

;; cua-mode
;; C-RET -> RETで矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; バッファ名の識別文字列変更
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

;; カッコの自動補完
(electric-pair-mode)

;; C-qをprefixとして利用する
(defvar ctrl-q-map (make-keymap))
(define-key global-map (kbd "C-q") ctrl-q-map)
(define-key ctrl-q-map (kbd "q") 'quoted-insert)

;; bytecompileを忘れていても新しい方を読み込んでくれる
(setq load-prefer-newer t)

;; use emacsclient
(when (not (windows-p))
  (when (require 'server nil t)
    (unless (server-running-p)
      (server-start))))

(setq switch-to-buffer-preserve-window-point nil)

;;========================================
;; el-get(packages)
;;========================================

;; General
;;;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;;; exec-path-from-shell
(el-get-bundle! exec-path-from-shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;; input method
(when (linux-p)
  (when (require 'mozc nil t)
    (setq default-input-method "japanese-mozc")
    (setq mozc-candidate-style 'overlay))
  (global-set-key (kbd "C-'") 'toggle-input-method))

;;;; theme
(el-get-bundle hbin/molokai-theme
  :description "Yet another molokai theme for Emacs 24."
  :post-init (add-to-list 'custom-theme-load-path default-directory)
  (when window-system
    (load-theme 'molokai t t)
    (enable-theme 'molokai)
    (set-face-background 'default "black")
    (set-face-foreground 'font-lock-doc-face "darkgray")))

(el-get-bundle! highlight-parentheses
  (setq hl-paren-colors '("red" "blue" "yellow" "green" "magenta" "peru" "cyan"))
  (add-hook 'common-lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'racket-mode-hook 'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'bold))

(el-get-bundle fill-column-indicator
  (setq-default fci-rule-column 120)
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)
  (add-hook 'common-lisp-mode-hook 'fci-mode)
  (add-hook 'lisp-mode-hook 'fci-mode)
  (add-hook 'ruby-mode-hook 'fci-mode)
  (add-hook 'c-mode-hook 'fci-mode))

;; Utility
;;; Org mode
(define-key ctrl-q-map (kbd "C-m") 'org-capture)
(setq org-capture-templates
      '(("m"
         "✍ Memo"
         entry
         (file+headline "~/.memo/memo.org" "Memo")
         "* %U\n%a\n%i\n%?")
        ("t"
         "TODO"
         entry
         (file+headline "~/.memo/memo.org" "TODOs")
         "* TODO %?")))
(setq org-agenda-files '("~/.memo/memo.org"))
(define-key ctrl-q-map (kbd "a") 'org-agenda)

;;;; google translate
(el-get-bundle! google-translate
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja"))
  (define-key ctrl-q-map (kbd "t") 'google-translate-at-point))

;;;; Highlight Indent Guides
(el-get-bundle! highlight-indent-guides in highlight-indentation-guides
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive t)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

;;;; beacon
(el-get-bundle! beacon
  (setq beacon-color "yellow")
  (beacon-mode t))

;;;; twittering-mode
(el-get-bundle twittering-mode)

;; ========================================
;; popwin
;; ========================================
(el-get-bundle! popwin
  (popwin-mode t)
  (setq popwin:popup-window-height 30)
  (setq popwin:popup-window-width 100)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:close-popup-window-timer-interval 1.0)

  (global-set-key (kbd "C-c") popwin:keymap)

  (setq helm-samewindow nil)
  (push '("*quickrun*" :position :right) popwin:special-display-config)
  (push '("*compilation*" :position :right) popwin:special-display-config)
  (push '(" *auto-async-byte-compile*") popwin:special-display-config)
  (push '("*git now*") popwin:special-display-config)
  (push '("*git-gutter+-diff*") popwin:special-display-config)
  (push '("\\`\\*eshell" :regexp t :dedicated t :position bottom :height 0.3) popwin:special-display-config)
  ;; (push '("^\*helm .+\*$" :regexp t :position :right) popwin:special-display-config)
  ;; Sly
  (push 'mrepl-mode popwin:special-display-config)
  ;; Scheme
  (push '"*scheme*" popwin:special-display-config)
  ;; Rubocop
  (push '("^\*RuboCop.*$" :dedicated t :regexp t :position :bottom :height 0.2) popwin:special-display-config)
  ;; Google Translate
  (push '("*Google Translate" :position :right) popwin:special-display-config)
  ;; Rust
  (push '("*rustfmt*" :position :right) popwin:special-display-config))

;;;; quicrun
(el-get-bundle quickrun
  (define-key ctrl-q-map (kbd "C-q") 'quickrun))

;;;; elscreen
(el-get-bundle! elscreen
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  (set-face-background 'elscreen-tab-current-screen-face "gray85")
  (set-face-background 'elscreen-tab-other-screen-face "gray40")
  (elscreen-start))

;;========================================
;; evil
;;========================================
(el-get-bundle evil
  :before (setq evil-want-C-u-scroll t
                evil-want-C-i-jump t
                evil-search-module 'evil-search
                evil-ex-search-vim-style-regexp t
                evil-esc-delay 0
                evil-bigword "^ \_\t\r\n"
                evil-ex-complete-emacs-commands t)

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
       (define-key ctrl-q-map (kbd "h h") 'helm-imenu)
       (define-key evil-normal-state-map "\C-a\C-x" 'helm-M-x)
       (define-key evil-normal-state-map "\C-a\C-a" 'helm-for-files)
       (define-key evil-normal-state-map "\C-a\C-b" 'helm-buffers-list)
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
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'xref-mode 'emacs))
(el-get-bundle! evil-leader
  (evil-leader/set-leader (kbd "\\"))
  (evil-leader/set-key
    "r" 'quickrun
    "o" 'find-file-at-point))
(el-get-bundle! evil-numbers)
(el-get-bundle! evil-surround)
(el-get-bundle! emacsmirror/evil-elscreen
  :depends (evil elscreen)
  (defun close-window-or-tab ()
    (interactive)
    (if (one-window-p)
        (evil-elscreen/tab-close)
      (delete-window)))
  (define-key evil-normal-state-map (kbd "gt") 'elscreen-next)
  (define-key evil-normal-state-map (kbd "gT") 'elscreen-previous)
  (evil-ex-define-cmd "q" 'close-window-or-tab))
(el-get-bundle! evil-matchit
  (global-evil-matchit-mode t))
(el-get-bundle! mode-line-color
  :type http
  :url "https://raw.githubusercontent.com/tarao/elisp/master/mode-line-color.el")
(el-get-bundle! evil-mode-line
  :type http
  :url "https://raw.githubusercontent.com/tarao/evil-plugins/master/evil-mode-line.el"
  :depends mode-line-color)

;;;; helm
;;========================================
;; Helm
;;========================================
(el-get-bundle helm
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
  (define-key ctrl-q-map (kbd "G") 'helm-google-suggest))

(el-get-bundle helm-git-grep)
(el-get-bundle helm-ghq)
(el-get-bundle projectile)
(el-get-bundle helm-projectile
  :depends (helm projectile)
  (define-key ctrl-q-map (kbd "C-g") 'helm-projectile)
  (define-key ctrl-q-map (kbd "h g") 'helm-projectile-switch-project)
  (define-key ctrl-q-map (kbd "h t") 'helm-projectile-switch-to-eshell)
  (define-key ctrl-q-map (kbd "g r") 'helm-projectile)

  (eval-after-load 'helm
    '(progn
       (require 'helm-locate)
       (define-key helm-generic-files-map (kbd "C-w") 'backward-kill-word)))
  (projectile-mode)

  (custom-set-variables
   '(helm-projectile-sources-list '(helm-source-projectile-buffers-list
                                    helm-source-projectile-recentf-list
                                    helm-source-projectile-files-list))))

;;;; git
(el-get-bundle git-gutter
  (global-git-gutter-mode t)
  (defvar git-gutter-map (make-keymap))
  (define-key global-map (kbd "C-c C-g") git-gutter-map)
  (define-key git-gutter-map (kbd "C-n") 'git-gutter:next-hunk)
  (define-key git-gutter-map (kbd "C-p") 'git-gutter:previous-hunk)
  (define-key git-gutter-map (kbd "C-a") 'git-gutter:stage-hunk))
(el-get-bundle! git-gutter-fringe)

;;;; flymake
(el-get-bundle! meqif/flymake-diagnostic-at-point
  :features (flymake-diagnostic-at-point)
  :depends (popup)
  (add-hook 'flymake-mode-hook 'flymake-diagnostic-at-point-mode))

;;;; open-junk-file
(el-get-bundle open-junk-file
  (setq open-junk-file-format "~/.memo/junk/%Y-%m-%d-%H%M%S."))

;;;; Company mode
(el-get-bundle company-mode/company-mode
  (global-company-mode t)
  (setq company-auto-expand t)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 2)

  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-w") nil)

  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))
(el-get-bundle lsp-mode)
(el-get-bundle lsp-ui
  (add-hook 'rust-mode-hook 'lsp)
  (add-hook 'php-mode-hook 'lsp)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(el-get-bundle tigersoldier/company-lsp
  :depends (company-mode lsp-mode s)
  (push 'company-lsp company-backends))

;; Programming Language

;;;; Common Lisp
(when (linux-p)
  (el-get-bundle slime/slime
    (load (expand-file-name "~/.roswell/helper.el"))
    (defun slime-qlot-exec (directory)
      (interactive (list (read-directory-name "Project directory: ")))
      (slime-start :program "qlot"
                   :program-args '("exec" "ros" "-S" "." "run")
                   :directory directory
                   :name 'qlot
                   :env (list (concat "PATH="
                                      (mapconcat 'identity exec-path ":"))
                              (concat "QUICKLISP_HOME="
                                      (file-name-as-directory directory) "quicklisp/")))))

  (el-get-bundle! hyperspec
    :type http
    :url "http://www.naggum.no/emacs/hyperspec.el"
    (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "~/.emacs.d/docs/HyperSpec/")))
    (setq common-lisp-hyperspec-symbol-table (expand-file-name "~/.emacs.d/docs/HyperSpec/Data/Map_Sym.txt"))))

;;;; JavaScript

(el-get-bundle prettier-js
  (setq prettier-js-args '("--prose-wrap" "never"
                           "--jsx-bracket-same-line" "false"))
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  (add-hook 'web-mode-hook '(lambda ()
                              (enable-minor-mode
                               '("\\.jsx?\\'" . prettier-js-mode)))))

;;;; Lua

(el-get-bundle lua-mode
  (add-to-list 'auto-mode-alist '("\\.lua" . lua-mode)))

;;;; Markdown
(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mkd$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

;;;; Rust

(el-get-bundle rust-mode
  (setq rust-format-on-save t))
(el-get-bundle toml-mode)

;; Ruby
(el-get-bundle! ruby-mode

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
                (lsp)
                (setq ruby-deep-indent-paren nil)
                (setq ruby-deep-indent-paren-style t)
                (setq ruby-insert-encoding-magic-comment nil)))))
(el-get-bundle! rubocop
  (define-key ruby-mode-map (kbd "C-c C-e") 'rubocop-autocorrect-current-file))
(el-get-bundle! emacsmirror/ruby-block
  (ruby-block-mode t))
(el-get-bundle! ruby-electric
  (setq ruby-electric-expand-delimiters-list nil)
  (add-hook 'ruby-mode-hook
            (lambda ()
              ruby-electric-mode t))
  (add-hook 'rspec-mode-hook
            (lambda ()
              ruby-electric-mode t)))
(el-get-bundle! rspec-mode
  (define-key ruby-mode-map (kbd "C-c C-,") 'rspec-verify)
  (define-key ruby-mode-map (kbd "C-c C-.") 'rspec-verify-single)
  (define-key ruby-mode-map (kbd "C-c , t") 'rspec-toggle-spec-and-target)
  (define-key rspec-mode-map (kbd "C-c , t") 'rspec-toggle-spec-and-target))

;;;; CSS
(defun css-indent-hook()
  (setq indent-tabs-mode nil)
  (setq css-basic-offset 2)
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'css-indent-hook)

(el-get-bundle scss-mode
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (defun scss-hook ()
    (setq indent-tabs-mode nil)
    (setq css-basic-offset 2)
    (setq css-indent-offset 2)
    (setq scss-compile-at-save nil))
  (add-hook 'scss-mode-hook 'scss-hook))

(el-get-bundle web-mode
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.es6$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tag$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-enable-auto-quoting nil)
                             (setq web-mode-auto-close-style 1)
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-css-indent-offset 2))))

;;;; YAML
(el-get-bundle yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)))

;;;; Yasnippet
(el-get-bundle! yasnippet
  (push (locate-user-emacs-file "snippets") yas-snippet-dirs)
  (yas-global-mode t))

;;;; Go
(el-get-bundle go-mode
  (setenv "GOPATH" (expand-file-name "~/"))
  (add-to-list 'auto-mode-alist '("\\.go" . go-mode))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (evil-define-key 'normal go-mode-map (kbd "C-]")
    'godef-jump)
  (font-lock-add-keywords
   'go-mode
   '(("\\b\\(err\\)\\b" 1 '((:foreground "yellow") (:weight bold)) t)))
  (let ((golint-emacs (concat (getenv "GOPATH") "src/github.com/golang/lint/misc/emacs")))
    (when (file-exists-p golint-emacs)
      (add-to-list 'load-path golint-emacs)
      (require 'golint)))
  (let ((gocode-emacs-company (concat (getenv "GOPATH") "src/github.com/nsf/gocode/emacs-company")))
    (when (file-exists-p gocode-emacs-company)
      (add-to-list 'load-path gocode-emacs-company)
      (when (require 'company-go nil t)
        (add-hook 'go-mode-hook (lambda ()
                                  (set (make-local-variable 'company-backends) '(company-go))
                                  (company-mode)))))))
(el-get-bundle! go-eldoc
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

;;;; Racket
(el-get-bundle! racket-mode
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run))))

;;;; PHP
(el-get-bundle php-mode)

;;;; Swift
(el-get-bundle swift-mode)

;;;; C
(el-get-bundle! clang-format
  (defun my/clang-format ()
    (interactive)
    (when (eq major-mode 'c-mode)
      (clang-format-buffer)))
  (defun toggle-clang-format-hook ()
    (interactive)
    (cond ((memq 'my/clang-format before-save-hook)
           (remove-hook 'before-save-hook 'my/clang-format))
          (t
           (add-hook 'before-save-hook 'my/clang-format))))
  (add-hook 'before-save-hook 'my/clang-format))

;;;; Java
(defun java-indent-hook()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))
(add-hook 'java-mode-hook 'java-indent-hook)

;;;; PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD$" . sh-mode))
(defun sh-indent-hook ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))
(add-hook 'sh-mode-hook 'sh-indent-hook)


;; ========================================
;; Font Settings
;; ========================================
;; (prin1 (font-family-list))
;; 調整用
;; | 数字 | アルファベット | 日本語     |
;; | 0123 | abcdefghijklmn | こんにちは |
(setq-default line-spacing 1)
(cond ((mac-os-p)
       (set-face-attribute 'default
                           nil
                           :family "Noto Sans Mono CJK JP"
                           :height 140)
       (setq face-font-rescale-alist
             '(("Noto Sans Mono CJK JP" . 1.0)))
       (set-fontset-font t
                         'japanese-jisx0208
                         (font-spec :family "Noto Sans Mono CJK JP")))
      ((linux-p)
       (set-face-attribute 'default nil
                           :family "Noto Sans Mono CJK JP"
                           :height 120)
       (setq face-font-rescale-alist
             '(("Noto Sans Mono CJK JP" . 1.0)))
       (set-fontset-font nil
                         'japanese-jisx0208
                         (font-spec :family "Noto Sans Mono CJK JP" :size 16))))

;;========================================
;; Key Config
;;========================================

;;;; C-hをBackspaceにする
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-d") 'delete-char)

;;;; minibufferでC-wで前の単語を削除
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-completion-map (kbd "C-h") 'backward-delete-char)

;;;; 行頭のC-kで行全体を削除
(setq kill-whole-line t)

;;;; C-x C-oでウィンドウを切り替える
(global-set-key (kbd "C-o") 'other-window)

;;;; my work memo
(define-key ctrl-q-map (kbd "C-w") 'my/memo)

(show-paren-mode t)

;;;; linum
(when (require 'linum nil t)
  (setq linum-format " %5d"))
(global-linum-mode)
;; @see: http://d.hatena.ne.jp/daimatz/20120215/1329248780
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; ========================================
;; view-mode
;; ========================================
;; 移動をhjklで
(define-key ctrl-q-map (kbd "C-v") 'view-mode)
(add-hook 'view-mode-hook
          (lambda ()
            (progn
              ;; C-b, ←
              (define-key view-mode-map "h" 'backward-char)
              ;; C-n, ↓
              (define-key view-mode-map "j" 'next-line)
              ;; C-p, ↑
              (define-key view-mode-map "k" 'previous-line)
              ;; C-f, →
              (define-key view-mode-map "l" 'forward-char)
              )))

;; ========================================
;; doc-view-mode
;; ========================================
(add-hook 'doc-view-mode-hook
          (lambda ()
            (progn
              (define-key doc-view-mode-map "k" 'doc-view-previous-page)
              (define-key doc-view-mode-map "j" 'doc-view-next-page))))

;; ========================================
;; Custom Variables (Auto Edit)
;; ========================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(helm-projectile-sources-list
   (quote
    (helm-source-projectile-buffers-list helm-source-projectile-recentf-list helm-source-projectile-files-list)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))
