(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)

(when (not (package-installed-p 'use-package))
   (package-refresh-contents)
   (package-install 'use-package))
(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)
(require 'use-package)

;;========================================
;; FUNCTIONS
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
(setq-default header-line-format '(" %f"))

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

(tab-bar-mode t)
(customize-set-variable 'tab-bar-new-tab-choice "*scratch*")

;;;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(remove-hook 'el-get-post-install-hooks 'el-get-post-install-notification)

;;;; exec-path-from-shell
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;; theme
(use-package molokai-theme
  :config
  (when window-system
    (load-theme 'molokai t t)
    (enable-theme 'molokai)
    (set-face-background 'default "black")
    (set-face-foreground 'font-lock-doc-face "darkgray")))

(use-package highlight-parentheses
  :ensure t
  :custom (highlight-parentheses-colors '("red" "blue" "yellow" "green" "magenta" "peru" "cyan"))
  :hook ((common-lisp-mode lisp-mode-hook emacs-lisp-mode-hook) . highlight-parentheses-mode))

;;;; beacon
(use-package beacon
  :custom (beacon-color "yellow")
  :init (beacon-mode t))

;;;; golden-ratio
(use-package golden-ratio)

;;;; popwin
(use-package popwin
  :custom
  ((popwin:popup-window-height 30)
  (popwin:popup-window-width 100)
  (popwin:popup-window-position 'bottom)
  (popwin:close-popup-window-timer-interval 1.0))
  :init
  (popwin-mode t)
  (global-set-key (kbd "C-c") popwin:keymap)

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
  ;; xref
  (push '("*xref*" :dedicated t :position :bottom) popwin:special-display-config))

;;;; quickrun
(use-package quickrun
  :bind (:map ctrl-q-map ("C-q" . quickrun))
  :config
  (quickrun-add-command
    "rust/evalrs"
    '((:command . "evalrs")
      (:exec . ("cat %s | %c %a")))
    :default "evalrs"))

;;;; undo-tree
(use-package undo-tree
  :custom  (undo-tree-auto-save-history nil)
  :init (global-undo-tree-mode))

;;; flycheck
(use-package flycheck)

;;; flymake-diagnostic-at-point
(use-package flymake-diagnostic-at-point
   :after flymake
   :hook (flymake-mode . flymake-diagnostic-at-point-mode))

;;;; powerline
(use-package powerline
  :config
  (powerline-center-evil-theme))

;;;; RFC
(use-package rfc-mode
  :custom
  (rfc-mode-directory (expand-file-name "~/.emacs.d/rfc")))

;;;; editor-config
(use-package editorconfig
  :init
  (editorconfig-mode t))

(use-package request)

(use-package mermaid-mode)

(el-get-bundle shibayu36/emacs-open-github-from-here
  :features open-github-from-here)

;;========================================
;; evil
;;========================================
(use-package evil
  :custom
  ((evil-want-C-u-scroll t)
   (evil-want-C-i-jump t)
   (evil-search-module 'evil-search)
   (evil-ex-search-vim-style-regexp t)
   (evil-esc-delay 0)
   (evil-bigword "^ \_\t\r\n")
   (evil-ex-complete-emacs-commands t)
   (evil-shift-width 2))
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

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
  (define-key evil-normal-state-map (kbd "C-x C-g") 'evil-escape-or-quit)
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo)

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
  (define-key minibuffer-local-isearch-map (kbd "C-w") 'backward-kill-word)

  ;; key mappings
  ;; normal map
  (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
  (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  (define-key evil-normal-state-map (kbd ";") nil)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "C-h") 'help)
  (define-key evil-normal-state-map (kbd "C-q C-t") 'toggle-cleanup-spaces)
  (define-key evil-normal-state-map (kbd "SPC") ctrl-q-map)
  (eval-after-load 'consult
    '(progn
       (define-key evil-normal-state-map "\C-a\C-a" 'consult-recent-file)
       (define-key evil-normal-state-map "\C-a\C-b" 'consult-buffer)))
  (define-key evil-normal-state-map (kbd "C-e C-e") 'eshell)

  ;; insert map
  (define-key evil-insert-state-map (kbd "C-[") [escape])
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
  (evil-set-initial-state 'xref-mode 'emacs)
  ;; ex-command
  (evil-ex-define-cmd "q[uit]" 'tab-bar-close-tab)
  (evil-ex-define-cmd "tabe[dit]" 'tab-bar-new-tab))
(use-package evil-leader
  :after (evil)
  :config
  (evil-leader/set-leader (kbd "\\"))
  (evil-leader/set-key
    "r" 'quickrun
    "o" 'find-file-at-point))
(use-package evil-numbers)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))
(el-get-bundle! mode-line-color
  :type http
  :url "https://raw.githubusercontent.com/tarao/elisp/master/mode-line-color.el")
(el-get-bundle! evil-mode-line
  :type http
  :url "https://raw.githubusercontent.com/tarao/evil-plugins/master/evil-mode-line.el"
  :depends mode-line-color)
(use-package treemacs
  :bind ("C-c C-t" . treemacs))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))

;;========================================
;; vertico
;;========================================
(use-package vertico
  :init
  (vertico-mode)
  (savehist-mode)
  :custom
  (vertico-count 20)
  :bind (:map vertico-map ("C-w" . vertico-directory-delete-word)))
(use-package consult
  :config
  (recentf-mode)
  :bind (("C-s" . consult-line)
         :map ctrl-q-map
         ("h a" . consult-buffer)))
(use-package consult-projectile)
(use-package consult-ghq
  :bind (:map ctrl-q-map
              ("g r" . consult-projectile)
              ("g g" . consult-git-grep)))
(use-package consult-lsp)
(use-package marginalia
  :init
  (marginalia-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package embark)

;;;; git
(use-package git-gutter
  :init
  (global-git-gutter-mode t)
  (defvar git-gutter-map (make-keymap))
  (define-key global-map (kbd "C-c C-g") git-gutter-map)
  :bind (:map git-gutter-map
              ("C-n" . git-gutter:next-hunk)
              ("C-p" . git-gutter:previous-hunk)
              ("C-a" . git-gutter:stage-hunk)))
(use-package git-gutter-fringe)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

;;;; open-junk-file
(use-package open-junk-file
  :custom
  (open-junk-file-format "~/.memo/junk/%Y-%m-%d-%H%M%S."))

;;;; Company mode
(use-package company
  :init
  (global-company-mode t)
  :custom
  (company-tooltip-limit 10)
  (company-idle-delay .1)
  (company-echo-delay 0)
  (company-begin-commands '(self-insert-command))
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 2)
  :bind
  (:map evil-insert-state-map
              ("C-c C-e" . company-complete)
              ([(control return)] . company-complete))
  (:map company-active-map
              ;; C-n, C-pで補完候補を次/前の候補を選択
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ;; C-sで絞り込む
              ("C-s" . company-filter-candidates)
              ;; TABで候補を設定
              ("C-i" . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("C-h". nil)
              ("C-w". nil))
  :custom-face
   (company-preview
     ((t (:foreground "darkgray" :underline t))))
   (company-preview-common
     ((t (:inherit company-preview))))
   (company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   (company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   (company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   (company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection)))))

;; LSP
(use-package lsp-mode
  :init
  :hook (((rust-mode
           scss-mode
           c-mode
           dart-mode
           terraform-mode
           kotlin-mode
           ruby-mode) . lsp)
         (lsp-mode . (lambda () (let ((lsp-keymap-prefix "M-l"))
                                  lsp-enable-which-key-integration))))
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-clippy-preference "on")
  (lsp-modeline-code-actions-segments '(count icon name))
  :commands lsp
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map))
(use-package lsp-ui
  :after (lsp)
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (("C-c C-l C-z" . lsp-ui-doc-focus-frame))
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c C-l C-l") (lambda ()
                                                    (interactive)
                                                    (lsp-ui-doc-show)
                                                    (when (lsp-ui-doc--visible-p)
                                                      (lsp-ui-doc-focus-frame)))))
(use-package lsp-dart
  :custom
  (lsp-dart-flutter-widget-guides nil))

;; which-keyboard
(use-package which-key
  :init
  (which-key-mode))

;; magit
(use-package magit)

;; Programming Language

;;;; tree-sitter
(use-package tree-sitter
  :ensure t
  :after (tree-sitter-langs)
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

;;;; JavaScript
(use-package prettier-js
  :config
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  (add-hook 'web-mode-hook (lambda ()
                              (enable-minor-mode
                               '("\\.jsx?$" . prettier-js-mode)))))
(use-package typescript-mode
  :hook (typescript-mode . prettier-js-mode)
  :mode (("\\.ts$" . typescript-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :config
  :hook
  (before-save . tide-format-before-save)
  (typescript-mode . (lambda ()
                            (interactive)
                            (tide-setup)
                            (flycheck-mode +1)
                            (eldoc-mode +1)
                            (tide-hl-identifier-mode +1)
                            (company-mode +1))))

;;;; GraphQL
(use-package graphql-mode)

;;;; Lua
(use-package lua-mode)

;;;; Markdown
(use-package markdown-mode
  :mode
  (("\\.markdown$" . gfm-mode)
   ("\\.mkd$" . gfm-mode)
   ("\\.md$" . gfm-mode))
  :config
  (set-face-attribute 'markdown-code-face nil :inherit 'default :weight 'bold)
  (set-face-foreground 'markdown-code-face "lightsteelblue3"))

;;;; Rust
(use-package rust-mode
  :custom (rust-format-on-save t))
(use-package cargo)
(use-package toml-mode)
(use-package flycheck-rust
  :hook
  (rust-mode . flycheck-rust-setup))

;; Ruby
(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Berksfile" . ruby-mode)
         ("Guardfile" . ruby-mode)
         ("config.ru" . ruby-mode)
         ("\\.ruby$" . ruby-mode)
         ("\\.gemspec" . ruby-mode)
         ("\\.json.jbuilder" . ruby-mode)
         ("\\.json.ruby" . ruby-mode))
  :interpreter (("ruby" . ruby-mode))
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :hook
  (ruby-mode . (lambda ()
                 (progn
                   (flycheck-mode 1)
                   (flymake-mode nil)
                   (evil-matchit-mode)
                   (setq ruby-deep-indent-paren nil)
                   (setq ruby-deep-indent-paren-style t)
                   (setq ruby-insert-encoding-magic-comment nil)))))

(use-package rubocop
  :after (ruby-mode)
  :config
  (setq rubocop-autocorrect-command "rubocop -A --format emacs")
  (define-key ruby-mode-map (kbd "C-c C-e") 'rubocop-autocorrect-current-file))
;;;; flycheck ruby
;; https://github.com/flycheck/flycheck/issues/1223#issuecomment-283021487
;; .dir-locals.el
;; ((ruby-mode . ((eval . (setq flycheck-command-wrapper-function
;;                            (lambda (command)
;;                              (append '("bundle" "exec") command)))))))
(el-get-bundle! emacsmirror/ruby-block
  (ruby-block-mode t))
(use-package ruby-electric
  :config
  (setq ruby-electric-expand-delimiters-list nil)
  :hook
  (ruby-mode . (lambda () ruby-electric-mode t))
  (rspec-mode . (lambda () ruby-electric-mode t)))
(use-package rspec-mode
  :bind (:map ruby-mode-map
         ("C-c C-," . rspec-verify)
         ("C-c C-." . rspec-verify-single)
         ("C-c , t" . rspec-toggle-spec-and-target)
         :map rspec-mode-map
         ("C-c , t" . rspec-toggle-spec-and-target)))
(use-package slim-mode)
(use-package haml-mode)
(use-package rufo
  :custom
  (rufo-minor-mode-use-bundler t)
  :hook
  (ruby-mode . rufo-minor-mode))

;;;; CSS
(defun css-indent-hook()
  (setq indent-tabs-mode nil))
(add-hook 'css-mode-hook 'css-indent-hook)

(use-package scss-mode
  :mode
  (("\\.scss$" . scss-mode))
  :custom
  (css-indent-offset 2)
  (indent-tabs-mode nil)
  (scss-compile-at-save nil)
  :hook
  (scss-mode . (lambda()
                 (flycheck-mode t))))

(use-package web-mode
  :mode
  (("\\.erb$" . web-mode)
   ("\\.es6$" . web-mode)
   ("\\.hbs$" . web-mode)
   ("\\.html$" . web-mode)
   ("\\.js[x]?$" . web-mode)
   ("\\.tag$" . web-mode)
   ("\\.json$" . web-mode)
   ("\\.tsx$" . web-mode)
   ("\\.vue$" . web-mode)
   ("\\.ctp$" . web-mode))
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-auto-close-style 1)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))
(use-package mmm-mode
  :custom
  (mmm-global-mode 'maybe)
  (mmm-submode-decoration-level 0)
  :config
  (mmm-add-classes
   '((vue-embeded-slim-mode
      :submode slim-mode
      :front "^<template.*lang=\"pug\">\n"
      :back "^</template>")
     (vue-embeded-web-mode
      :submode web-mode
      :front "^<template>\n"
      :back "^</template>\n")
     (vue-embeded-ts-mode
      :submode web-mode
      :front "^<script.*lang=\"ts\">\n"
      :back "^</script>")
     (vue-embeded-js-mode
      :submode web-mode
      :front "^<script>\n"
      :back "^</script>")
     (vue-embeded-scss-mode
      :submode scss-mode
      :front "^<style.*lang=\"scss\">\n"
      :back "^</style>")))

  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-slim-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-web-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-ts-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-js-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-scss-mode))

(use-package emmet-mode
  :after (evil)
  :custom
  (emmet-move-cursor-between-quotes t)
  :hook
  (web-mode . emmet-mode)
  :bind (:map evil-insert-state-map
              ("C-c C-," . emmet-expand-line)))

;;;; YAML
(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)))
(use-package indent-guide
  :config
  (indent-guide-global-mode))

;;;; Yasnippet
(use-package yasnippet
  :init
  (yas-global-mode t))
(use-package yasnippet-snippets
  :after (yasnippet))

;;;; Go
(use-package go-mode
  :init
  (setenv "GOPATH" (expand-file-name "~/"))
  :mode
  (("\\.go" . go-mode))
  :hook
  (go-mode . (lambda ()
                    (add-hook 'before-save-hook 'lsp-format-buffer t t)
                    (add-hook 'before-save-hook 'lsp-organize-imports t t)))
  :config
  (font-lock-add-keywords
   'go-mode
   '(("\\b\\(err\\)\\b" 1 '((:foreground "yellow") (:weight bold)) t)))
  (let ((golint-emacs (concat (getenv "GOPATH") "src/github.com/golang/lint/misc/emacs")))
    (when (file-exists-p golint-emacs)
      (add-to-list 'load-path golint-emacs)
      (require 'golint))))
(use-package go-eldoc
  :hook
  (go-mode . go-eldoc-setup)
  :config
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

;;;; C
;(use-package clang-format
;  :config
;  (defun my/clang-format ()
;    (interactive)
;    (when (eq major-mode 'c-mode)
;      (clang-format-buffer)))
;  (defun toggle-clang-format-hook ()
;    (interactive)
;    (cond ((memq 'my/clang-format before-save-hook)
;           (remove-hook 'before-save-hook 'my/clang-format))
;          (t
;           (add-hook 'before-save-hook 'my/clang-format))))
;  (add-hook 'before-save-hook 'my/clang-format))

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

;;;; Docker
(use-package docker)
(use-package dockerfile-mode)

;;;; Terraform
(use-package terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))

;;;; CSV
(use-package csv-mode)

;;;; Flutter
(use-package dart-mode
  :custom
  (dart-format-on-save t))
(use-package dart-server
  :config
  (customize-set-variable 'dart-server-format-on-save t))
(use-package flutter
  :after
  (dart-mode)
  :custom
  (flutter-sdk-path "/opt/flutter")
  :bind (:map dart-mode-map
              ("C-q C-r" . 'flutter-run-or-hot-reload)))

;;;; Kotlin
(use-package kotlin-mode)
  ;;; 現在のファイルをIntelliJで開く
(defun open-by-idea ()
  (interactive)
  (when (linux-p)
    (shell-command
     (format "idea --line %d %s >/dev/null 2>&1"
             (line-number-at-pos)
             (buffer-file-name)))))
(use-package flycheck-kotlin
  :hook
  (kotlin-mode . flycheck-kotlin-setup))

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
                           :family "HackGen"
                           :height 140)
       (setq face-font-rescale-alist
             '(("Noto Sans Mono CJK JP" . 1.0)))
       (set-fontset-font t
                         'japanese-jisx0208
                         (font-spec :family "HackGen")))
      ((linux-p)

       (cond ((< 1920 (apply 'max (cdr (assoc 'geometry (car (display-monitor-attributes-list))))))
              (set-face-attribute 'default nil
                                  :family "HackGen"
                                  :height 150)

              (set-fontset-font t
                                'japanese-jisx0208
                                (font-spec :family "HackGen" :size 21)))
             (t
              (set-face-attribute 'default nil
                                  :family "HackGen"
                                  :height 130)
              (set-fontset-font t
                                'japanese-jisx0208
                                (font-spec :family "HackGen" :size 17))))))

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

(show-paren-mode t)

;;;; linum
(if (version<= "26.0.50" emacs-version)
    (add-hook 'prog-mode-hook #'display-line-numbers-mode))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b494aae329f000b68aa16737ca1de482e239d44da9486e8d45800fd6fd636780" default))
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter flycheck-kotlin consult-ghq graphql-mode magit treemacs-evil kotlin-mode terraform-mode go-eldoc go-mode yasnippet-snippets yasnippet indent-guide yaml-mode emmet-mode mmm-mode web-mode scss-mode rufo haml-mode slim-mode rspec-mode ruby-block flycheck-rust rust-mode lua-mode tide typescript-mode prettier-js lsp-dart lsp-ui which-key company company-mode open-junk-file git-gutter consult-lsp embark orderless marginalia consult vertico vertica-snippets queue csv-mode))
 '(tab-bar-new-tab-choice "*scratch*"))
