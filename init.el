
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
  (find-file-other-window (expand-file-name "~/Dropbox/memo/memo.markdown"))
  (progn
    (beginning-of-buffer)
    (next-line)
    (insert (concat "\n" "## " (format-time-string "%Y/%m/%d %H:%M:%S") " ##" "\n\n\n"))
    (previous-line)))

;; daily-memo
(defun my/work-memo ()
  (interactive)
  (find-file-other-window (expand-file-name "~/.memo/memo.markdown"))
  (progn
    (beginning-of-buffer)
    (next-line)
    (insert (concat "\n" "## " (format-time-string "%Y/%m/%d") " ##" "\n\n\n"))
    (previous-line)))

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
(setq-default tab-width 2)
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

(when (linux-p)
  (require 'uim-leim)
  (setq default-input-method "japanese-skk-uim")
  ;; Set UTF-8 as preferred character encoding (default is euc-jp).
  (setq uim-lang-code-alist
        (cons '("Japanese" "Japanese" utf-8 "UTF-8")
              (delete (assoc "Japanese" uim-lang-code-alist)
                      uim-lang-code-alist)))
  (setq uim-default-im-prop '("action_skk_hiragana"))
  (global-set-key (kbd "C-'") 'toggle-input-method))

;;========================================
;; el-get(packages)
;;========================================

;; General
;;;; Load config files
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(setq el-get-user-package-directory (locate-user-emacs-file "config.d"))

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
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "LANG")
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GOROOT")))

;;;; theme
(el-get-bundle hbin/molokai-theme
  :description "Yet another molokai theme for Emacs 24."
  :post-init (add-to-list 'custom-theme-load-path default-directory)
  (when window-system
    (load-theme 'molokai t t)
    (enable-theme 'molokai)
    (set-face-background 'default "black")))

(el-get-bundle! highlight-parentheses
  (setq hl-paren-colors '("red" "blue" "yellow" "green" "magenta" "peru" "cyan"))
  (add-hook 'common-lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'racket-mode-hook 'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'bold))

(el-get-bundle fill-column-indicator
  (setq-default fci-rule-column 80)
  (add-hook 'coffee-mode-hook 'fci-mode)
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)
  (add-hook 'common-lisp-mode-hook 'fci-mode)
  (add-hook 'lisp-mode-hook 'fci-mode)
  (add-hook 'ruby-mode-hook 'fci-mode)
  (add-hook 'c-mode-hook 'fci-mode))

;; Utility
;;;; magit
(el-get-bundle! magit)

;;;; popwin
(el-get-bundle! popwin)

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

;;;; evil
(el-get-bundle evil
  :before (setq evil-want-C-u-scroll t
                evil-want-C-i-jump t
                evil-search-module 'evil-search
                evil-ex-search-vim-style-regexp t
                evil-esc-delay 0
                evil-bigword "^ \_\t\r\n"
                evil-ex-complete-emacs-commands t))
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
(el-get-bundle helm)
(el-get-bundle helm-git-grep)
(el-get-bundle helm-ghq)
(el-get-bundle projectile)
(el-get-bundle helm-projectile
  :depends (helm projectile))

;;;; git
(el-get-bundle git-gutter
  (global-git-gutter-mode t)
  (defvar git-gutter-map (make-keymap))
  (define-key global-map (kbd "C-c C-g") git-gutter-map)
  (define-key git-gutter-map (kbd "C-n") 'git-gutter:next-hunk)
  (define-key git-gutter-map (kbd "C-p") 'git-gutter:previous-hunk)
  (define-key git-gutter-map (kbd "C-a") 'git-gutter:stage-hunk))
(el-get-bundle! git-gutter-fringe)

;;;; flycheck
(el-get-bundle! flycheck
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-eslintrc ".eslintrc.js"))

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
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
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
(el-get-bundle! tigersoldier/company-lsp
  :denpends (company-mode lsp-mode)
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
(el-get-bundle json-mode
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))
(el-get-bundle! coffee-mode
  (when (require 'quickrun nil t )
    (define-key coffee-mode-map (kbd "C-q C-q") 'quickrun-compile-only))
  (add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode)))

;;;; Lua

(el-get-bundle lua-mode
  (add-to-list 'auto-mode-alist '("\\.lua" . lua-mode)))

;;;; Markdown
(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mkd$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

;;;; Rust

(el-get-bundle! rust-mode
  (setq rust-format-on-save t))
(el-get-bundle! racer in emacs-racer
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (evil-define-key 'normal racer-mode-map (kbd "C-]")
    'racer-find-definition)
  (evil-define-key 'normal racer-mode-map (kbd "C-o")
    'pop-tag-mark))
(el-get-bundle! flycheck-rust
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))
(el-get-bundle! emacs-lsp/lsp-mode)
(el-get-bundle! emacs-lsp/lsp-rust
  :depends (lsp-mode)
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (require 'lsp-rust)
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    (add-hook 'rust-mode-hook 'lsp-rust-enable)
    (add-hook 'rust-mode-hook 'flycheck-mode)))
(el-get-bundle! toml-mode)

;; Ruby
(el-get-bundle! ruby-mode)
(el-get-bundle! rubocop)
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
(el-get-bundle robe
  (add-hook 'ruby-mode-hook (lambda()
                              (set (make-local-variable 'company-backends)
                                   '(company-robe company-yasnippet company-keywords company-dabbrev))
                              (robe-mode)
                              (company-mode))))

;;;; CSS
(defun css-indent-hook()
  (setq indent-tabs-mode nil)
  (setq css-basic-offset 2)
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'css-indent-hook)

(el-get-bundle less-css-mode
  (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode)))

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
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.es6$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-enable-auto-quoting nil)
                             (setq web-mode-auto-close-style 1)
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-css-indent-offset 2)
                             (when (or (equal web-mode-content-type "jsx") (equal web-mode-content-type "js"))
                               (flycheck-add-mode 'javascript-eslint 'web-mode)))))

;;;; YAML
(el-get-bundle yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;;;; Yasnippet
(el-get-bundle! yasnippet
  (push (locate-user-emacs-file "snippets") yas-snippet-dirs)
  (yas-global-mode t))

;;;; Go

(el-get-bundle go-mode
  (setenv "GOPATH" (expand-file-name "~/"))
  (add-to-list 'auto-mode-alist '("\\.go" . go-mode))
  (add-hook 'before-save-hook 'gofmt-before-save)
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
        (add-hook 'go-mode-hook (lambda()
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


;;;; C
(add-hook 'c-mode-hook '(lambda () (setq tab-width 4)))

;;;; Gauche
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; 以下はインデントの定義です。
(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)

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
  (sh-basic-offset 2)
  (sh-indentation 2))
(add-hook 'sh-mode-hook 'sh-indent-hook)


;; ========================================
;; Font Settings
;; ========================================
;; (prin1 (font-family-list))
;; 調整用
;; | 数字 | アルファベット | 日本語     |
;; | 0123 | abcdefghijklmn | こんにちは |
(cond ((mac-os-p)
       (set-face-attribute 'default nil
                           :family "Noto Sans Mono CJK JP"
                           :height 140)
       (setq face-font-rescale-alist
             '(("Noto Sans Japanese" . 1.0)))
       (set-fontset-font nil
                         'japanese-jisx0208
                         (font-spec :family "Noto Sans Mono CJK JP" :size 14)))
      ((windows-p)
       (set-face-attribute 'default nil
                           :family "Inconsolata"
                           :height 120)
       (set-fontset-font nil
                         'japanese-jisx0208
                         (font-spec :family "M+2VM+IPAG circle"))
       (setq face-font-rescale-alist
             '((".*Inconsolata.*" . 1.0)
               (".*M+1VM+IPAG circle.*" . 1.0))))
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

;;;; my memo
(define-key ctrl-q-map (kbd "C-m") 'my/memo)

;;;; my work memo
(define-key ctrl-q-map (kbd "C-w") 'my/work-memo)

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
          (lambda()
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
          (lambda()
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
 '(helm-projectile-sources-list
   (quote
    (helm-source-projectile-buffers-list helm-source-projectile-recentf-list helm-source-projectile-files-list)))
 '(package-selected-packages (quote (robe))))
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
