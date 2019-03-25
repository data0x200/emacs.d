;; ========================================
;; popwin
;; ========================================
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
;; (push '("^\*helm .+\*$" :regexp t :position :right) popwin:special-display-config)
;; Sly
(push 'mrepl-mode popwin:special-display-config)
;; Scheme
(push '"*scheme*" popwin:special-display-config)
;; Rubocop
(push '("^\*RuboCop.*$" :dedicated t :regexp t :position :bottom :height 0.2) popwin:special-display-config)
;; Google Translate
(push '("*Google Translate" :position :right) popwin:special-display-config)
