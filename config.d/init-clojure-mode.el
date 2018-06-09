;;;; Clojure
(when (locate-library "clojure-mode")
  (autoload 'clojure-mode "clojure-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(provide 'init-clojure)
