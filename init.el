;;;; Initialize

(package-initialize)
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'use-package)
(use-package better-defaults)

;;;; Editor

(when (window-system)
  (set-default-font "Fira Code"))

(setq inhibit-startup-message t)
(blink-cursor-mode 0)
(menu-bar-mode 1)
(setq visible-bell nil)
(setq fast-but-imprecise-scrolling t)
(set-default 'truncate-lines t)

;; Buffer settings
(setq default-indicate-empty-lines t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)

(use-package whitespace
  :config
  (progn
    (setq whitespace-style '(face
                             trailing
                             tabs
                             spaces
                             empty
                             space-mark
                             tab-mark))
    (setq whitespace-display-mappings
          '((space-mark ?\u3000 [?\u25a1])
            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
    (setq whitespace-space-regexp "\\(\u3000+\\)")
    (global-whitespace-mode 1)
    (setq-default tab-width 4 indent-tabs-mode nil)
    (add-hook 'before-save-hook 'whitespace-cleanup)))

(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

; ;; Themes

; (use-package neotree
;   :config
;   (progn
;     (setq neo-smart-open t)
;     (setq neo-window-fixed-size nil)))

; (use-package powerline)

(load-theme 'afternoon t)

; (use-package diminish
;   :config
;   (progn
;     (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
;     (eval-after-load "simple" '(diminish 'auto-fill-function))
;     (eval-after-load "eldoc" '(diminish 'eldoc-mode))
;     (eval-after-load "guide-key" '(diminish 'guide-key-mode))
;     (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
;     (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode " sln"))
;     (eval-after-load "paredit" '(diminish 'paredit-mode " par"))
;     (eval-after-load "company" '(diminish 'company-mode " cmp"))
;     (eval-after-load "cider" '(diminish 'cider-mode " cid"))
;     (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
;     (eval-after-load "helm" '(diminish 'helm-mode))))

; (eval-after-load "flyspell"
;   '(defun flyspell-mode (&optional arg)))

; ;;;; Global keybindings

; ; (global-set-key (kbd "s-x") 'kill-region)
; ; (global-set-key (kbd "s-c") 'kill-ring-save)
; ; (global-set-key (kbd "s-v") 'yank)
; ; (global-set-key (kbd "s-z") 'undo)
; ; (global-set-key (kbd "s-s") 'save-buffer)
; ; (global-set-key (kbd "s-w") 'delete-window)
; ; (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

; ;;;; Modes

(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(global-undo-tree-mode)

(use-package paredit
  :init
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))

(use-package paren-face
  :init
  (global-paren-face-mode)
  :config
  (add-hook 'clojure-mode-hook (lambda () (setq paren-face-regexp "#?[](){}[]"))))

(use-package company
  :init (global-company-mode)
  :config
  (progn
    (defun indent-or-complete ()
      (interactive)
      (if (looking-at "\\_>")
          (company-complete-common)
        (indent-according-to-mode)))
    (global-set-key "\t" 'indent-or-complete)))

; (use-package magit)

; (use-package guide-key
;   :init (guide-key-mode 1)
;   :config
;   (progn
;     (setq guide-key/idle-delay 1)
;     (setq guide-key/recursive-key-sequence-flag t)
;     (setq guide-key/popup-window-position 'bottom)))

(use-package ido)

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-faces nil))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package helm
  :config
  (progn
    (helm-mode 1)
    (setq helm-autoresize-mode t)
    (setq helm-buffer-max-length 40)
    (diminish 'helm-mode)
    (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package helm-projectile
  :config
  (progn
    (global-set-key (kbd "C-x C-f") 'helm-projectile-find-file)))

(use-package projectile
  :init (projectile-global-mode))

; (use-package yasnippet
;   :init
;   (progn
;     (yas-global-mode 1)
;     (use-package clojure-snippets)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

; (use-package markdown-mode
;   :mode (("\\.markdown$" . markdown-mode)
;          ("\\.md$" . markdown-mode))
;   :init (add-hook 'markdown-mode-hook 'auto-fill-mode))

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :config
  (progn
    (setq clojure-align-forms-automatically nil)

    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-routes 1))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2)
      (checking 2)
      (async 1))

    (define-clojure-indent
      (select 1)
      (insert 1)
      (update 1)
      (delete 1))

    (define-clojure-indent
      (run* 1)
      (fresh 1))

    (define-clojure-indent
      (extend-freeze 2)
      (extend-thaw 1))

    (define-clojure-indent
      (go-loop 1))

    (define-clojure-indent
      (this-as 1)
      (specify 1)
      (specify! 1))

    (define-clojure-indent
      (s/fdef 1))

    (setq clojure--prettify-symbols-alist
          '(("fn" . ?λ)))

    (defun toggle-nrepl-buffer ()
      "Toggle the nREPL REPL on and off"
      (interactive)
      (if (string-match "cider-repl" (buffer-name (current-buffer)))
          (delete-window)
        (cider-switch-to-repl-buffer)))

    (defun cider-save-and-refresh ()
      (interactive)
      (save-buffer)
      (call-interactively 'cider-refresh))

    (defun cider-eval-last-sexp-and-append ()
      (interactive)
      (cider-eval-last-sexp '(1)))

    (global-set-key (kbd "s-r") 'cider-save-and-refresh)))

(use-package cider
  :config
  (progn
    (setq nrepl-hide-special-buffers t)
    (setq cider-popup-stacktraces-in-repl t)
    (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
    (setq cider-repl-pop-to-buffer-on-connect t)
    (setq cider-auto-select-error-buffer nil)
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-repl-display-help-banner nil)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-refresh-before-fn "reloaded.repl/suspend")
    (setq cider-refresh-after-fn "reloaded.repl/resume")
    (setq cider-cljs-lein-repl "(do (reloaded.repl/go) (user/cljs-repl))")
    (setq cider-prompt-for-symbol nil)))

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config
  (cljr-add-keybindings-with-prefix "C-c r"))

; (use-package rustic)

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-c d") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c D") 'mc/mark-all-like-this)))
