;;;; Initialize

(setq byte-compile-warnings '(not cl-functions obsolete))
(setq package-enable-at-startup nil)
(setq warning-suppress-log-types '((package reinitialization)))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)
(use-package better-defaults)

;;;; Editor

; (when (window-system)
;   (set-default-font "Fira Code"))

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

(delete-selection-mode t)
(setq shell-file-name "/bin/sh")

(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-e"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-h"))

(global-set-key (kbd "C-r") 'replace-regexp)
; (global-set-key (kbd "C-h") 'highlight-symbol-prev)

(use-package expand-region :ensure t
  :config
  (progn
   (global-set-key (kbd "C-t") 'er/expand-region)))

(use-package highlight-symbol :ensure t
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "#FA009A")
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t)
  (global-set-key (kbd "C-n") 'highlight-symbol-next)
  (global-set-key (kbd "C-p") 'highlight-symbol-prev)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))

;; consult
(setq consult-find-command "fd --color=never --full-path ARG OPTS")

(global-display-line-numbers-mode)
(global-hl-line-mode)

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

(load-theme 'planet t)

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

(use-package flycheck-joker :ensure t)
(use-package flycheck-clj-kondo :ensure t)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(use-package flycheck :ensure t
  :init (global-flycheck-mode)
  :config
  (setq
    flycheck-display-errors-delay 1
    flycheck-highlighting-mode 'lines
    flycheck-check-syntax-automatically '(save))
  (set-face-attribute 'flycheck-error nil :underline '(:color "red3" :style wave))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "orange2" :style wave)))

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

; (use-package helm
;   :config
;   (progn
;     (helm-mode 1)
;     (setq helm-autoresize-mode t)
;     (setq helm-buffer-max-length 40)
;     (diminish 'helm-mode)
;     (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package helm-projectile
  :config
  (progn
    (defun helm-projectile-find-file-in-mono-repo ()
      ""
      (interactive)
      (helm-projectile-find-file))

    (global-set-key (kbd "C-f") 'helm-projectile-find-file-in-mono-repo)
    (global-set-key (kbd "C-a") 'helm-projectile-ag)))

(use-package projectile
  :init (projectile-global-mode))

(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)
    (global-set-key (kbd "C-h") 'yas-insert-snippet)
    (use-package clojure-snippets)))

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
    (require 'flycheck-clj-kondo)
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
      ; (checking 2)
      (async 1)
      (setup/run -1)
      (it.setup/run -1))

    ; (define-clojure-indent
    ;   (select 1)
    ;   (insert 1)
    ;   (update 1)
    ;   (delete 1))

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
      (s/fdef 3))

    (define-clojure-indent
      (cond-> 1))

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
    (setq cider-prompt-for-symbol nil)
    (setq cider-test-defining-forms '("deftest" "defspec" "deftest+trace" "defspec+trace" "def-market-state-test"))))

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config
  (cljr-add-keybindings-with-prefix "C-c r"))

; (use-package rustic
;   :mode (("\\.rs$" . rustic-mode))
;   :commands (rustic-mode)
;   :config
;   (add-hook 'rustic-mode-hook
;     '(lambda ()
;       (setq rustic-lsp-server 'rust-analyzer)
;       (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;       (setq-default rustic-format-trigger 'on-save)
;       (yas-minor-mode t)
;       (rainbow-delimiters-mode t)
;       (smartparens-mode t)
;       (paredit-mode t)
;       (dumb-jump-mode t)
;       (highlight-symbol-mode t))))

(use-package quickrun
  :config
  (progn
    (global-set-key (kbd "<f5>") 'quickrun)
    (global-set-key (kbd "S-<f5>") 'quickrun-with-arg)
    (setq quickrun-focus-p nil)))

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-e") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-S-e") 'mc/mark-all-like-this)))

(use-package helm-open-github
  :config
  (progn
    (global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
    (global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
    (global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
    (global-set-key (kbd "C-c p p") 'helm-open-github-from-pull-requests)))

(use-package vertico
  :init
  (vertico-mode)

  ;; Optionally enable cycling for `vertico-next', `vertico-previous',
  ;; `vertico-next-group' and `vertico-previous-group'.
  ;; (setq vertico-cycle t)
)

;; 補完スタイルにorderlessを利用する
(with-eval-after-load 'orderless
  (setq completion-styles '(orderless basic)))

;; 補完候補を最大20行まで表示する
(setq vertico-count 20)

;; vertico-modeとmarginalia-modeを有効化する
(defun after-init-hook ()
  (vertico-mode)
  (marginalia-mode)
  ;; savehist-modeを使ってVerticoの順番を永続化する
  (savehist-mode))

(add-hook 'after-init-hook #'after-init-hook)

;; embark-consultを読み込む
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
(defun my-consult-line (&optional at-point)
  "Consult-line uses things-at-point if set C-u prefix."
  (interactive "P")
  (if at-point
      (consult-line (thing-at-point 'symbol))
    (consult-line)))

;; C-s（isearch-forward）をmy-consult-lineコマンドに割り当てる
(global-set-key (kbd "C-s") 'my-consult-line)

;; C-s/C-rで行を移動できるようにする
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))

(global-set-key (kbd "M-g") 'consult-goto-line)

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2593436c53c59d650c8e3b5337a45f0e1542b1ba46ce8956861316e860b145a0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "454c1c9ce70f7d807c51c890910365fd3c64a9e63f596511e9ff57dd97bbeea8" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" default)))
 '(package-selected-packages
   (quote
    (dakrone-theme spacemacs-theme flycheck-color-mode-line eziam-theme flycheck-clj-kondo flycheck-pos-tip highlight-symbol quickrun helm-ag yaml-mode wrap-region use-package undo-tree string-inflection smex smartparens rustic rainbow-mode rainbow-delimiters racer powerline paren-face neotree magit lsp-mode linum-relative ido-vertical-mode helm-projectile helm-open-github guide-key flx-ido expand-region eglot drag-stuff diminish company clojure-snippets clj-refactor cask better-defaults aggressive-indent ag afternoon-theme)))
 '(safe-local-variable-values (quote ((cider-shadow-cljs-default-options . "app")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
