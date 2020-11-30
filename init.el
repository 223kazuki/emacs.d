(setq byte-compile-warnings '(not cl-functions obsolete))

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
      (expand-file-name
       (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    (leaf multiple-cursors :ensure t)
    (leaf projectile :ensure t)
    (leaf smex :ensure t)
    (leaf yasnippet :ensure t)
    (leaf clojure-snippets :ensure t)
    (leaf yaml-mode :ensure t)
    (leaf undo-tree :ensure t)
    (leaf diminish :ensure t)
    (leaf paredit :ensure t)
    (leaf smartparens :ensure t)
    (leaf clojure-mode :ensure t)
    (leaf paren-face :ensure t)
    (leaf cider :ensure t)
    (leaf clj-refactor :ensure t)
    (leaf company :ensure t)
    (leaf use-package :ensure t)
    (leaf ag :ensure t)
    (leaf guide-key :ensure t)
    (leaf flx-ido :ensure t)
    (leaf ido-vertical-mode :ensure t)
    (leaf better-defaults :ensure t)
    (leaf aggressive-indent :ensure t)
    (leaf rainbow-mode :ensure t)
    (leaf linum-relative :ensure t)
    (leaf neotree :ensure t)
    (leaf helm :ensure t)
    (leaf helm-projectile :ensure t)
    (leaf whitespace :ensure t)
    (leaf rainbow-delimiters :ensure t)
    (leaf string-inflection :ensure t)
    (leaf helm-open-github :ensure t)
    (leaf helm-ag :ensure t)
    (leaf highlight-symbol :ensure t)
    (leaf flycheck :ensure t)
    (leaf flycheck-joker :ensure t)
    (leaf flycheck-clj-kondo :ensure t)
    (leaf afternoon-theme :ensure t)
    (leaf spacemacs-theme :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

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
(setq show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)

;; Copy and Paste from Mac
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(global-unset-key (kbd "C-e"))

(leaf whitespace
  :custom
  ((whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                    (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (whitespace-space-regexp . "\\(\u3000+\\)"))
  :config
  (progn
    (global-whitespace-mode 1)
    (setq-default tab-width 4 indent-tabs-mode nil)
    (add-hook 'before-save-hook 'whitespace-cleanup)))

(provide 'init)

; ;;;; Initialize

; (use-package whitespace
;   :config
;   (progn
;     (setq whitespace-display-mappings
;           '((space-mark ?\u3000 [?\u25a1])
;             (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;     (setq whitespace-space-regexp "\\(\u3000+\\)")
;     (global-whitespace-mode 1)
;     (setq-default tab-width 4 indent-tabs-mode nil)
;     (add-hook 'before-save-hook 'whitespace-cleanup)))



; ; ;; Themes

; ; (use-package neotree
; ;   :config
; ;   (progn
; ;     (setq neo-smart-open t)
; ;     (setq neo-window-fixed-size nil)))

; ; (use-package powerline)

; (load-theme 'spacemacs-dark t)
; (load-theme 'afternoon t)

; ; (use-package diminish
; ;   :config
; ;   (progn
; ;     (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
; ;     (eval-after-load "simple" '(diminish 'auto-fill-function))
; ;     (eval-after-load "eldoc" '(diminish 'eldoc-mode))
; ;     (eval-after-load "guide-key" '(diminish 'guide-key-mode))
; ;     (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
; ;     (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode " sln"))
; ;     (eval-after-load "paredit" '(diminish 'paredit-mode " par"))
; ;     (eval-after-load "company" '(diminish 'company-mode " cmp"))
; ;     (eval-after-load "cider" '(diminish 'cider-mode " cid"))
; ;     (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
; ;     (eval-after-load "helm" '(diminish 'helm-mode))))

; ; (eval-after-load "flyspell"
; ;   '(defun flyspell-mode (&optional arg)))

; ; ;;;; Global keybindings

; ; ; (global-set-key (kbd "s-x") 'kill-region)
; ; ; (global-set-key (kbd "s-c") 'kill-ring-save)
; ; ; (global-set-key (kbd "s-v") 'yank)
; ; ; (global-set-key (kbd "s-z") 'undo)
; ; ; (global-set-key (kbd "s-s") 'save-buffer)
; ; ; (global-set-key (kbd "s-w") 'delete-window)
; ; ; (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

; ; ;;;; Modes

; (use-package flycheck-joker :ensure t)
; (use-package flycheck-clj-kondo :ensure t)

; (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
;   (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

; (use-package flycheck :ensure t
;   :init (global-flycheck-mode)
;   :config
;   (setq
;     flycheck-display-errors-delay 1
;     flycheck-highlighting-mode 'lines
;     flycheck-check-syntax-automatically '(save))
;   (set-face-attribute 'flycheck-error nil :underline '(:color "red3" :style wave))
;   (set-face-attribute 'flycheck-warning nil :underline '(:color "orange2" :style wave)))

; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
; (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
; (global-undo-tree-mode)

; (use-package paredit
;   :init
;   (progn
;     (add-hook 'clojure-mode-hook 'paredit-mode)
;     (add-hook 'clojurescript-mode-hook 'paredit-mode)
;     (add-hook 'clojurec-mode-hook 'paredit-mode)
;     (add-hook 'cider-repl-mode-hook 'paredit-mode)))

; (use-package paren-face
;   :init
;   (global-paren-face-mode)
;   :config
;   (add-hook 'clojure-mode-hook (lambda () (setq paren-face-regexp "#?[](){}[]"))))

; (use-package company
;   :init (global-company-mode)
;   :config
;   (progn
;     (defun indent-or-complete ()
;       (interactive)
;       (if (looking-at "\\_>")
;           (company-complete-common)
;         (indent-according-to-mode)))
;     (global-set-key "\t" 'indent-or-complete)))

; ; (use-package magit)

; ; (use-package guide-key
; ;   :init (guide-key-mode 1)
; ;   :config
; ;   (progn
; ;     (setq guide-key/idle-delay 1)
; ;     (setq guide-key/recursive-key-sequence-flag t)
; ;     (setq guide-key/popup-window-position 'bottom)))

; (use-package ido)

; (use-package flx-ido
;   :init (flx-ido-mode 1)
;   :config (setq ido-use-faces nil))

; (use-package ido-vertical-mode
;   :init (ido-vertical-mode 1))

; (use-package helm
;   :config
;   (progn
;     (helm-mode 1)
;     (setq helm-autoresize-mode t)
;     (setq helm-buffer-max-length 40)
;     (diminish 'helm-mode)
;     (global-set-key (kbd "M-x") 'helm-M-x)))

; (use-package helm-projectile
;   :config
;   (progn
;     (defun helm-projectile-find-file-in-mono-repo ()
;       ""
;       (interactive)
;       (helm-projectile-find-file))

;     (global-set-key (kbd "C-f") 'helm-projectile-find-file-in-mono-repo)
;     (global-set-key (kbd "C-a") 'helm-projectile-ag)))

; (use-package projectile
;   :init (projectile-global-mode))

; ; (use-package yasnippet
; ;   :init
; ;   (progn
; ;     (yas-global-mode 1)
; ;     (use-package clojure-snippets)))

; (use-package yaml-mode
;   :mode ("\\.yml$" . yaml-mode))

; (use-package clojure-mode
;   :mode (("\\.edn$" . clojure-mode))
;   :config
;   (progn
;     (require 'flycheck-clj-kondo)
;     (setq clojure-align-forms-automatically nil)

;     (define-clojure-indent
;       (defroutes 'defun)
;       (GET 2)
;       (POST 2)
;       (PUT 2)
;       (DELETE 2)
;       (HEAD 2)
;       (ANY 2)
;       (context 2)
;       (let-routes 1))

;     (define-clojure-indent
;       (form-to 1))

;     (define-clojure-indent
;       (match 1)
;       (are 2)
;       (checking 2)
;       (async 1)
;       (setup/run -1)
;       (it.setup/run -1))

;     (define-clojure-indent
;       (run* 1)
;       (fresh 1))

;     (define-clojure-indent
;       (extend-freeze 2)
;       (extend-thaw 1))

;     (define-clojure-indent
;       (go-loop 1))

;     (define-clojure-indent
;       (this-as 1)
;       (specify 1)
;       (specify! 1))

;     (define-clojure-indent
;       (s/fdef 3))

;     (defun toggle-nrepl-buffer ()
;       "Toggle the nREPL REPL on and off"
;       (interactive)
;       (if (string-match "cider-repl" (buffer-name (current-buffer)))
;           (delete-window)
;         (cider-switch-to-repl-buffer)))

;     (defun cider-save-and-refresh ()
;       (interactive)
;       (save-buffer)
;       (call-interactively 'cider-refresh))

;     (defun cider-eval-last-sexp-and-append ()
;       (interactive)
;       (cider-eval-last-sexp '(1)))

;     (global-set-key (kbd "s-r") 'cider-save-and-refresh)))

; (use-package cider
;   :config
;   (progn
;     (setq nrepl-hide-special-buffers t)
;     (setq cider-popup-stacktraces-in-repl t)
;     (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
;     (setq cider-repl-pop-to-buffer-on-connect t)
;     (setq cider-auto-select-error-buffer nil)
;     (setq cider-prompt-save-file-on-load nil)
;     (setq cider-repl-display-help-banner nil)
;     (setq cider-repl-use-pretty-printing t)
;     (setq cider-refresh-before-fn "reloaded.repl/suspend")
;     (setq cider-refresh-after-fn "reloaded.repl/resume")
;     (setq cider-cljs-lein-repl "(do (reloaded.repl/go) (user/cljs-repl))")
;     (setq cider-prompt-for-symbol nil)
;     (setq cider-test-defining-forms '("deftest" "defspec" "deftest+trace" "defspec+trace" "def-market-state-test"))))

; (use-package clj-refactor
;   :init
;   (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
;   :config
;   (cljr-add-keybindings-with-prefix "C-c r"))

; (use-package multiple-cursors
;   :config
;   (progn
;     (global-set-key (kbd "C-e") 'mc/mark-next-like-this)
;     (global-set-key (kbd "C-S-e") 'mc/mark-all-like-this)))

; (use-package helm-open-github
;   :config
;   (progn
;     (global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
;     (global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
;     (global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
;     (global-set-key (kbd "C-c p p") 'helm-open-github-from-pull-requests)))

; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(custom-safe-themes
;    (quote
;     ("2593436c53c59d650c8e3b5337a45f0e1542b1ba46ce8956861316e860b145a0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "454c1c9ce70f7d807c51c890910365fd3c64a9e63f596511e9ff57dd97bbeea8" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" default)))
;  '(package-selected-packages
;    (quote
;     (dakrone-theme spacemacs-theme flycheck-color-mode-line eziam-theme flycheck-clj-kondo flycheck-pos-tip highlight-symbol quickrun helm-ag yaml-mode wrap-region use-package undo-tree string-inflection smex smartparens rustic rainbow-mode rainbow-delimiters racer powerline paren-face neotree magit lsp-mode linum-relative ido-vertical-mode helm-projectile helm-open-github guide-key flx-ido expand-region eglot drag-stuff diminish company clojure-snippets clj-refactor cask better-defaults aggressive-indent ag afternoon-theme)))
;  '(safe-local-variable-values (quote ((cider-shadow-cljs-default-options . "app")))))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(f clojure-mode leaf-keywords hydra el-get blackout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
