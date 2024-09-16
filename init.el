(package-initialize)

(setq package-user-dir "~/.emacs.d/elpa")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(setq confirm-kill-emacs 'y-or-n-p)
(setq enable-recursive-minibuffers t)
(setq max-lisp-eval-depth 10000)
;; Symbolic link setting
(setq find-file-visit-truename t)
(setq vc-handled-backends '(Git))
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq show-trailing-whitespace t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(menu-bar-mode 0)
(windmove-default-keybindings)
(save-place-mode 1)
(electric-pair-mode 1)
(show-paren-mode t)
;; Move between windows using the Shift key and arrow keys.
(windmove-default-keybindings)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x C-j") 'execute-extended-command)

(set-face-background 'mode-line "gray10")
(set-face-foreground 'mode-line "gray95")

(use-package hl-line
  :init
  (global-hl-line-mode +1)
  :custom-face
  (hl-line ((t (:background "#222222")))))

(use-package which-func
  :defer t
  :init
  (which-function-mode 1))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (unless (window-system) (diff-hl-margin-mode))
  :custom-face
  (diff-hl-change ((t (:background "gray10"))))
  (diff-hl-delete ((t (:background "gray10"))))
  (diff-hl-insert ((t (:background "gray10")))))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'exec-path (expand-file-name "~/go/bin/"))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package go-dlv
  :ensure t
  :defer t)

(use-package go-eldoc
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t
  :config
  (setq flycheck-indication-mode 'left-margin)
  (add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  (setq company-selection-wrap-around t)
  (setq company-bg-color "gray10")
  (setq company-fg-color "#AAAAAA")
  (setq company-selection-color "white")
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (set-face-attribute 'company-preview nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-preview-common nil :foreground company-fg-color :background company-bg-color :underline t)
  (set-face-attribute 'company-preview-search nil :foreground company-fg-color :background company-bg-color :underline t)
  (set-face-attribute 'company-tooltip nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-tooltip-common nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-tooltip-common-selection nil :foreground company-selection-color :background company-bg-color :underline t :weight 'bold)
  (set-face-attribute 'company-tooltip-selection nil :foreground company-selection-color :background company-bg-color :underline t :weight 'bold)
  (set-face-attribute 'company-tooltip-annotation-selection nil :foreground company-selection-color :background company-bg-color :underline t :weight 'bold)
  (set-face-attribute 'company-tooltip-annotation nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-tooltip-search nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-tooltip-search-selection nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-scrollbar-fg nil :background company-bg-color)
  (set-face-attribute 'company-scrollbar-bg nil :background company-bg-color)
  (set-face-attribute 'company-echo nil :foreground company-fg-color :background company-bg-color)
  (set-face-attribute 'company-echo-common nil :foreground company-fg-color :background company-bg-color))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-x l")
  :hook ((go-mode rust-mode). lsp)
  :commands (lsp lsp-deferred))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-rust
  :defer t
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-chaining-hints t))

(use-package rust-mode
  :ensure t
  :defer t
  :config (add-hook 'rust-mode-hook #'lsp)
  :custom rust-format-on-save t)

(use-package lsp-ui
  :ensure t
  :custom ((lsp-ui-doc-enable))
  :hook ((lsp-mode-hook . lsp-ui-mode))
  :bind ("C-c C-u" . lsp-ui-doc-show)
  :config
  (setq lsp-lens-enable nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-max-height 50)
  (setq lsp-ui-peek-enable t)
  :custom-face
  (lsp-ui-doc-background ((t (:background "gray10")))))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom"))

(use-package quickrun
  :ensure t
  :bind
  (("C-c C-c" . quickrun)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("*quickrun*") popwin:special-display-config))

(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 100)
  (recentf-mode 1))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (add-to-list 'load-path "~/.emacs.d/snippets")
  (yas-global-mode 1)
  :bind
  (("C-c y" . company-yasnippet) ("C-c C-y" . company-yasnippet)))

(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  (add-hook 'after-init-hook #'(lambda ()
				 (minions-mode)
				 (setq minions-mode-line-lighter ""))))

(use-package nasm-mode
  :ensure t
  :defer t)

(use-package masm-mode
  :ensure t
  :defer t)

(use-package treemacs
  :ensure t
  :defer t
  :bind ("C-o" . treemacs)
  :config
  (setq treemacs-file-event-delay 2000)
  (setq treemacs-file-follow-delay 0.2)
  (treemacs-filewatch-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (add-hook 'treemacs-mode-hook (lambda () (which-function-mode -1))))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-font-lock-level 4)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :ensure t
  :bind
  (("C-c l" . magit-log-current)
   ;; To view the git history of a specific range of lines, select the region first and then execute.
   ("C-c C-l" . magit-log-buffer-file)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-vcs-max-length 30)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-rust t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-project-detection 'project))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  (setq rg-group-result t)
  :bind
  ("C-c r" . rg-project))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-c a" . embark-act)
   ("C-." . embark-act)
   ("C-c d" . embark-dwim)
   ("C-," . embark-dwim)
   ("C-c b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package goto-line-preview
  :ensure t
  :defer t
  :config
  (setq goto-line-preview-hl-duration 1.5)
  :custom-face
  (goto-line-preview-hl ((t (foreground "#AAAAAA" :background "gray10" :underline t)))))

(use-package org-modern
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package howm
  :ensure t
  :defer t
  :init
  (setq howm-directory "~/howm")
  (setq howm-home-directory howm-directory)
  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
  (setq howm-view-title-header "*")
  (setq howm-dtime-format "<%Y-%m-%d %a %H:%M>")
  (setq howm-prefix (kbd "C-c ;"))
  :bind*
  ("C-c ; ;" . howm-menu))

(use-package robe
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-ts-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package projectile-rails
  :ensure t
  :defer t
  :config (projectile-rails-global-mode))

(use-package prescient
  :ensure t
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode +1))

(use-package company-prescient
  :ensure t)

(use-package vertico-prescient
  :ensure t)

(use-package request
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package know-your-http-well
  :ensure t
  :defer t)

(use-package so-long
  :init
  (global-so-long-mode +1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-buffer-height 15)
  (setq vertico-cycle t)
  (setq vertico-buffer-name "*Vertico*")
  (vertico-multiform-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico-buffer
  :after vertico
  :init (vertico-buffer-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; -------------------------------------------------------------

(defun my/recompile-elpa ()
  "Recompile packages in elpa directory."
  (interactive)
  (package-refresh-contents)
  (byte-recompile-directory package-user-dir nil 'force))

(defun my/kill-other-buffers ()
  "Kill all buffers but the current one.Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer))
                (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun my/extract-webpage-content (url)
  (interactive "sEnter URL: ")
  (request url
    :parser 'buffer-string
    :success
    (cl-function (lambda (&key data &allow-other-keys)
                   (when data
                     (with-current-buffer (get-buffer-create "*Webpage Content*")
			   (erase-buffer)
			   (insert data)
			   (html-mode)
			   (display-buffer (current-buffer))))))
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                   (message "Got error: %S" error-thrown)))
    :complete
    (lambda (&rest _) (message "Finished!"))))

(defun my/set-flycheck-margins ()
  ;; Adjust margins and fringe widths…
  (setq left-fringe-width 8 right-fringe-width 0
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

;; -------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(request dirvish flycheck gopls use-package)))

;; -------------------------------------------------------------
