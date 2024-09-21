(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-user-dir "~/.emacs.d/elpa")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa stable" . "https://stable.melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
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
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq tab-always-indent t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(menu-bar-mode 0)
(windmove-default-keybindings)
(save-place-mode +1)
(electric-pair-mode +1)
(show-paren-mode t)
;; Move between windows using the Shift key and arrow keys.
(windmove-default-keybindings)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(define-key global-map (kbd "C-x C-j") 'execute-extended-command)
(define-key global-map (kbd "C-x j") 'execute-extended-command)

(set-face-background 'mode-line "gray10")
(set-face-foreground 'mode-line "gray95")

(global-auto-revert-mode +1)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(use-package hl-line
  :init
  (global-hl-line-mode +1)
  :custom-face
  (hl-line ((t (:background "#222222")))))

(use-package which-func
  :defer t
  :init
  (which-function-mode +1))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (unless (window-system) (diff-hl-margin-mode))
  :hook (dired-mode . diff-hl-dired-mode)
  :custom-face
  (diff-hl-change ((t (:background "gray10"))))
  (diff-hl-delete ((t (:background "gray10"))))
  (diff-hl-insert ((t (:background "gray10")))))

(use-package go-mode
  :ensure t
  :hook
  (before-save . gofmt-before-save)
  :config
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
  :hook
  (flycheck-mode . my/set-flycheck-margins)
  (go-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-margin))

(use-package corfu
  :ensure t
  :custom ((corfu-auto t)
           (corfu-auto-prefix +1)
           (corfu-auto-delay 0)
           (corfu-cycle t))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :ensure t
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package rust-mode
  :ensure t
  :custom
  (rust-format-on-save t))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom"))

(use-package quickrun
  :ensure t)

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
  (recentf-mode +1))

(use-package minions
  :ensure t
  :config
  (minions-mode +1)
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

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind
  (("C-c l" . magit-log-current)
   ;; To view the git history of a specific range of lines, select the region first and then execute.
   ("C-c C-l" . magit-log-buffer-file)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode +1)
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
  ("C-c r" . rg-project)
  ("C-c C-r" . rg-project))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-align 'left)
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

(use-package org
  :ensure t
  :defer
  :hook
  (org-mode . howm-mode)
  (org-mode . org-modern-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(use-package org-modern
  :ensure t
  :after org
  :defer t
  :hook (org-agenda-finalize . org-modern-agenda)
  :config
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

(use-package ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode)))

(use-package ruby-end
  :ensure t)

(use-package rbenv
  :ensure t
  :config
  (setq rbenv-installation-dir "~/.rbenv")
  (global-rbenv-mode))

(use-package eglot
  :ensure t
  :hook ((rust-mode ruby-mode go-mode c-mode c++-mode) . eglot-ensure))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((c++-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((go-mode) "gopls"))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (add-to-list 'eglot-server-programs
             '((rust-mode rust-ts-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode +1))

(use-package projectile-rails
  :ensure t
  :defer t
  :config (projectile-rails-global-mode))

(use-package prescient
  :ensure t
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode +1))

(use-package vertico-prescient
  :ensure t
  :config
  (vertico-prescient-mode +1))

(use-package corfu-prescient
  :ensure t
  :config
  (corfu-prescient-mode +1))

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
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-multiform-mode))

(use-package savehist
  :init
  (savehist-mode))

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

(use-package consult
  :ensure t
  :demand
  :bind
  (("C-s" . consult-line)
   ("C-c C-s" . my/consult-line-symbol-at-point)
   ("C-c C-i" . consult-imenu-multi)
   ("C-x b" . consult-buffer)
   ("C-c g" . consult-git-grep)
   ("C-c C-g" . consult-grep)
   ("C-c f" . consult-find)
   ("C-x C-r" . consult-recent-file))
  :config
  (setq consult-preview-key 'any)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-recent-file consult-xref
   consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."))

(use-package consult-eglot
  :ensure t
  :after eglot
  :bind (:map eglot-mode-map
          ("C-c s" . consult-eglot-symbols)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package delsel
  :config
  (delete-selection-mode +1))

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

(defun my/consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;; -------------------------------------------------------------
