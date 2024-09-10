(package-initialize)

(setq package-user-dir "~/.emacs.d/elpa")
(setq max-lisp-eval-depth 10000)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(defun recompile-elpa ()
  "Recompile packages in elpa directory."
  (interactive)
  (package-refresh-contents)
  (byte-recompile-directory package-user-dir nil 'force))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)

(menu-bar-mode 0)
(windmove-default-keybindings)
(save-place-mode 1)
(electric-pair-mode 1)

(set-face-background 'mode-line "gray10")
(set-face-foreground 'mode-line "gray95")

(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray10")))))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(add-to-list 'exec-path (expand-file-name "~/go/bin/"))

(defun insert-and ()
  (interactive)
  (insert "&"))
(global-set-key (kbd "C-c ;") 'insert-and)

(defun insert-underscore ()
  (interactive)
  (insert "_"))
(global-set-key (kbd "C-c u") 'insert-underscore)

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
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'before-save-hook 'gofmt-before-save)

(use-package go-dlv
  :ensure t)

(use-package direx
  :ensure t)

(use-package dirvish
  :ensure t)
(dirvish-override-dired-mode)

(use-package go-eldoc
  :ensure t)

(use-package flycheck
  :ensure t)
(add-hook 'go-mode-hook 'flycheck-mode)

(use-package go-playground
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred))

(use-package company
  :ensure t)
(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 10)
(setq company-selection-wrap-around t)
(setq company-bg-color "gray10")
(setq company-fg-color "#AAAAAA")
(setq company-selection-color "white")

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
(set-face-attribute 'company-echo-common nil :foreground company-fg-color :background company-bg-color)

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode rust-mode). lsp)
  :commands lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-rust
  :defer t
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-chaining-hints t))

(global-set-key (kbd "M-*") 'xref-pop-marker-stack)
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-/") 'xref-find-references)

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)
(add-hook 'rust-mode-hook #'lsp)

;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :ensure t
  :custom ((lsp-ui-doc-enable))
  :hook ((lsp-mode-hook . lsp-ui-mode))
  :custom-face
  (lsp-ui-doc-background ((t (:background "gray10")))))

(global-set-key (kbd "C-c C-l") 'lsp-ui-doc-show)
(setq lsp-lens-enable nil)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-max-width 100)
(setq lsp-ui-doc-max-height 50)
(setq lsp-ui-peek-enable t)

(use-package ivy
  :ensure t)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package counsel-at-point
  :ensure t)

(require 'ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-j") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git-grep)
(global-set-key (kbd "C-c C-g") 'counsel-at-point-git-grep)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c C-i") 'counsel-at-point-imenu)

;; Don't insert an initial ^ (caret) when calling counsel-M-x.
(setq ivy-initial-inputs-alist
      (assq-delete-all 'counsel-M-x ivy-initial-inputs-alist))

;; https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
)

(use-package dired-subtree
  :ensure t
  :demand
  :bind
  (:map dired-mode-map
    ("<enter>" . mhj/dwim-toggle-or-open)
    ("<return>" . mhj/dwim-toggle-or-open)
    ("<tab>" . mhj/dwim-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))

;;https://www.flycheck.org/en/latest/user/error-reports.html#fringe-and-margin-icons
;; Show indicators in the left margin
(setq flycheck-indication-mode 'left-margin)

;; Adjust margins and fringe widths…
(defun my/set-flycheck-margins ()
  (setq left-fringe-width 8 right-fringe-width 0
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

;; …every time Flycheck is activated in a new buffer
(add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)

(use-package quickrun
  :ensure t)
(global-set-key (kbd "C-c C-c") 'quickrun)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)
              ("C-c C-p" . projectile-command-map)))

(use-package popwin
  :ensure t)
(setq display-buffer-function 'popwin:display-buffer)
(push '("*quickrun*") popwin:special-display-config)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(setq recentf-max-menu-items 50)

(use-package yasnippet
  :ensure t)
(add-to-list 'load-path
              "~/.emacs.d/snippets")
(yas-global-mode 1)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(global-set-key (kbd "C-c C-y") 'company-yasnippet)

(show-paren-mode t)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

(use-package minions
  :ensure t
  :config
  (minions-mode 1))
(add-hook 'after-init-hook #'(lambda ()
                               (minions-mode)
                               (setq minions-mode-line-lighter "")))

(use-package nasm-mode
  :ensure t)

(use-package masm-mode
  :ensure t)

(use-package consult
  :ensure t)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x C-b") 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

(use-package find-file-in-project
  :ensure t)

(use-package neotree
  :ensure t)
(global-set-key (kbd "C-o") 'neotree)

(use-package lua-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1)
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil))

;; -------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(dirvish flycheck gopls use-package)))

;; -------------------------------------------------------------
