(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)

(menu-bar-mode 0)
(windmove-default-keybindings)
(save-place-mode 1)

(set-face-background 'mode-line "gray10")
(set-face-foreground 'mode-line "gray95")
(set-face-background 'region "gray40")

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

(use-package git-gutter+
  :ensure t)
(global-git-gutter+-mode)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

(use-package go-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package go-eldoc
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package smex
  :ensure t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-j") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(use-package company
  :ensure t)
(global-company-mode) 
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
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; https://oremacs.com/swiper/
(add-to-list 'load-path "~/.emacs.d/git/swiper/")
(require 'ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key "\C-s" 'swiper)



;; -------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(gopls use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; -------------------------------------------------------------
