(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'company)
(require 'company-irony)
(require 'company-irony-c-headers)
(require 'company-rtags)
(global-company-mode)
(add-hook 'irony-mode-hook
	  'company-irony-setup-begin-commands)
(setq company-backends
      (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends
    '(company-irony-c-headers company-irony)))
(setq company-idle-delay 0)
(define-key c-mode-map [(tag)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

(require 'flycheck)
(require 'flycheck-rtags)
(require 'flycheck-irony)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(require 'helm)
(require 'helm-config)
(require 'helm-rtags)
(require 'helm-c-yasnippet)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8 ;scroll lines other window using m-<next>/m-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)
(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input feild."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
		   (let ((bg-color (face-background 'default nil)))
		     '(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook
	  'spacemacs//helm-hide-minibuffer-maybe)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 50)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") "helm-show-kill-ring")
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-greap H --no-group --no-color %e %p %f"))
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

(require 'helm-eshell)
(add-hook 'eshell-mode-hook
	  #'(lambda ()
	      (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))
(define-key shell-mode-map (kbd "C-c c-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)

(require 'nyan-mode)
(nyan-mode)

(require 'zone-select)
(require 'zone-nyan)
(require 'zone-sl)
(require 'zone-rainbow)
(zone-select-add-program 'zone-nyan)
(zone-select-add-program 'zone-sl)
(zone-select-add-program 'zone-rainbow)

(require 'yasnippet)
(yas-global-mode 1)

(require 'rtags)
(setq rtags-use-helm t)
(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(defun my-irony-mode-hook()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'cmake-ide)
(cmake-ide-setup)

(require 'autopair)
(autopair-global-mode)

(global-linum-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
