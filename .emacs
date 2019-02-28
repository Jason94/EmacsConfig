;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(let* ((no-ssl (and (memq system-type '(win000000dows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(custom-safe-themes
   (quote
    ("d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-notifications-mode t)
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (prettier-js zenburn-theme define-word iedit slime solarized-theme sunshine hydra origami avy telephone-line landmark purescript-mode psc-ide feature-mode yasnippet-snippets win-switch flycheck-flow company-mode flycheck flow-minor-mode dracula-theme ag web-mode undo-tree magit dumb-jump color-theme-modern ensime projectile dashboard page-break-lines scala-mode use-package)))
 '(web-mode-enable-auto-indentation nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Disable local variables
(setq enable-local-variables nil
      enable-local-eval nil)

; Hide toolbar
(tool-bar-mode -1)

; Start in full-screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-t") 'kill-region)
(global-set-key (kbd "M-t") 'kill-ring-save)
(global-set-key (kbd "C-h") 'newline-and-indent)
(global-set-key (kbd "M-s") 'ag-project)
(global-set-key (kbd "M-e") 'revert-buffer)

;; Notifications
(require 'notifications)

;; Packages ;;

; I guess I can't do (use-package use-package)?
; Just remember to do manually haha

; Load .el files
(add-to-list 'load-path "~/.emacs.d/lisp")
; Add all subdirectories in lisp/ to load-path
(let ((files (directory-files "~/.emacs.d/lisp/")))
  (dolist (file files)
    (if (and
	 (file-directory-p (concat "~/.emacs.d/lisp/" file))
	 (not (string-equal file "."))
	 (not (string-equal file "..")))
	(add-to-list 'load-path (expand-file-name file "~/.emacs.d/lisp")))))

(setq use-package-always-ensure t)

(use-package hydra)

(use-package ensime
  :ensure t)
(setq ensime-startup-notification nil)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package projectile
  :config
  (global-set-key (kbd "C-c C-p f") 'projectile-find-file))

(use-package yasnippet
  :config
  (define-key yas-minor-mode-map (kbd "C-c y") yas-maybe-expand)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package page-break-lines)

(use-package org)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g f" . magit-find-file)
         ("<C-tab>" . magit-section-cycle-diffs)
         ("C-x M-l" . magit-log-buffer-file)
         ("C-x M-b" . magit-blame-addition)))

(use-package exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 15)
			  (bookmarks . 5)
			  (projects . 5))))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind
  (("C-z" . undo)
   ("C-M-z" . redo)
   ("C-x u" . undo-tree-visualize)))

(use-package iedit)

(use-package ag)

(use-package web-mode
  :custom
  (web-mode-enable-auto-indentation nil))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck-flow)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
            '(javascript-jshint)))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
;;  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package flow-minor-mode)

(use-package company-flow
  :custom
  (company-idle-delay 0))

(use-package company
  :ensure t
  :bind (("C-c C-v" . company-complete))
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (add-to-list 'company-backends 'company-flow)))

(defun load-solarized-theme ()
  (use-package solarized-theme)
  (load-theme 'solarized-light))

(defun load-dracula-theme ()
 (use-package dracula-theme)
 (load-theme 'dracula t)
 (setq ensime-sem-high-faces
       '((implicitConversion . (:underline (:color "gray40"))))))

(defun load-zenburn-theme()
  (use-package zenburn-theme)
  (load-theme 'zenburn))

(load-zenburn-theme)
;;(load-solarized-theme)

(use-package feature-mode)

(use-package purescript-mode)

(use-package psc-ide
  :config
  (progn
    (add-hook 'purescript-mode-hook
       (lambda ()
         (psc-ide-mode)
         (company-mode)
         (flycheck-mode)
         (turn-on-purescript-indentation)))
    (setq psc-ide-use-npm-bin t)))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(defun win-switch-set-key (key command)
  (win-switch-set-keys (list key) command))

(use-package win-switch
  :config
  (setq win-switch-idle-time 4)
  (setq win-switch-other-window-first nil)
  (global-set-key (kbd "C-x o") 'win-switch-dispatch)
;;  (win-switch-setup-keys-ijkl "\C-xo")
  (win-switch-set-key "c" 'up)
  (win-switch-set-key "t" 'down)
  (win-switch-set-key "h" 'left)
  (win-switch-set-key "n" 'right)
  (win-switch-set-key "r" 'next-window)
  (win-switch-set-key "l" 'previous-window)
  (win-switch-set-key "C" 'enlarge-vertically)
  (win-switch-set-key "T" 'shrink-vertically)
  (win-switch-set-key "H" 'shrink-horizontally)
  (win-switch-set-key "N" 'enlarge-horizontally)
  (win-switch-set-key " " 'other-frame)
  (win-switch-set-keys '("u" [return]) 'exit)
  (win-switch-set-key "s" 'split-horizontally)
  (win-switch-set-key "d" 'split-vertically)
  (win-switch-set-key "0" 'delete-window)
  (win-switch-set-key "\M-\C-g" 'emergency-exit))
(use-package slime)
(require 'slime-autoloads)
(setq slime-lisp-implementations
  ;    '((clisp ("/usr/bin/clisp"))
      '((sbcl ("/usr/bin/sbcl"))))
(setq inferior-lisp-program 'sbcl)

(use-package avy
  :bind ("M-l" . avy-goto-char-2))

(use-package origami
  :config
  (setq global-origami-mode t)
  (defhydra hydra-origami (:color red)
    "
    _o_pen node    _n_ext fold       toggle _f_orward
    _c_lose node   _p_revious fold   toggle _a_ll
    "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))
  :bind ("C-c f" . hydra-origami/body))

(use-package sunshine
  :custom (sunshine-location "53705,USA"))

(use-package define-word
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package prettier-js
  :init
  (add-hook 'web-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-js-args '("--insert-pragma" "--require-pragma")))

;; line numbers
(global-linum-mode t)
(setq column-number-mode t)

;; Indentation
(setq-default indent-tabs-mode nil)

;; Backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq auto-save-list-file-prefix `(("." . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Language specs

(defun flow/set-flow-exec ()
  (interactive)
  (let* ((os (pcase system-type
               ('darwin "osx")
               ('gnu/linux "linux64")
               (_ nil)))
         (root (locate-dominating-file buffer-file-name "node_modules/flow-bin"))
         (executable (car (file-expand-wildcards
                           (concat root "node_modules/flow-bin/*" os "*/flow")))))
    (setq-local company-flow-executable executable)
    (setq-local flow-minor-default-binary executable)
    (setq-local flycheck-javascript-flow-executable executable)))

(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(defun cust-web-mode ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2))
(add-hook 'web-mode-hook 'cust-web-mode)
(add-hook 'web-mode-hook 'flow-minor-enable-automatically)
(add-hook 'flow-minor-mode-hook #'flow/set-flow-exec t)
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js\\'")
        ("jsx" . "\\.jsx\\'")))

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done 'time)
;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

; Parens
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-background 'show-paren-match "LightGoldenrod4")

; Disable beep
(setq ring-bell-function 'ignore)

;; ERC
(defun configure-erc ()
  "Configure ERC for my work setting."
;;  (setq erc-autojoin-channels-alist
;;        '(("wicourts.gov" "#ccap3" "#sccaefiling")))
  (erc :server "irc.wicourts.gov" :port 6667 :nick "JasonW")
  (add-hook 'erc-text-matched-hook 'erc-beep-on-match)
  (setq erc-beep-match-types '(current-nick keyword))
  (setq erc-log-channels-directory "~/.erc/logs/"))
(configure-erc)
