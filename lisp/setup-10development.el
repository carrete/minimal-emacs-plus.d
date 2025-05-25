;;; -*- coding: utf-8; mode: emacs-lisp; lexical-binding: t -*-

(require 'use-package)

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode))
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i" "2" "-bn" "-sr"))
  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
  (setf (alist-get 'prettier-ruby apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=ruby"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
  (setf (alist-get 'prettier-toml apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=toml"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
  (setf (alist-get 'toml-ts-mode apheleia-mode-alist)
        '(prettier-toml))
  (add-to-list 'apheleia-mode-alist '(toml-ts-mode . prettier-toml)))

(use-package cider
  :init
  (setq cider-auto-select-error-buffer nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-connection-message-fn nil)
  (setq cider-redirect-server-output-to-repl nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-reuse-dead-repls 'auto)
  (setq cider-show-error-buffer nil)
  (setq cider-test-show-report-on-success t)
  (setq nrepl-hide-special-buffers t)
  :config
  (cider-enable-cider-completion-style t))

(use-package flycheck-clj-kondo
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (require 'flycheck-clj-kondo))))

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode t)
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package cider-storm)

(use-package exec-path-from-shell
  :config
  (dolist (var '("GPG_AGENT_INFO" "LANG" "LC_COLLATE" "LC_TIME" "SSH_AGENT_PID" "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var)))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package git-modes)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flyover
  :init
  (setq flyover-display-mode 'show-only-on-same-line)
  (setq flyover-levels '(error warning))
  (setq flyover-use-theme-colors t)
  :config
  (add-hook 'flycheck-mode-hook #'flyover-mode))

(use-package magit
  :config
  (setq magit-diff-visit-prefer-worktree t)
  (setq magit-repository-directories '(("~/Projects" . 3)))
  (setq magit-show-long-lines-warning nil))

(use-package popper
  :bind (("C-," . popper-toggle)
         ("C-." . popper-cycle)
         ("C-M-," . popper-toggle-type))
  :config
  (setq popper-display-control nil)
  (setq popper-display-function #'display-buffer-in-direction)
  (setq popper-reference-buffers
        '("Output\\*$"
          "^\\*Async Shell Command"
          "^\\*Choices"
          "^\\*HTTP Response"
          "^\\*Messages"
          "^\\*cider-repl"
          "^\\*eldoc.*\\*"
          "^\\*rg"
          help-mode
          compilation-mode))
  (popper-mode t)
  (popper-echo-mode t))

;; https://github.com/AmaiKinono/puni/issues/20
(defun tvaughan/disable-puni-mode ()
  "Disable `puni-mode' unless when eval-expression."
  (unless (eq this-command 'eval-expression)
    (puni-disable-puni-mode)))

;; Use puni-mode globally and disable it for minibuffer-mode.
(use-package puni
  :defer t
  :bind (;; TODO: ("C-]" . puni-barf-forward)
         ;; TODO: ("C-[" . puni-barf-backward)
         ("C-)" . puni-slurp-forward)
         ("C-(" . puni-slurp-backward))
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'minibuffer-setup-hook #'tvaughan/disable-puni-mode))

(use-package rainbow-mode
  :hook (text-mode . rainbow-mode))

;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting. It supports a
;; broad set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, Markdown, and many others.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (delete 'bash treesit-auto-langs) ;; TODO:
  (global-treesit-auto-mode))

(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.njk\\'" . web-mode))
  :init
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-front-matter-block t)
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")
                                 ("django" . "\\.njk\\'"))))

(provide 'setup-10development)
;;; setup-10development.el ends here
