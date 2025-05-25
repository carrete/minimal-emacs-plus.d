;;; -*- coding: utf-8; mode: emacs-lisp; lexical-binding: t -*-

(require 'use-package)

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :ensure t
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
  :ensure t
  :init
  (setq cider-auto-select-error-buffer nil)
  (setq cider-redirect-server-output-to-repl nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-reuse-dead-repls 'auto)
  (setq cider-show-error-buffer nil)
  :config
  (cider-enable-cider-completion-style t))

(use-package cider-storm
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flyover
  :ensure t
  :init
  (setq flyover-levels '(error warning))
  (setq flyover-use-theme-colors t)
  :config
  (add-hook 'flycheck-mode-hook #'flyover-mode))

(use-package magit
  :ensure t
  :config
  (setq magit-repository-directories '(("~/Projects" . 3)))
  (setq magit-section-visibility-indicator nil)
  (setq magit-show-long-lines-warning nil))

(use-package popper
  :ensure t
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
  :bind (("C-]" . puni-barf-forward)
         ("C-[" . puni-barf-backward)
         ("C-)" . puni-slurp-forward)
         ("C-(" . puni-slurp-backward))
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'minibuffer-setup-hook #'tvaughan/disable-puni-mode))

(use-package rainbow-mode
  :ensure t
  :hook (text-mode . rainbow-mode))

;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting. It supports a
;; broad set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, Markdown, and many others.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package web-mode
  :ensure t
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
