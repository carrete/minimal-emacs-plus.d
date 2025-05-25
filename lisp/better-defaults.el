;;; -*- coding: utf-8; mode: emacs-lisp; lexical-binding: t -*-

(mapc #'disable-theme custom-enabled-themes)

(set-face-attribute 'default nil :height 130 :family "FiraCode Nerd Font")

(setq column-number-mode t)
(setq line-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(setq dired-listing-switches "-AFhl --dired --group-directories-first --time-style=long-iso")
(setq enable-remote-dir-locals t)
(setq ibuffer-expert t)
(setq ispell-dictionary "en_US")
(setq ispell-hunspell-dict-paths-alist '(("en_US" "/usr/share/hunspell/en_US.aff")))
(setq ispell-hunspell-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq next-line-add-newlines t)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

(setq rg-ignore-ripgreprc nil)

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))
(add-hook 'kill-emacs-hook #'recentf-cleanup)
(add-hook 'after-init-hook #'savehist-mode)

(auto-save-visited-mode t)

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-z") #'undo)

(defun tvaughan/parent-directory (path)
  "Return the parent directory of PATH."
  (or (file-name-directory (directory-file-name path)) ""))

(defun tvaughan/backward-delete-directory (n)
  "Delete characters backward until encountering the end of a word.
Given N, do this n many times."
  (interactive "p")
  (let ((contents (minibuffer-contents)))
    (delete-minibuffer-contents)
    (insert (tvaughan/parent-directory contents))))

(defun tvaughan/kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'tvaughan/kill-this-buffer)
(global-set-key (kbd "C-x w") #'delete-frame)

(define-minor-mode tvaughan/pinned-buffer-mode
  "Pin the current buffer to the selected window."
  :init-value nil
  :lighter (" PB")
  (let (window (selected-window))
    (set-window-dedicated-p window #'tvaughan/pinned-buffer-mode)
    (set-window-parameter window 'no-delete-other-windows #'tvaughan/pinned-buffer-mode)))

(global-set-key (kbd "C-c p") #'tvaughan/pinned-buffer-mode)

(defun tvaughan/previous-window ()
  "Corollary to 'other-window'."
  (interactive)
  (other-window nil))

(global-set-key (kbd "C-x p") #'tvaughan/previous-window)
(global-set-key (kbd "C-x n") #'other-window)

(defun tvaughan/sort-words (reverse beginning end)
  "Sort words (non-whitespace strings) in region, in REVERSE if negative, from BEGINNING to END."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\S-\\)+" "\\&" beginning end))

(defun tvaughan/window-enlarge-horizontally (columns)
  (enlarge-window columns t))

(defun tvaughan/window-shrink-horizontally (columns)
  (shrink-window columns t))

(defun tvaughan/window-set-width (columns)
  (interactive "nEnter new window width (in columns): ")
  (tvaughan/window-enlarge-horizontally (- columns (window-width)))
  (message "New window width is: %d" (window-width)))

(defun tvaughan/window-enlarge-vertically (rows)
  (enlarge-window rows nil))

(defun tvaughan/window-shrink-vertically (rows)
  (shrink-window rows nil))

(defun tvaughan/window-set-height (rows)
  (interactive "nEnter new window height (in rows): ")
  (tvaughan/window-enlarge-vertically (- rows (window-height)))
  (message "New window height is: %d" (window-height)))

(defun tvaughan/untabify ()
  "Preserve initial tab when makefile-mode."
  (interactive)
  (save-excursion
    (if (derived-mode-p 'makefile-mode)
        (progn
          (goto-char (point-min))
          (while (not (eobp))
            (skip-chars-forward "\t")
            (untabify (point) (line-end-position))
            (forward-line 1)))
      (untabify (point-min) (point-max)))))

(add-hook 'before-save-hook #'tvaughan/untabify)

(use-package uuid
  :ensure t)

(defun tvaughan/uuid ()
  "Return a lowercase UUID."
  (downcase (uuid-string)))

(defun tvaughan/insert-uuid ()
  "Insert a lowercase UUID at point and add it to the kill ring."
  (interactive)
  (let ((uuid (tvaughan/uuid)))
    (kill-new uuid)
    (insert uuid)))

(global-set-key (kbd "C-c u") #'tvaughan/insert-uuid)

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'macchiato)
  (load-theme 'catppuccin t))

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode t))

(use-package define-word
  :ensure t
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode t))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package olivetti
  :ensure t
  :config
  (setq olivetti-lighter nil))

(use-package savehist
  :ensure t
  :config
  (savehist-mode))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-subtle-mode-line
        '(:mode-line-active default :mode-line-inactive "#434C5E"))
  (spacious-padding-mode t))

(provide 'better-defaults)
;;; better-defaults.el ends here
