;;; -*- coding: utf-8; mode: emacs-lisp; lexical-binding: t -*-

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-compose-format-flowed t)
(setq mu4e-compose-in-new-frame nil)
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-confirm-quit nil)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-get-mail-command "~/bin/fetchmail")
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-headers-include-related t)
(setq mu4e-html2text-command "pandoc -f html -t plain --columns 78 --strip-comments")
(setq mu4e-view-prefer-html nil)
(setq mu4e-view-show-addresses t)
(setq mu4e-view-use-gnus t)
(setq smtpmail-stream-type 'starttls)
(setq user-full-name "Tom Vaughan")

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "1 tvaughan@tocino.cl"
           :match-func (lambda (msg)
                         (when msg
                           (or (string-match-p "^/tvaughan@tocino.cl" (mu4e-message-field msg :maildir))
                               (mu4e-message-contact-field-matches msg :to "tvaughan@tocino.cl"))))
           :vars '((user-mail-address . "tvaughan@tocino.cl")
                   (mu4e-drafts-folder . "/tvaughan@tocino.cl/Drafts")
                   (mu4e-sent-folder . "/tvaughan@tocino.cl/Sent")
                   (mu4e-trash-folder . "/tvaughan@tocino.cl/Trash")
                   (smtpmail-smtp-server . "smtp.migadu.com")
                   (smtpmail-smtp-service . 465)
                   (smtpmail-smtp-user "tvaughan@tocino.cl")))
         ,(make-mu4e-context
           :name "2 tom@clubcannabis.cl"
           :match-func (lambda (msg)
                         (when msg
                           (or (string-match-p "^/tom@clubcannabis.cl" (mu4e-message-field msg :maildir))
                               (mu4e-message-contact-field-matches msg :to "tom@clubcannabis.cl"))))
           :vars '((user-mail-address . "tom@clubcannabis.cl" )
                   (mu4e-drafts-folder . "/tom@clubcannabis.cl/Drafts")
                   (mu4e-sent-folder . "/tom@clubcannabis.cl/Sent")
                   (mu4e-trash-folder . "/tom@clubcannabis.cl/Trash")
                   (smtpmail-smtp-server . "smtp.migadu.com")
                   (smtpmail-smtp-service . 465)
                   (smtpmail-smtp-user "tom@clubcannabis.cl")))))

(require 'mu4e-transient)
(global-set-key (kbd "C-c m") #'mu4e-transient-menu)

(require 'auth-source-pass)
(auth-source-pass-enable)

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(provide 'setup-10mu4e)
;;; setup-10mu4e.el ends here
