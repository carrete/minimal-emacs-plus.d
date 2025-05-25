;;; -*- coding: utf-8; mode: emacs-lisp; lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defun dictionary-lessp (str1 str2)
  "Return t if STR1 is < STR2 when doing a dictionary compare.
Splits the string at numbers and doing numeric compare with them."
  (let ((str1-components (dict-split str1))
        (str2-components (dict-split str2)))
    (dict-lessp str1-components str2-components)))

(defun dict-lessp (slist1 slist2)
  "Compare the two lists of strings & numbers SLIST1 and SLIST2."
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         nil)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))))

(defun dict-split (str)
  "Split the string STR into a list of number and non-number components."
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun realpath (file-name)
  "Return absolute path to FILE-NAME."
  (file-name-directory file-name))

(defun current-buffer-file-name ()
  "Return file name of current buffer being visited, or evaluated e.g.
during start-up."
  (realpath (or load-file-name (buffer-file-name))))

(defun load-directory (directory)
  "Load *.el in DIRECTORY by natural sort order."
  (add-to-list 'load-path directory)
  (mapc (lambda (name)
          (require (intern (file-name-sans-extension name))))
        (sort (directory-files directory nil "\\.el$") 'dictionary-lessp)))

(defun load-subdirectory (subdirectory)
  "Load *.el in SUBDIRECTORY of `default-directory'."
  (load-directory (concat (current-buffer-file-name) subdirectory)))

;; https://stackoverflow.com/a/1942422
(mapc #'load-subdirectory '("lisp"))
