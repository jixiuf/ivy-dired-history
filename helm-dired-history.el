;;; helm-dired-history.el --- Show dired history with helm.el support.

;; Filename: helm-dired-history.el
;; Description:  Show dired history with helm.el support.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-03-26
;; Version: 0.1.0
;; URL: http://www.emacswiki.org/emacs/download/helm-dired-history.el
;; Keywords: helm, dired history
;; Compatibility: (Test on GNU Emacs 23.2.1)
;;  .
;;
;; Features that might be required by this library:
;;
;; `helm' `dired'
;;
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Someone like to reuse the current dired buffer to visit
;; another directory, so that you just need open one dired
;; buffer. but the bad point is ,you can't  easily go
;; forward and back in different dired directory. this file
;; can remember dired directory you have visited and list them
;; using `helm.el'.
;; `helm-dired-history.el' will save all dired directory you
;; have visited to file `helm-dired-history-cache-file'
;;

;;; Installation:
;;
;; (require 'helm-dired-history)
;; (define-key dired-mode-map "," 'helm-dired-history-view)

;; (require 'savehist)
;; (setq savehist-additional-variables
;;       '( helm-dired-history-variable
;;          ))
;; (savehist-mode 1)
;;
;; Or:
;; (autoload 'helm-dired-history-view "helm-dired-history"
;;    "view dired directories you have visited." t)
;; (define-key dired-mode-map "," 'helm-dired-history-view)
;;
;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-dired-history-view'
;;    call `helm' to show dired history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;


;;; Require
(require 'helm)
(require 'dired)


;; (defcustom helm-dired-history-cache-file
;;   "~/.emacs.d/cache/helm-dired-history-cache-file"
;;   "helm-dired-history-cache-file."
;;   :group 'helm-dired-history)

;; (defcustom helm-dired-history-max-length 20
;;     "the max length of dired history."
;;   :group 'helm-dired-history)

(defvar helm-dired-history-variable nil)

;; ;; if `helm-dired-history-cache-file' exists ,init
;; ;; `helm-dired-history-variable' with data from this file.
;; (when (file-exists-p (expand-file-name helm-dired-history-cache-file))
;;   (with-current-buffer (find-file-noselect helm-dired-history-cache-file)
;;     (goto-char (point-min))
;;     (setq helm-dired-history-variable (read (current-buffer)))
;;     (kill-buffer)))
;;;###autoload
(defun helm-dired-history-update()
  "update variable `helm-dired-history-variable'."
  (setq helm-dired-history-variable
        (delete-dups (delete (dired-current-directory) helm-dired-history-variable)))
  (setq helm-dired-history-variable
        (append (list (dired-current-directory)) helm-dired-history-variable)))

;;when you open dired buffer ,update `helm-dired-history-variable'.
;;;###autoload
(add-hook 'dired-after-readin-hook 'helm-dired-history-update)

;; (defun helm-dired-history-write2dist()
;;   "write `helm-dired-history-variable' to disk."
;;   (let ((tmp-history)(index 0))
;;     (while  (< index (min (length helm-dired-history-variable)
;;                           helm-dired-history-max-length))
;;       (setq tmp-history (append tmp-history (list (nth index helm-dired-history-variable))))
;;       (setq index (1+ index)))
;;       (with-temp-file (expand-file-name helm-dired-history-cache-file)
;;         (prin1 tmp-history (current-buffer)))
;;       ))
;; (add-hook 'kill-emacs-hook 'helm-dired-history-write2dist)
;; (run-with-timer 600 1800 'recentf-save-list)

;;;###autoload
(defvar helm-c-source-dired-history
  '((name . "Dired History:")
    (candidates . helm-dired-history-variable)
    (action . (("Go" . (lambda(candidate) (dired candidate)))))))

;;;###autoload
(defun helm-dired-history-view()
  "call `helm' to show dired history."
  (interactive)
  (let ((helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         (lambda () (message "No history record."))))
    (helm '(helm-c-source-dired-history)
              ;; Initialize input with current symbol
              ""  nil nil)))

(provide 'helm-dired-history)
;;ends here.
