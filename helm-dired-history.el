;;; helm-dired-history.el --- Show dired history with helm.el support.

;; Author: Joseph(纪秀峰) <jixiuf@gmail.com>
;; Copyright (C) 2011,2012, Joseph(纪秀峰), all rights reserved.
;; Created: 2011-03-26
;; Version: 0.1.1
;; X-URL:https://github.com/jixiuf/helm-dired-history
;; Keywords: helm, dired history
;;
;; Features that might be required by this library:
;;
;; `helm' `dired'
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

;;; Installation:

;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
;; (savehist-mode 1)
;; (eval-after-load 'dired
;;   '(progn (require 'helm-dired-history)
;;           (define-key dired-mode-map "," 'helm-dired-history-view)))
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

;;; Code:

(require 'helm)
(require 'dired)

(defvar helm-dired-history-variable nil)

(defun helm-dired-history-update()
  "update variable `helm-dired-history-variable'."
  (setq helm-dired-history-variable
        (delete-dups (delete (dired-current-directory) helm-dired-history-variable)))
  (setq helm-dired-history-variable
        (append (list (dired-current-directory)) helm-dired-history-variable)))

;;when you open dired buffer ,update `helm-dired-history-variable'.
(add-hook 'dired-after-readin-hook 'helm-dired-history-update)

(defvar helm-source-dired-history
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
    (helm '(helm-source-dired-history)
              ;; Initialize input with current symbol
              ""  nil nil)))

(provide 'helm-dired-history)
;;ends here.
