;;; register-jump.el --- jump-to-register with preview  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: internal, convenience
;; Created: 2013-10-02

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (define-key ctl-x-r-map "j" 'register-jump)

;;; Code:

(eval-when-compile (require 'cl))

(defcustom register-jump-delay 0.5
  "The number of seconds idle for the preview window to pop up."
  :type 'number
  :group 'register)

(defvar register-jump-preview-buffer "*Register Preview*"
  "Name of the register preview buffer.")

(defun register-jump-shorten (s len)
  "Shorten string S to LEN."
  (if (<= (length s) len)
      s
    (concat (substring s 0 (- len 2)) " \u2026")))

(defun register-jump-describe (c)
  (let ((d (replace-regexp-in-string
            "\n[ \t]*" " "
            (with-output-to-string (describe-register-1 c)))))
    (if (string-match "Register.+? contains a " d)
        (substring d (match-end 0))
      d)))

(defun register-jump-get-line (buffer point)
  (when (and buffer point)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char point)
          (format "(%s) %s"
                  (propertize (register-jump-shorten (buffer-name) 20)
                              'face 'bold)
                  (buffer-substring
                   (line-beginning-position) (line-end-position))))))))

(defun register-jump-preview (buffer)
  (let ((split-height-threshold 0))
    (with-temp-buffer-window
     buffer
     (cons 'display-buffer-below-selected
           '((window-height . fit-window-to-buffer)))
     nil
     (with-current-buffer standard-output
       (setq cursor-in-non-selected-windows nil)
       (mapc (lambda (r)
               (let ((what (cdr r)))
                 (insert
                  (format
                   "%s %s\n"
                   (concat (single-key-description (car r)) ":")
                   (or (cond
                        ((markerp what)
                         (register-jump-get-line (marker-buffer what) what))
                        ((and (consp what) (eq (car what) 'file-query))
                         (register-jump-get-line
                          (get-file-buffer (nth 1 what)) (nth 2 what))))
                       (register-jump-describe (car r)))))))
             register-alist)))))

;;;###autoload
(defun register-jump (&optional register delete)
  "Like `jump-to-register' but show register preview after some delay."
  (interactive (list nil current-prefix-arg))
  (or (consp register-alist) (user-error "No register"))
  (let ((timer (run-with-timer register-jump-delay nil
                               #'register-jump-preview
                               register-jump-preview-buffer))
        (inhibit-quit t))
    (unwind-protect
        (let ((r (or register
                     (ignore-errors
                       (read-char (propertize "Jump to register: "
                                              'face 'minibuffer-prompt))))))
          (if (get-register r)
              (jump-to-register r delete)
            (push last-input-event unread-command-events)))
      (and (timerp timer) (cancel-timer timer))
      (let ((w (get-buffer-window register-jump-preview-buffer)))
        (and (window-live-p w) (delete-window w)))
      (and (get-buffer register-jump-preview-buffer)
           (kill-buffer register-jump-preview-buffer)))))

(provide 'register-jump)
;;; register-jump.el ends here
