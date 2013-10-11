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
;;
;; NOTE: Much of this feature has been merged upstream and will appear
;; in Emacs 24.4.

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  (unless (fboundp 'register-read-with-preview)
    (defcustom register-preview-delay 1
      "If non-nil delay in seconds to pop up the preview window."
      :type '(choice number (const :tag "Indefinitely" nil))
      :group 'register)

    (defun register-describe-oneline (c)
      "One-line description of register C."
      (let ((d (replace-regexp-in-string
                "\n[ \t]*" " "
                (with-output-to-string (describe-register-1 c)))))
        (if (string-match "Register.+? contains \\(?:an? \\|the \\)?" d)
            (substring d (match-end 0))
          d)))
    (defvar register-preview-functions nil)

    (defun register-preview (buffer &optional show-empty)
      "Pop up a window to show register preview in BUFFER.
If SHOW-EMPTY is non-nil show the window even if no registers."
      (when (or show-empty (consp register-alist))
        (let ((split-height-threshold 0))
          (with-temp-buffer-window
           buffer
           (cons 'display-buffer-below-selected
                 '((window-height . fit-window-to-buffer)))
           nil
           (with-current-buffer standard-output
             (setq cursor-in-non-selected-windows nil)
             (mapc
              (lambda (r)
                (insert (or (run-hook-with-args-until-success
                             'register-preview-functions r)
                            (format "%s %s\n"
                                    (concat (single-key-description (car r)) ":")
                                    (register-describe-oneline (car r))))))
              register-alist))))))

    (defun register-read-with-preview (prompt)
      "Read an event with register preview using PROMPT.
Pop up a register preview window if the input is a help char but
is not a register. Alternatively if `register-preview-delay' is a
number the preview window is popped up after some delay."
      (let* ((buffer "*Register Preview*")
             (timer (when (numberp register-preview-delay)
                      (run-with-timer register-preview-delay nil
                                      (lambda ()
                                        (unless (get-buffer-window buffer)
                                          (register-preview buffer))))))
             (help-chars (cl-loop for c in (cons help-char help-event-list)
                                  when (not (get-register c))
                                  collect c)))
        (unwind-protect
            (progn
              (while (memq (read-event (propertize prompt 'face 'minibuffer-prompt))
                           help-chars)
                (unless (get-buffer-window buffer)
                  (register-preview buffer 'show-empty)))
              last-input-event)
          (and (timerp timer) (cancel-timer timer))
          (let ((w (get-buffer-window buffer)))
            (and (window-live-p w) (delete-window w)))
          (and (get-buffer buffer) (kill-buffer buffer)))))))

(defun register-jump-shorten (s len)
  "Shorten string S to LEN."
  (if (<= (length s) len)
      s
    (concat (substring s 0 (- len 2)) " \u2026")))

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

(defun register-jump-describe-marker-or-file-query (r)
  (let* ((w (cdr r))
         (d (cond
             ((markerp w)
              (register-jump-get-line (marker-buffer w) w))
             ((and (consp w) (eq (car-safe w) 'file-query))
              (register-jump-get-line
               (get-file-buffer (nth 1 w)) (nth 2 w))))))
    (when d
      (format "%s %s\n"
              (concat (single-key-description (car r)) ":")
              d))))

;;;###autoload
(defun register-jump (&optional delete)
  "Like `jump-to-register' but show register preview after some delay."
  (interactive "P")
  (or (consp register-alist) (user-error "No registers"))
  (let ((register-preview-functions register-preview-functions))
    (add-hook 'register-preview-functions
              #'register-jump-describe-marker-or-file-query)
    (let ((c (register-read-with-preview "Jump to register: ")))
      (if (get-register c)
          (jump-to-register c delete)
        (push last-input-event unread-command-events)))))

(provide 'register-jump)
;;; register-jump.el ends here
