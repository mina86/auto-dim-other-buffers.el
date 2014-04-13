;;; auto-dim-other-buffers.el --- Makes non-current buffers less prominent -*- lexical-binding: t -*-

;; Copyright 2013 Steven Degutis
;; Copyright 2013,2014 Google Inc.
;; Copyright 2014 Justin Talbott

;; Author: Steven Degutis
;;	Michal Nazarewicz <mina86@mina86.com>
;; Maintainer: Michal Nazarewicz <mina86@mina86.com>
;; URL: https://github.com/mina86/auto-dim-other-buffers.el
;; Version: 1.5

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; The `auto-dim-other-buffers-mode' is a global minor mode which
;; makes non-current buffer less prominent making it more apparent
;; which window has a focus.

;; The preferred way to install the mode is by installing a package
;; form MELPA:
;;
;;     M-x package-install RET auto-dim-other-buffers RET

;; Once installed, the mode can be turned on (globally) with:
;;
;;     M-x auto-dim-other-buffers-mode RET

;; To make the mode enabled every time Emacs starts, add the following
;; to Emacs initialisation file (~/.emacs or ~/.emacs.d/init.el):
;;
;;     (add-hook 'after-init-hook (lambda ()
;;       (when (fboundp 'auto-dim-other-buffers-mode)
;;         (auto-dim-other-buffers-mode t))))

;; To configure how dimmed buffers look like, customise
;; `auto-dim-other-buffers-face'.  This can be accomplished by:
;;
;;     M-x customize-face RET auto-dim-other-buffers-face RET

;;; Code:

(defface auto-dim-other-buffers-face '((t :background "black"))
  "Face (presumably dimmed somehow) for non-current buffers."
  :group 'auto-dim-other-buffers)

(defvar adob--last-buffer nil
  "Buffer we were before command finished.")

(defun adob--ignore-buffer (buffer)
  "Return whether to ignore BUFFER and do not affect it's state.
Currently only mini buffer and echo areas are ignored."
  (or (null buffer)
      (minibufferp buffer)
      (string-match "^ \\*Echo Area" (buffer-name buffer))))

(defun adob--pre-command-hook ()
  "Record current buffer before the command is run."
  (setq adob--last-buffer (current-buffer)))

(defun adob--post-command-hook ()
  "If buffer has changed, dim the last one and undim the new one."
  ;; if we haven't switched buffers, do nothing
  (unless (eq (current-buffer) adob--last-buffer)

    ;; first, try to dim the last buffer.  if it's nil, then the
    ;; feature was just turned on and all buffers are already
    ;; dimmed. if it's just killed, don't try to set its face.
    (and (buffer-live-p adob--last-buffer)
         (not (adob--ignore-buffer adob--last-buffer))
         (with-current-buffer adob--last-buffer
           (buffer-face-set 'auto-dim-other-buffers-face)))

    ;; now, restore the current buffer, and undim it.
    (buffer-face-set nil)))

(defun adob--after-change-major-mode-hook ()
  "Dim or undim a new buffer if a new window, like help window, has popped up."
  (buffer-face-set (unless (eq (current-buffer) (window-buffer))
                     'auto-dim-other-buffers-face)))

(defun adob--window-configuration-change-hook ()
  "For command delete-window(C-x 0)."
  (adob--dim-all-buffers)
  (adob--after-change-major-mode-hook))

(defun adob--set-face-on-all-buffers (face)
  "Set FACE on all buffers which are not to be ignored.
Whether buffer should be ignored is determined by `adob--ignore-buffer'
function."
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (unless (adob--ignore-buffer buffer)
        (set-buffer buffer)
        (buffer-face-set face)))))

(defun adob--undim-all-buffers ()
  "Undim all buffers."
  (adob--set-face-on-all-buffers nil))

(defun adob--dim-all-buffers ()
  "Dim all buffers."
  (adob--set-face-on-all-buffers 'auto-dim-other-buffers-face))

(defun turn-off-auto-dim-other-buffers ()
  "Turn `auto-dim-other-buffers-mode' off."
  (remove-hook 'pre-command-hook 'adob--pre-command-hook)
  (remove-hook 'post-command-hook 'adob--post-command-hook)
  (remove-hook 'focus-out-hook 'adob--dim-all-buffers)
  (remove-hook 'focus-in-hook 'adob--after-change-major-mode-hook)
  (remove-hook 'after-change-major-mode-hook
               'adob--after-change-major-mode-hook)
  (remove-hook 'next-error-hook 'adob--after-change-major-mode-hook)
  (remove-hook 'window-configuration-change-hook
               'adob--window-configuration-change-hook)
  (adob--undim-all-buffers))

(defun turn-on-auto-dim-other-buffers ()
  "Turn `auto-dim-other-buffers-mode' on."
  (setq adob--last-buffer nil)
  (adob--dim-all-buffers)
  (add-hook 'pre-command-hook 'adob--pre-command-hook)
  (add-hook 'post-command-hook 'adob--post-command-hook)
  (add-hook 'focus-out-hook 'adob--dim-all-buffers)
  (add-hook 'focus-in-hook 'adob--after-change-major-mode-hook)
  (add-hook 'after-change-major-mode-hook 'adob--after-change-major-mode-hook)
  (add-hook 'next-error-hook 'adob--after-change-major-mode-hook)
  (add-hook 'window-configuration-change-hook
            'adob--window-configuration-change-hook))

;;;###autoload
(define-minor-mode auto-dim-other-buffers-mode
  "Visually makes non-current buffers less prominent"
  :lighter " Dim"
  :global t
  (if auto-dim-other-buffers-mode
      (turn-on-auto-dim-other-buffers)
    (turn-off-auto-dim-other-buffers)))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
