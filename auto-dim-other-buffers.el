;;; auto-dim-other-buffers.el --- Makes windows without focus less prominent -*- lexical-binding: t -*-
;; Author: Michal Nazarewicz <mina86@mina86.com>
;; Maintainer: Michal Nazarewicz <mina86@mina86.com>
;; URL: https://github.com/mina86/auto-dim-other-buffers.el
;; Version: 2.0.1

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

;; The ‘auto-dim-other-buffers-mode’ is a global minor mode which makes windows
;; without focus less prominent.  With many windows in a frame, the idea is that
;; this mode helps recognise which is the selected window by providing
;; a non-intrusive but still noticeable visual indicator.

;; The preferred way to install the mode is by grabbing ‘auto-dim-other-buffers’
;; package form MELPA:
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

;; More customisation can be found in ‘auto-dim-other-buffers’ customisation
;; group which can be accessed with:
;;
;;     M-x customize-group RET auto-dim-other-buffers RET

;; Note that despite it’s name, since Emacs 27 the mode operates on *windows*
;; rather than buffers.  I.e. selected window is highlighted and all other
;; windows are dimmed even if they display the same buffer.  In older Emacs
;; versions the mode falls back to the old behaviour where all windows
;; displaying selected buffer are highlighted.  This historic behaviour
;; is where the mode gets its name from.

;;; Code:

(defface auto-dim-other-buffers-face
  '((((background light)) :background "#eff") (t :background "#122"))
  "Face (presumably dimmed somehow) for non-selected window."
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-dim-on-focus-out t
  "Whether to dim all windows when frame looses focus."
  :type 'boolean
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-dim-on-switch-to-minibuffer t
  "Whether to dim last buffer when switching to minibuffer or echo area."
  :type 'boolean
  :group 'auto-dim-other-buffers)


(defconst adob--adow-mode (not (version< emacs-version "27.0.90"))
  "Whether Emacs supports :filtered faces.
If t, the code will run in ‘auto dim other window’ mode (hence
‘adow-mode’) which operates on windows rather than buffers.  To
operate on windows, Emacs must support :filtered face predicate
which has been added in Emacs 27.")

(defconst adob--remap-face
  (if adob--adow-mode
      '(:filtered (:window adob--dim t) auto-dim-other-buffers-face)
    'auto-dim-other-buffers-face)
  "Face to use when adding relative face remapping.
Depending on whether Emacs supports :filtered predicate, this
will or will not use it.  See ‘adob--adow-mode’.")

(defvar adob--last-buffer nil
  "Last selected buffer which, i.e. buffer which is currently not dimmed.")
(defvar adob--last-window nil
  "Last selected buffer which, i.e. window which is currently not dimmed.
This is only used in adow mode (i.e. if ‘adob--adow-mode’).")

(defun adob--never-dim-p (buffer)
  "Return whether to never dim BUFFER.
Currently, no hidden buffers (ones whose name starts with a space) are dimmed.
This is only used outside of adow-mode (i.e. if not ‘adob--adow-mode’)."
  (eq t (compare-strings " " 0 1 (buffer-name buffer) 0 1)))

(defvar-local adob--face-mode-remapping nil
  "Current face remapping cookie for `auto-dim-other-buffers-mode'.")

(defun adob--remap-face (buffer object)
  "Make sure face remapping is active in BUFFER.

Does not preserve current buffer and changes it to BUFFER if new
remapping had to be added.

If face remapping had to be added, force update of OBJECT which
can be a window (which forces update of only that window) or
a buffer (which forces update of all windows displaying that
buffer).  To keep track of the face remapping, update
‘adob--face-mode-remapping’.

Return non-nil if new remapping has been added; nil if it was
already active in current buffer."
  (unless (buffer-local-value 'adob--face-mode-remapping buffer)
    (set-buffer buffer)
    (force-window-update object)
    (setq adob--face-mode-remapping
          (face-remap-add-relative 'default adob--remap-face))))

(defun adob--unmap-face (buffer object)
  "Make sure face remapping is inactive in BUFFER.

Does not preserve current buffer and changes it to BUFFER if new
remapping had to be added.

If face remapping had to be removed, force update of OBJECT which
can be a window (which forces update of only that window) or
a buffer (which forces update of all windows displaying that
buffer).  To keep track of the face remapping, update
‘adob--face-mode-remapping’.

Return t if remapping has been removed; nil if it was not active
in current buffer."
  (when (buffer-local-value 'adob--face-mode-remapping buffer)
    (set-buffer buffer)
    (face-remap-remove-relative adob--face-mode-remapping)
    (setq adob--face-mode-remapping nil)
    (force-window-update object)
    t))

(defun adob--dim-buffer (buffer &optional except-in)
  "Dim BUFFER if not already dimmed except in EXCEPT-IN window.

Does not preserve current buffer and changes it to BUFFER if new
remapping had to be added.

EXCEPT-IN only works if the code is running in adow mode (see
‘adob--adow-mode’) and it works by deactivating the dimmed face
in specified window."
  (when (adob--remap-face buffer buffer)
    (dolist (wnd (and adob--adow-mode
                      (get-buffer-window-list buffer 'n 'visible)))
      (set-window-parameter wnd 'adob--dim (not (eq wnd except-in))))))

(defun adob--update ()
  "Make sure that selected window is not dimmed.
Dim previously selected window if selection has changed."
  (when (or auto-dim-other-buffers-dim-on-switch-to-minibuffer
            (not (window-minibuffer-p)))
    (let* ((wnd (selected-window))
           (buf (window-buffer wnd)))

      ;; If window has changed, update old and new window’s parameters.
      (when (and adob--adow-mode
                 (not (eq wnd adob--last-window)))
        (when (and (window-live-p adob--last-window)
                   (not (window-minibuffer-p adob--last-window)))
          (set-window-parameter adob--last-window 'adob--dim t)
          (force-window-update adob--last-window))
        (setq adob--last-window wnd)
        (unless (window-minibuffer-p adob--last-window)
          (set-window-parameter adob--last-window 'adob--dim nil)
          (force-window-update adob--last-window)))

      ;; If buffer has changed, update their status.
      (unless (eq buf adob--last-buffer)
        (save-current-buffer
          (when (buffer-live-p adob--last-buffer)
            (adob--dim-buffer adob--last-buffer wnd))
          (setq adob--last-buffer buf)
          (if adob--adow-mode
              (adob--remap-face buf buf)
            (adob--unmap-face buf buf)))))))

(defun adob--rescan-windows ()
  "Rescan all windows in selected frame and dim all non-selected windows."
  (let* ((selected-window (selected-window))
         (selected-buffer (window-buffer selected-window))
         (windows (window-list nil 'n)))
    (save-current-buffer
      (while windows
        (let* ((wnd (car windows))
               (buf (window-buffer wnd)))
          (setq windows (cdr windows))
          (cond (adob--adow-mode
                 ;; Update window’s ‘adob--dim’ parameter.  If it changes set
                 ;; we’ll also later tell Emacs to redisplay the window.
                 (let ((new (not (eq wnd selected-window))))
                   (unless (eq new (window-parameter wnd 'adob--dim))
                     (set-window-parameter wnd 'adob--dim new)
                     (force-window-update wnd)))
                 ;; In adow-mode, make sure that the buffer has remapped faces.
                 (adob--remap-face buf wnd))
                ;; Outside of adow-mode, add or remove face remapping depending
                ;; on whether current buffer selected buffer or not.
                ((eq buf selected-buffer)
                 (adob--unmap-face buf wnd))
                ((adob--never-dim-p buf)
                 (adob--remap-face buf wnd))))))))

(defun adob--buffer-list-update-hook ()
  "React to buffer list changes.
If selected buffer has changed, change which buffer is dimmed.
Otherwise, if a new buffer is displayed somewhere, dim it."
  (let ((current (current-buffer)))
    (if (eq (window-buffer) current)
        ;; Selected buffer has changed.  Update what we dim.
        (adob--update)
      ;; A new buffer is displayed somewhere but it’s not the selected one so
      ;; dim it.
      (unless (adob--never-dim-p current)
        (adob--dim-buffer current)))))

(defun adob--focus-out-hook ()
  "Dim all buffers if `auto-dim-other-buffers-dim-on-focus-out'."
  (cond ((not (and auto-dim-other-buffers-dim-on-focus-out
                   (buffer-live-p adob--last-buffer))))
        ((and (window-live-p adob--last-window)
              (not (window-minibuffer-p adob--last-window)))
         (set-window-parameter adob--last-window 'adob--dim t)
         (force-window-update adob--last-window)
         (setq adob--last-buffer nil
               adob--last-window nil))
        ((not (or adob--adow-mode
                  (adob--never-dim-p adob--last-buffer)))
         (save-current-buffer (adob--dim-buffer adob--last-buffer))
         (setq adob--last-buffer nil))))

(defun adob--focus-change-hook ()
  "Based on focus status of selected frame dim or undim selected buffer.
Do nothing if `auto-dim-other-buffers-dim-on-focus-out' is nil
and frame’s doesn’t have focus."
  ;; ‘after-focus-change-function’ has been added at the same time as
  ;; ‘frame-focus-state’ function so if we’re here we know that function is
  ;; defined.
  (if (with-no-warnings (frame-focus-state))
      (adob--update)
    (adob--focus-out-hook)))

;;;###autoload
(define-minor-mode auto-dim-other-buffers-mode
  "Visually makes windows without focus less prominent.

Windows without input focus are made to look less prominent by
applying ‘auto-dim-other-buffers-face’ to them.  With many
windows in a frame, the idea is that this mode helps recognise
which is the selected window by providing a non-intrusive but
still noticeable visual indicator.

Note that despite it’s name, since Emacs 27 this mode operates
on *windows* rather than buffers.  In older versions of Emacs, if
a buffer was displayed in multiple windows, none of them would be
dimmed even though at most one could have focus.  This historic
behaviour is where the mode gets its name from."
  :global t
  ;; Add/remove all hooks
  (let ((callback (if auto-dim-other-buffers-mode #'add-hook #'remove-hook)))
    (funcall callback 'window-configuration-change-hook #'adob--rescan-windows)
    (funcall callback 'buffer-list-update-hook #'adob--buffer-list-update-hook)
    ;; Prefer ‘after-focus-change-function’ (which was added in Emacs 27)
    ;; to ‘focus-out-hook’ and ‘focus-in-hook’.
    (if (boundp 'after-focus-change-function)
        (if auto-dim-other-buffers-mode
            (add-function :after after-focus-change-function
                          #'adob--focus-change-hook)
          (remove-function after-focus-change-function
                           #'adob--focus-change-hook))
      (funcall callback 'focus-out-hook #'adob--focus-out-hook)
      (funcall callback 'focus-in-hook #'adob--update)))

  (save-current-buffer
    (if auto-dim-other-buffers-mode
        ;; Dim all except for the current buffer.  When running in adow mode,
        ;; dim current buffer as well except for in selected window.  If not
        ;; running in adow mode, don’t dim hidden buffers either (most notably
        ;; we care about Minibuffer and Echo Area buffers).
        (progn
          (setq adob--last-buffer (window-buffer)
                adob--last-window (if adob--adow-mode (selected-window)))
          (dolist (buffer (buffer-list))
            (when (or adob--adow-mode
                      (not (or (eq buffer adob--last-buffer)
                               (adob--never-dim-p buffer))))
              (adob--dim-buffer buffer adob--last-window))))

      ;; Clean up by removing all face remaps.
      (setq adob--last-buffer nil
            adob--last-window nil)
      (dolist (buffer (buffer-list))
        ;; Kill the local variable even if it’s already set to nil.  This is why
        ;; we’re checking whether it’s a local variable rather than it’s value
        ;; in the buffer.
        (when (local-variable-p 'adob--face-mode-remapping buffer)
          (set-buffer buffer)
          (adob--unmap-face buffer buffer)
          (kill-local-variable 'adob--face-mode-remapping))))))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
