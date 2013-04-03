;;; auto-dim-other-buffers.el --- Visually makes non-current buffers less prominent

;; Author: Steven Degutis
;; URL: https://github.com/sdegutis/auto-dim-other-buffers.el
;; Version: 1.2

(defface auto-dim-other-buffers-face '((t :background "black"))
  "Face (presumably dimmed somehow) for non-current buffers."
  :group 'auto-dim-other-buffers-mode)

(defun adob/pre-command-hook ()
  (setq adob/last-buffer (current-buffer)))

(defun adob/post-command-hook ()
  (let ((original (current-buffer)))

    ;; if we haven't switched buffers, do nothing
    (unless (eq original adob/last-buffer)

      ;; first, try to dim the last buffer.  if it's nil, then the
      ;; feature was just turned on and all buffers are already
      ;; dimmed. if it's just killed, don't try to set its face.
      (when (and adob/last-buffer
                 (buffer-live-p adob/last-buffer)
                 ;; (not (minibufferp adob/last-buffer)) ;; this doesn't do what i want
                 )

        (set-buffer adob/last-buffer)
        (buffer-face-set 'auto-dim-other-buffers-face))

      ;; now, restore the current buffer, and undim it.
      (set-buffer original)
      (buffer-face-set nil))))

(defun adob/set-face-on-all-buffers (face)
  (let ((original (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (buffer-face-set face))
    (set-buffer original)))

(defun adob/undim-all-windows ()
  (adob/set-face-on-all-buffers nil))

(defun adob/dim-all-windows ()
  (adob/set-face-on-all-buffers 'auto-dim-other-buffers-face))

(defun turn-off-auto-dim-other-buffers ()
  (message "turning off")
  (remove-hook 'pre-command-hook 'adob/pre-command-hook)
  (remove-hook 'post-command-hook 'adob/post-command-hook)
  (adob/undim-all-windows))

(defun turn-on-auto-dim-other-buffers ()
  (message "turning on")
  (setq adob/last-buffer nil)
  (adob/dim-all-windows)
  (add-hook 'pre-command-hook 'adob/pre-command-hook)
  (add-hook 'post-command-hook 'adob/post-command-hook))

(define-minor-mode auto-dim-other-buffers-mode
  "Visually makes non-current buffers less prominent"
  :lighter " auto-dim"
  (if auto-dim-other-buffers-mode
      (turn-on-auto-dim-other-buffers)
    (turn-off-auto-dim-other-buffers)))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
