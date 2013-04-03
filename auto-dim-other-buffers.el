;;; auto-dim-other-buffers.el --- Visually makes non-current buffers less prominent

;; Author: Steven Degutis
;; URL: https://github.com/sdegutis/auto-dim-other-buffers.el
;; Version: 1.0

(make-face 'sd/dimmed-font)
(set-face-attribute 'sd/dimmed-font nil :background "black")

(defcustom auto-dim-other-buffers-face 'sd/dimmed-font
  "Face (presumably dimmed somehow) for non-current buffers."
  :type 'face
  :group 'auto-dim-other-buffers)

(defun adob/pre-command-hook ()
  (setq adob/last-buffer (current-buffer)))

(defun adob/post-command-hook ()
  (let ((original (current-buffer)))
    (unless (eq original adob/last-buffer) ;; if we haven't switched buffers, do nothing
      (when (and adob/last-buffer
                 (buffer-live-p adob/last-buffer)
                 (not (minibufferp adob/last-buffer)))
        (set-buffer adob/last-buffer)
        (buffer-face-set auto-dim-other-buffers-face))
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
  (adob/set-face-on-all-buffers auto-dim-other-buffers-face))

(defun turn-off-auto-dim-other-buffers ()
  (interactive)
  (remove-hook 'pre-command-hook 'adob/pre-command-hook)
  (remove-hook 'post-command-hook 'adob/post-command-hook)
  (adob/undim-all-windows))

(defun turn-on-auto-dim-other-buffers ()
  (interactive)
  (setq adob/last-buffer nil)
  (adob/dim-all-windows)
  (add-hook 'pre-command-hook 'adob/pre-command-hook)
  (add-hook 'post-command-hook 'adob/post-command-hook))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
