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

(setq adob/last-buffer nil)

(defun adob/pre-command-hook ()
  (setq adob/last-buffer (current-buffer)))

(defun adob/post-command-hook ()
  (let ((original (current-buffer)))
    (unless (eq original adob/last-buffer)
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (unless (minibufferp)
          (buffer-face-set auto-dim-other-buffers-face)))
      (set-buffer original)
      (buffer-face-set nil))))

(defun adob/clear-all-windows ()
  (interactive)
  (let ((original (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (buffer-face-set nil))
    (set-buffer original)))

(defun turn-off-auto-dim-other-buffers ()
  (interactive)
  (remove-hook 'pre-command-hook 'adob/pre-command-hook)
  (remove-hook 'post-command-hook 'adob/post-command-hook)
  (adob/clear-all-windows))

(defun turn-on-auto-dim-other-buffers ()
  (interactive)
  (setq adob/last-buffer nil)
  (add-hook 'pre-command-hook 'adob/pre-command-hook)
  (add-hook 'post-command-hook 'adob/post-command-hook))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
