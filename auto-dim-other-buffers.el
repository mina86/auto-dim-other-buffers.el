;;; auto-dim-other-buffers.el --- Visually makes non-current buffers less prominent

;; Author: Steven Degutis
;; URL: https://github.com/sdegutis/auto-dim-other-buffers.el
;; Version: 1.0

(make-face 'sd/dimmed-font)
(set-face-attribute 'sd/dimmed-font nil :background "black")

(defun turn-off-auto-dim-other-buffers ()
  (interactive)
  (remove-hook 'post-command-hook 'sd/auto-dim-other-buffers)
  (let ((original (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (buffer-face-set nil))
    (set-buffer original)
    (buffer-face-set nil)))

(defun turn-on-auto-dim-other-buffers ()
  (interactive)
  (add-hook 'post-command-hook 'sd/auto-dim-other-buffers))

(defun sd/auto-dim-other-buffers ()
  (let ((original (current-buffer)))
    (dolist (buffer (buffer-list))
      (unless (minibufferp buffer)
        (set-buffer buffer)
        (buffer-face-set 'sd/dimmed-font)))
    (set-buffer original)
    (buffer-face-set nil)))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
