;;; auto-dim-other-buffers.el --- Makes windows without focus less prominent -*- lexical-binding: t -*-
;; Author: Michal Nazarewicz <mina86@mina86.com>
;; URL: https://github.com/mina86/auto-dim-other-buffers.el
;; Version: 2.2.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces

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

;; The `auto-dim-other-buffers-mode' is a global minor mode which makes windows
;; without focus less prominent.  With many windows in a frame, this mode helps
;; recognise which is the selected window by providing a non-intrusive but still
;; noticeable visual indicator.

;; The mode provides two indications of the selected window.  Firstly,
;; background of non-selected windows is dimmed.  Secondly, fringes of the
;; selected windows are highlighted.

;; # Installation

;; The preferred way to install the mode is by grabbing
;; `auto-dim-other-buffers' package form NonGNU ELPA:

;;     M-x package-install RET auto-dim-other-buffers RET

;; Once installed, enable the mode with:

;;     M-x auto-dim-other-buffers-mode RET

;; To make the mode enabled every time Emacs starts, add the following to Emacs
;; initialisation file (see `user-init-file'):

;;     (add-hook 'after-init-hook (lambda ()
;;       (when (fboundp 'auto-dim-other-buffers-mode)
;;         (auto-dim-other-buffers-mode t))))

;; To configure how dimmed buffers look, customise
;; `auto-dim-other-buffers'.  This can be accomplished by:

;;     M-x customize-face RET auto-dim-other-buffers RET

;; More options can be found in `auto-dim-other-buffers' customisation
;; group which can be accessed with:

;;     M-x customize-group RET auto-dim-other-buffers RET

;; Highlighting of fringes can be done by removing `fringe' entry from
;; `auto-dim-other-buffers-affected-faces' list.  Either via customising
;; the variable or using the following snippet:

;;     (setq auto-dim-other-buffers-affected-faces
;;           (assq-delete-all 'fringe auto-dim-other-buffers-affected-faces))


;; ## Troubleshooting

;; ### My screen is flickering

;; By its nature, `auto-dim-other-buffers-mode' often forces full-window
;; refreshes which may cause flickering on some systems and displays.  To
;; mitigate it, try disabling `fringe' highlighting which—due to Emacs’ display
;; code limitation—require full-frame refresh.  See Customisation section above
;; for instruction how to do it.

;; ### Text which should be hidden in org-mode is not

;; To hide text, `org-mode' uses `org-hide' face whose foreground is set to the
;; background colour of the `default' face.  When `auto-dim-other-buffers-mode'
;; changes background of a dimmed window it also needs to be applied to the
;; `org-hide' face.  The good news is that this is supported out of the box.
;; The caveat is that it requires that `auto-dim-other-buffers' and
;; `auto-dim-other-buffers-hide' are changed in sync.

;; If text which should be hidden in org-mode is visible faintly, the most
;; likely reason is that the latter face has not been updated.  The solution is
;; to customise it via

;;     M-x customize-face RET auto-dim-other-buffers-hide-face RET

;; and set its foreground and background to match background of the
;; `auto-dim-other-buffers'.


;; ## Afterword

;; Note that despite it, the mode operates on *windows* rather than buffers.  In
;; other words, selected window is highlighted and all other windows are dimmed
;; even if they display the same buffer.  The package is named
;; `auto-dim-other-buffer' for historical reasons.

;;; Code:

(require 'face-remap)


(defgroup auto-dim-other-buffers nil
  "Visually makes windows without focus less prominent."
  :prefix "auto-dim-other-buffers-"
  :group 'convenience)

(defface auto-dim-other-buffers
  '((((background light)) :background "#eff") (t :background "#122"))
  "Face with a (presumably) dimmed background for non-selected window.

By default it is applied to, among others, the ‘default’ face and is
intended to affect background of non-selected windows.  A related
‘auto-dim-other-buffers-hide’ face is intended for faces which need
their foreground to be changed in sync.  Which faces are modified is
configured by the ‘auto-dim-other-buffers-affecteds’ variable."
  :group 'auto-dim-other-buffers)
(define-obsolete-face-alias 'auto-dim-other-buffers-face
                            'auto-dim-other-buffers
                            "2.2.1")

(defface auto-dim-other-buffers-hide
  '((((background light)) :foreground "#eff" :background "#eff")
    (t                    :foreground "#122" :background "#122"))
  "Face with a (presumably) dimmed background and matching foreground.

The intention is that the face has the same foreground and
background as the background of ‘auto-dim-other-buffers’ and
that it’s used as remapping for faces which hide the text by
rendering it in the same colour as background.

By default it is applied to the ‘org-hide’ face and is intended
to modify foreground of faces which hide the text by rendering it
in the same colour as the background.  Since the mode alters the
background in a window such faces need to be updated as well.

Which faces are modified is configured by the
‘auto-dim-other-buffers-affecteds’ variable."
  :group 'auto-dim-other-buffers)
(define-obsolete-face-alias 'auto-dim-other-buffers-hide-face
                            'auto-dim-other-buffers-hide
                            "2.2.1")

(defvar auto-dim-other-buffers-affected-faces) ; Forward declaration.

(defcustom auto-dim-other-buffers-dim-on-focus-out t
  "Whether to dim all windows when frame looses focus."
  :type 'boolean
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-dim-on-switch-to-minibuffer t
  "Whether to dim last buffer when switching to minibuffer or echo area."
  :type 'boolean
  :group 'auto-dim-other-buffers)


(defvar adob--last-buffer nil
  "Last selected buffer, i.e. buffer which is currently not dimmed.")
(defvar adob--last-window nil
  "Last selected window, i.e. window which is currently not dimmed.")

(defun adob--never-dim-p (buffer)
  "Return whether to never dim BUFFER.
Call ‘auto-dim-other-buffers-never-dim-buffer-functions’ to see
if any of them return non-nil in which case the BUFFER won’t be
dimmed."
  (run-hook-with-args-until-success
   'auto-dim-other-buffers-never-dim-buffer-functions buffer))


(defface adob--hack nil "A hack to make fringe refresh work.  Do not use.")
(defvar adob--has-fringes nil
  "Whether we are remapping `fringe' face; see `adob--has-fringes--refresh'.")

(defun adob--has-fringes--refresh ()
  "Refresh value of `adob--has-fringes'
based on ‘auto-dim-other-buffers-affected-faces’ variable."
  (setq
   adob--has-fringes
   (let ((spec (cdr-safe (assq 'fringe auto-dim-other-buffers-affected-faces))))
     (and spec (or (symbolp spec) (car-safe spec) (cdr-safe spec)) t))))

(defun adob--force-window-update (object)
  "Force window to be updated on next redisplay.
This does more than `force-window-update' by also forcing redisplay of
fringes if necessary (see `adob--has-fringes').  This is done by forcing
redisplay of frames containing affected windows."
  (force-window-update object)
  (when adob--has-fringes
    (adob--force-fringes-refresh (if (windowp object)
                                     (list object)
                                   (get-buffer-window-list object nil t)))))

(defun adob--positive-assqp (symbol params)
  "Check that SYMBOL entry in PARAMS alist is a positive number."
  (let ((value (cdr-safe (assq symbol params))))
    (and (numberp value) (> value 0))))

(defun adob--force-fringes-refresh (windows)
  "Force refresh of fringes in WINDOWS.
This is done by forcing full frame redraws."
  (dolist (frame (delete-dups (mapcar #'window-frame windows)))
    (let ((params (frame-parameters frame)))
      (when (or (adob--positive-assqp 'right-fringe params)
                (adob--positive-assqp 'left-fringe params))
        ;; By changing the 'adob--hack face, we force the redisplay of the
        ;; frame.  (Tracing the C code, I believe what we’re after is setting
        ;; f->face_change and calling fset_redisplay).  We could instead call
        ;; redraw-frame but my intuition tells me that that’s a slower
        ;; operation, though honestly I dunno.
        ; (redraw-frame frame)
        (let ((value (face-attribute 'adob--hack :inverse-video frame nil)))
          (internal-set-lisp-face-attribute
           'adob--hack :inverse-video (not value) frame))))))


(defvar-local adob--face-mode-remapping nil
  "Current face remapping cookie for `auto-dim-other-buffers-mode'.")

(defun adob--remap-add-relative ()
  "Map all necessary relative face in current buffer.
Updates ‘adob--face-mode-remapping’ variable accordingly and returns its
new value."
  (setq adob--face-mode-remapping
        (delq nil (mapcar #'adob--remap-add-relative-process-entry
                          auto-dim-other-buffers-affected-faces))))

(defun adob--remap-add-relative-process-entry (entry)
  "Add a single face mapping specified in ENTRY.
ENTRY is either '(DIM-FACE . HIGHLIGHT-FACE) cons or (for backwards
compatibility) 'DIM-FACE."
  (let ((face (car entry)) (spec (cdr entry)) args)
    (let ((add (lambda (value face)
                 (when face
                   (push `(:filtered (:window adob--dim ,value) ,face) args)))))
      ;; spec is either 'dim-face or '(dim-face . high-face) and either of the
      ;; faces can be nil.
      (funcall add t   (if (consp spec) (car spec) spec))
      (funcall add nil (cdr-safe spec)))
    (when args
      (apply #'face-remap-add-relative face args))))

(defun adob--remap-remove-relative ()
  "Remove all relative mappings that we’ve added.
List of existing mappings is taken from ‘adob--face-mode-remapping’
variable whose local value is killed afterwards."
  (mapc #'face-remap-remove-relative adob--face-mode-remapping)
  (kill-local-variable 'adob--face-mode-remapping))

(defun adob--remap-cycle-all (add)
  "Remove and re-add face remappings in all buffers where they exist.
If ADD is nil, do not re-add the mappings.

This needs to be called after ‘auto-dim-other-buffers-affected-faces’ is
changed to update state of all affected buffers (which is done when the
variable is changed via Customize).  It is also used when disabling the
adob mode."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list))
        ;; Check if adob--face-mode-remapping has local value indicating we have
        ;; influence over the buffer. The value may be nil (if there are no
        ;; affected faces) which is why we’re not simply reading the value.
        (when (local-variable-p 'adob--face-mode-remapping buffer)
          (set-buffer buffer)
          (let* ((had-some (prog1 adob--face-mode-remapping
                             (adob--remap-remove-relative)))
                 (has-some (and add
                                (not (adob--never-dim-p buffer))
                                (adob--remap-add-relative))))
            (when (or had-some has-some)
              (push buffer buffers)
              (force-window-update buffer))))))
    (when (adob--has-fringes--refresh)
      (adob--force-fringes-refresh
       (apply #'nconc (mapcar (lambda (b) (get-buffer-window-list b 'n t))
                              buffers))))))

(defun adob--remap-faces (buffer object)
  "Make sure face remappings are active in BUFFER unless its never-dim.

Does not preserve current buffer.

If BUFFER is never-dim (as determined by ‘adob--never-dim-p’),
remove adob face remappings from it.  Otherwise, make sure the
remappings are active by adding them if it’s missing.

If face remapping had to be changed, force update of OBJECT,
which can be a window or a buffer.

Return non-nil if remappings have been added to BUFFER."
  (let ((wants (not (adob--never-dim-p buffer)))
        (has (buffer-local-value 'adob--face-mode-remapping buffer)))
    (when (eq wants (not has))
      (set-buffer buffer)
      (if wants
          (adob--remap-add-relative)
        (adob--remap-remove-relative))
      (adob--force-window-update object)
      wants)))

(defun adob--kill-all-local-variables-advice (kill &rest args)
  "Call KILL with ARGS and restore face remapping.
Intended as an advice around ‘kill-all-local-variables’ function which
kills all local variables and removes all face remapping."
  (when (prog1 adob--face-mode-remapping (apply kill args))
    (adob--remap-add-relative)
    nil))

(defun adob--dim-buffer (buffer &optional except-in)
  "Dim BUFFER if not already dimmed except in EXCEPT-IN window.

Does not preserve current buffer.

EXCEPT-IN works by deactivating the dimmed face in specified window."
  (when (adob--remap-faces buffer buffer)
    (dolist (wnd (get-buffer-window-list buffer 'n 'visible))
      (set-window-parameter wnd 'adob--dim (not (eq wnd except-in))))))

(defun adob--update ()
  "Make sure that selected window is not dimmed.
Dim previously selected window if selection has changed."
  (when (or auto-dim-other-buffers-dim-on-switch-to-minibuffer
            (not (window-minibuffer-p)))
    (let* ((wnd (selected-window))
           (buf (window-buffer wnd)))

      (unless (eq wnd adob--last-window)
        ;; Window has changed.  Update the adob--dim parameter accordingly.
        (when (and (window-live-p adob--last-window)
                   (not (window-minibuffer-p adob--last-window)))
          (set-window-parameter adob--last-window 'adob--dim t)
          (adob--force-window-update adob--last-window))
        (setq adob--last-window wnd)
        (unless (window-minibuffer-p adob--last-window)
          (set-window-parameter adob--last-window 'adob--dim nil)
          (adob--force-window-update adob--last-window)))

      ;; If buffer has changed, update its status.
      (unless (eq buf adob--last-buffer)
        (save-current-buffer
          (when (buffer-live-p adob--last-buffer)
            (adob--dim-buffer adob--last-buffer wnd))
          (setq adob--last-buffer buf)
          (adob--remap-faces buf buf))))))

(defun adob--rescan-windows ()
  "Rescan all windows in selected frame and dim all non-selected windows."
  (let* ((selected-window (selected-window))
         (selected-buffer (window-buffer selected-window)))
    (save-current-buffer
      (dolist (wnd (window-list nil 'n))
        (let ((buf (window-buffer wnd)))
          ;; Update window’s ‘adob--dim’ parameter.  If it changes set
          ;; we’ll also later tell Emacs to redisplay the window.
          (let ((new (not (eq wnd selected-window))))
            (unless (eq new (window-parameter wnd 'adob--dim))
              (set-window-parameter wnd 'adob--dim new)
              (adob--force-window-update wnd)))
          ;; Make sure that the buffer has remapped faces.
          (adob--remap-faces buf wnd))))))

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
      (adob--dim-buffer current))))

(defvar adob--focus-change-debounce-delay 0.015
  "Delay in seconds to use when debouncing focus change events.
Window manager may send spurious focus change events.  To filter
them, the code delays handling of focus-change events by this
number of seconds.  Based on rudimentary testing, 0.015 (i.e. 15
milliseconds) is a good compromise between performing the
filtering and introducing a visible delay.

Setting this variable to zero will disable the debouncing.")

(defvar adob--focus-change-timer nil
  "Timer used to debounce focus change events.
Timer used by ‘adob--focus-change-hook’ when debouncing focus
change events.  The actual delay is specified by the
`adob--focus-change-debounce-delay` variable.")

(defvar adob--focus-change-last-state 'force-update
  "Last ‘frame-focus-state’ when handling focus change event.
Window manager may send spurious focus change events.  The code
attempts to debounce them but this may result in getting a change
event even if the focus state hasn’t changed.  This variable
stores the last state we’ve seen so that we can skip doing any
work if it hasn’t changed.")

(defun adob--focus-change ()
  "Based on focus status of selected frame dim or undim selected buffer.
Do nothing if `auto-dim-other-buffers-dim-on-focus-out' is nil
and frame’s doesn’t have focus."
  ;; Reset the timer variable so `adob--focus-change-hook’ will schedule us
  ;; Again.
  (setq adob--focus-change-timer nil)
  ;; ‘after-focus-change-function’ has been added at the same time as
  ;; ‘frame-focus-state’ function so if we’re here we know that function is
  ;; defined.
  (let ((state (with-no-warnings (frame-focus-state))))
    (unless (eq adob--focus-change-last-state state)
      (setq adob--focus-change-last-state state)
      (cond (state (adob--update))
            ((and auto-dim-other-buffers-dim-on-focus-out
                  (buffer-live-p adob--last-buffer))
             (when (and (window-live-p adob--last-window)
                        (not (window-minibuffer-p adob--last-window)))
               (set-window-parameter adob--last-window 'adob--dim t)
               (force-window-update adob--last-window))
             (setq adob--last-buffer nil
                   adob--last-window nil))))))

(defun adob--focus-change-hook ()
  "Debounce focus-change event and call `adob--focus-change'."
  (cond ((<= adob--focus-change-debounce-delay 0) (adob--focus-change))
        ((not adob--focus-change-timer)
         (setq adob--focus-change-timer
               (run-with-timer adob--focus-change-debounce-delay nil
                               #'adob--focus-change)))))

(defun adob--initialize ()
  "Dim all except for the selected buffer."
  ;; Dim current buffer as well except in selected window.
  (setq adob--last-window (selected-window)
        adob--last-buffer (window-buffer adob--last-window))
  (dolist (buffer (buffer-list))
    (adob--dim-buffer buffer adob--last-window)))

;;;###autoload
(define-minor-mode auto-dim-other-buffers-mode
  "Visually makes windows without focus less prominent.

Windows without input focus are made to look less prominent by applying
‘auto-dim-other-buffers’ to them.  With many windows in a frame,
the idea is that this mode helps recognise which is the selected window
by providing a non-intrusive but still noticeable visual indicator.

Beware: This mode may cause flickering, especially if fringe changing is
enabled (which is the default).  To mitigate the flickering, try
removing fringe changing (see `auto-dim-other-buffers-affected-faces').

Note: Despite it’s name, this mode operates on *windows* rather than
buffers, i.e. even if a buffer is shown in multiple windows, only one of
them is considered selected and all other will be dimmed.  Historically,
prior to Emacs 27, all or none windows displaying a buffer would be
dimmed; this historical behaviour is where the mode gets its name from."
  :global t
  :group 'auto-dim-other-buffers
  (adob--has-fringes--refresh)

  ;; Add/remove all hooks
  (let ((callback (if auto-dim-other-buffers-mode #'add-hook #'remove-hook)))
    (funcall callback 'window-configuration-change-hook #'adob--rescan-windows)
    (funcall callback 'buffer-list-update-hook #'adob--buffer-list-update-hook)
    ;; ‘after-focus-change-function’ has been added in Emacs 27 so we’re safe
    ;; using it.
    (if auto-dim-other-buffers-mode
        (add-function :after after-focus-change-function
                      #'adob--focus-change-hook)
      (remove-function after-focus-change-function
                       #'adob--focus-change-hook)))

  ;; Kill focus change timer if present.  If we’re enabling the mode we want to
  ;; start from fresh state.  If we’re disabling the mode, we don’t want the
  ;; timer anyway.
  (when adob--focus-change-timer
    (cancel-timer adob--focus-change-timer)
    (setq adob--focus-change-timer nil))

  (save-current-buffer
    (if auto-dim-other-buffers-mode
        (progn
          (advice-add #'kill-all-local-variables :around
                      #'adob--kill-all-local-variables-advice)
          (adob--initialize))
      (advice-remove #'kill-all-local-variables
                     #'adob--kill-all-local-variables-advice)
      (setq adob--last-buffer nil adob--last-window nil)
      (adob--remap-cycle-all nil))))


(defcustom auto-dim-other-buffers-never-dim-buffer-functions nil
  "A list of functions run to determine if a buffer should stay lit.
Each function is called with buffer as its sole argument.  If any
of them returns non-nil, the buffer will not be dimmed even if
it’s not selected one.

Each hook function should return the same value for the lifespan
of a buffer.  Otherwise, display state of a buffers may be
inconsistent with the determination of a hook function and remain
stale until the buffer is selected.  Tests based on buffer name
will work well, but tests based on major mode, buffer file name
or other properties which may change during lifespan of a buffer
may be problematic.

Changing this variable outside of customize does not immediately
update display state of all affected buffers."
  :type 'hook
  :group 'auto-dim-other-buffers
  :set (lambda (symbol value)
         (set-default symbol value)
         (when auto-dim-other-buffers-mode
           (save-current-buffer*
             (adob--initialize)))
         value))

(defcustom auto-dim-other-buffers-affected-faces
  '((default   . (auto-dim-other-buffers      . nil))
    (fringe    . (auto-dim-other-buffers      . nil))
    (org-block . (auto-dim-other-buffers      . nil))
    (org-hide  . (auto-dim-other-buffers-hide . nil)))
  "A list of faces affected when dimming/highlighting a window.

The list comprising of (FACE . (DIM-FACE . HIGH-FACE)) cons pairs.
FACE is an existing face for which a remapping will be added (see
`face-remap-add-relative').  DIM-FACE and HIGH-FACE are remapping faces
which are active in dimmed and highlighted windows respectively.  Either
face can be nil; if they are both nil, the entry has no effect.

Typically, DIM-FACE is either ‘auto-dim-other-buffers’ or
‘auto-dim-other-buffers-hide’.  The former is used when the
background of the face needs to be dimmed while the latter when in
addition the foreground needs to be set to match the background.

HIGH-FACE allows highlighting the selected window, for example as shown
in example below.  Alas, it’s then up to the user to properly set up
faces such that all of the highlighting works.

    (setq auto-dim-other-buffers-affected-faces
          '((default   . (nil . auto-dim-other-buffers))
            (fringe    . (nil . mode-line-active))
            (org-block . (nil . auto-dim-other-buffers))
            (org-hide  . (nil . auto-dim-other-buffers-hide))))

Beware: inclusion of `fringe' face in the list forces a more expensive
redraw procedure to be used.  This may cause additional flickering on
some systems.  If you’re observing flickering, try removing the `fringe'
entry, e.g. by using code such as:

    (setq auto-dim-other-buffers-affected-faces
          (assq-delete-all 'fringe auto-dim-other-buffers-affected-faces))

For backwards compatibility, a (FACE . DIM-FACE) format for the entries
is also accepted.  (Although, setting that is not supported through
Customize).

Changing this variable outside of Customize does not update display
state of affected buffers and requires toggling the mode off and on."
  ;; TODO: We’re using (symbol ...) rather than (face ...)  in the type
  ;; definition because the former breaks if the face is not defined.  This
  ;; happens in our case since org isn’t loaded by default and we’re including
  ;; org faces in the default value.
  ;;
  ;; Another aspect of symbol is that it allows nil value.  It’s convenient for
  ;; us in the Remapping part but inconvenient in the Target part.
  ;;
  ;; (face ...) would be better (and we’d use choice between face and (const
  ;; nil) in the Remapping) but for now sticking with the former.
  :type '(repeat (cons :tag "Remapping specification"
                       (symbol :tag "Target face")
                       (cons :tag "Remapping faces"
                             (symbol :tag "Dimmed     ")
                             (symbol :tag "Highlighted"))))
  :group 'auto-dim-other-buffers
  :set (lambda (symbol value)
         (set-default symbol value)
         (when auto-dim-other-buffers-mode
           (adob--remap-cycle-all t))))


(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
