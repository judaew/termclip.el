;;; termclip.el --- Terminal Clipboard Integration -*- lexical-binding: t -*-

;; Author: Vadym-Valdis Yudaiev
;; Version 0.1
;; Keywords: clipboard, terminal, Wayland
;; Homepage: https://github.com/judaew/termclip.el
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; termclip.el provides seamless integration between Emacs and
;; the system clipboard when running Emacs in terminal mode.

;;; Code:

(defun termclip/kill-region (begin end)
  "Cut region between BEGIN and END to system clipboard."
  (interactive "r")
  (call-process-region begin end "wl-copy")
  (delete-region begin end))

(defun termclip/kill-ring-save (begin end)
  "Copy region between BEGIN and END to system clipboard."
  (interactive "r")
  (call-process-region begin end "wl-copy")
  (deactivate-mark))

(defun termclip/yank ()
  "Paste contents from system clipboard."
  (interactive)
  (let ((clipboard-text (shell-command-to-string "wl-paste --no-newline")))
    (insert clipboard-text)))

(defvar termclip-mode-map (make-sparse-keymap)
  "Keymap for terminal clipboard mode.")

(defun termclip/enable-bindings ()
  "Enable terminal clipboard keybindings."
  (define-key termclip-mode-map (kbd "C-w") #'termclip/kill-region)
  (define-key termclip-mode-map (kbd "M-w") #'termclip/kill-ring-save)
  (define-key termclip-mode-map (kbd "C-y") #'termclip/yank))

(defun termclip/disable-bindings ()
  "Disable terminal clipboard keybindings."
  (define-key termclip-mode-map (kbd "C-w") #'kill-region)
  (define-key termclip-mode-map (kbd "M-w") #'kill-ring-save)
  (define-key termclip-mode-map (kbd "C-y") #'yank))

;;;###autoload
(define-minor-mode termclip-mode
  "Minor mode for terminal clipboard integration."
  :init-value nil
  :global t
  :lighter "termclip"
  :keymap termclip-mode-map
  (if termclip-mode
      (termclip/enable-bindings)
    (termclip/disable-bindings)))

;;;###autoload
(defun termclip/setup-if-needed (frame)
  "Configure termclip-mode for newly created FRAME."
  (with-selected-frame frame
    (if (not (display-graphic-p))
	(termclip-mode 1)
      (termclip-mode 0))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'termclip/setup-if-needed)
  (unless (display-graphic-p)
    termclip-mode 1))

(provide 'termclip)

;;; termclip.el ends here
