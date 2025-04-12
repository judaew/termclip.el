;;; termclip.el --- Terminal Clipboard Integration -*- lexical-binding: t -*-

;; Author: Vadym-Valdis Yudaiev
;; Version 0.1
;; Keywords: clipboard, terminal, Wayland, X.Org, macOS
;; Homepage: https://github.com/judaew/termclip.el
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; termclip.el provides seamless integration between Emacs and
;; the system clipboard when running Emacs in terminal mode.

;;; Code:

(defgroup termclip nil
  "Terminal Clipboard Integration."
  :group 'external
  :prefix "termclip-"
  :link '(url-link "https://github.com/judaew/termclip.el"))

(defcustom termclip-clipboard-tool 'wayland
  "The executable tool to be used with termclip for copying/pasting.

Available options:
- \=wayland\= (for Wayland) [default]
- \=xorg-xsel\= (for X.org)
- \=xorg-xclip\= (for X.org)
- \=macos\= (for macOS)"
  :type '(choice
	  (const :tag "wl-clipboard (for Wayland)" wayland)
	  (const :tag "xsel (for X.org)" xorg-xsel)
	  (const :tag "xclip (for X.org)" xorg-xclip)
	  (const :tag "pbcopy/pbpaste (for macOS)" macos))
  :group 'termclip)

(defun termclip--get-command (type)
  "Return the copy/paste (TYPE) command based on `termclip-clipboard-tool'.
TYPE must be either \=copy\= or \=paste\="
  (let ((cmd (pcase (cons termclip-clipboard-tool type)
	       ('(wayland . copy ) "wl-copy")
	       ('(xorg-xsel . copy) "xsel --clipboard -i")
	       ('(xorg-xclip . copy) "xclip -selection clipboard")
	       ('(macos . copy) "pbcopy")
	       ('(wayland . paste) "wl-paste --no-newline")
	       ('(xorg-xsel . paste) "xsel --clipboard -o")
	       ('(xorg-xclip . paste) "xclip -selection clipboard -o")
	       ('(macos . paste) "pbpaste")
	       (_ (error "Unsupported clipboard tool: '%s'-'%s'"
			 termclip-clipboard-tool type)))))
    (let ((bin (car (split-string cmd))))
      (unless (executable-find bin)
	(error "Clipboard tool '%s' not found in PATH" bin)))
    cmd))

(defun termclip--advice-kill-region (orig-fun &rest args)
  "Advice for `kill-region' (as ORIG-FUN with ARGS)."
  (termclip--copy-to-clipboard (car args) (cadr args)))

(defun termclip--advice-kill-ring-save (orig-fun &rest args)
  "Advice for `kill-ring-save' (as ORIG-FUN with ARGS)."
  (apply orig-fun args)
  (termclip--copy-to-clipboard (car args) (cadr args)))

(defun termclip--advice-yank (orig-fun &rest args)
  "Advice for `yank' (as ORIG-FUN with ARGS)."
  (if (not (display-graphic-p))
      (termclip--paste)
    (apply orig-fun args)))

(defun termclip--copy-to-clipboard (begin end)
  "Copy region between BEGIN and END to system clipboard."
  (condition-case err
      (let* ((raw-cmd (termclip--get-command 'copy))
	     (cmd-parts (split-string-and-unquote raw-cmd)))
	
	(apply #'call-process-region begin end
	       (car cmd-parts) nil nil nil
	       (cdr cmd-parts)))
    
    (error (message "Failed to copy to system clipboard: %s"
		    (error-message-string err)))))

(defun termclip--paste ()
  "Paste contents from system clipboard."
  (interactive)
    (condition-case err
      (let* ((raw-cmd (termclip--get-command 'paste))
             (cmd-parts (split-string-and-unquote raw-cmd))
             (clipboard-text (shell-command-to-string (string-join cmd-parts " "))))
        (insert clipboard-text))
      (error (message "Failed to paste from system clipboard: %s"
		      (error-message-string err)))))

(defun termclip--enable-advice ()
  "Enable terminal clipboard advice (\=termclip.el\=)."
  (advice-add 'kill-region :around #'termclip--advice-kill-region)
  (advice-add 'kill-ring-save :around #'termclip--advice-kill-ring-save)
  (advice-add 'kill-yank :around #'termclip--advice-yank))

(defun termclip--disable-advice ()
  "Disable terminal clipboard advice (\=termclip.el\=)."
  (advice-remove 'kill-region #'termclip--advice-kill-region)
  (advice-remove 'kill-ring-save #'termclip--advice-kill-ring-save)
  (advice-remove 'kill-yank #'termclip--advice-yank))

;;;###autoload
(define-minor-mode termclip-mode
  "Minor mode for terminal clipboard Integration."
  :init-value nil
  :global t
  :lighter " tc"
  (if termclip-mode
      (termclip--enable-advice)
    (termclip--disable-advice)))

(defun termclip-setup-if-needed (frame)
  "Configure termclip-mode for newly created FRAME."
  (with-selected-frame frame
    (termclip-mode (if (display-graphic-p) 0 1))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'termclip-setup-if-needed)
  (unless (display-graphic-p)
      (termclip-mode 1))))

(provide 'termclip)

;;; termclip.el ends here
