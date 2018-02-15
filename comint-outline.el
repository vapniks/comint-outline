;;; comint-outline.el --- Hide/show output blocks in comint buffers using outline-minor-mode

;; Filename: comint-outline.el
;; Description: Hide/show output blocks in comint buffers using outline-minor-mode
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2018, Joe Bloggs, all rites reversed.
;; Created: 2018-02-15 13:27:18
;; Version: 0.1
;; Last-Updated: 2018-02-15 13:27:18
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/comint-outline
;; Keywords: convenience 
;; Compatibility: GNU Emacs 25.2.1
;; Package-Requires: ((outline "20000101"))
;;
;; Features that might be required by this library:
;;
;; outline
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; This package allows you to use `outline-minor-mode' for hiding/showing output
;; blocks in `comint-mode' buffers, e.g. ESS, shell, python buffers.
;; The function `comint-outline-start' should be called from a hook function for the
;; `comint-mode' buffer, e.g:
;; (add-hook 'py-ipython-shell-mode-hook
;; 	  (lambda nil (comint-outline-start "In \\[[0-9]+\\]: .*")))
;;
;; If you don't supply any arguments to `comint-outline-start' it should still work
;; in most comint buffers as it uses `comint-prompt-regexp' as the default value for 
;; `outline-regexp`. However, if you find that it treats some output lines as headers, 
;; you should specify the value of `outline-regexp' in the first argument to `comint-outline-start'.
;;
;; The option `comint-outline-override-keys' contains a list of keys to override default
;; bindings in `outline-minor-mode-map', and you can add more as arguments to `comint-outline-start'.
;;
;; WARNING: this package overwrites `outline-on-heading-p' which is defined in outline.el.
;;          This shouldn't cause any problems since it only makes a minor change to allow it to
;;          detect outline headings in text fields such as comint prompts. 
;;;;;;;;

;;; Installation:
;;
;; Put comint-outline.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'comint-outline)

;;; History:

;;; Require
(require 'outline)

;;; Code:

;;;###autoload
(defcustom comint-outline-override-keys
  '(("<M-up>" . comint-previous-prompt)
    ("<M-down>" . comint-next-prompt)
    ("<M-left>" . outline-hide-entry)
    ("<M-right>" . outline-show-entry))
  "List of '(KEY . CMD) cons cells defining keybindings to override the ones in `outline-minor-mode-map'
when `comint-outline-start' is called."
  :type '(alist :key-type (string :tag "Keybinding")
		:value-type (function :tag "Command")))

;;;###autoload
(cl-defun comint-outline-start (&optional (regexp comint-prompt-regexp)
					  (levelfn (lambda nil 1)) &rest pairs)
  "Enable `outline-minor-mode-map' in the current buffer, and override some keys.
Set `outline-regexp' to REGEXP, and variable `outline-level' to LEVELFN.
By default `outline-regexp' is set to `comint-prompt-regexp', and `outline-level'
is set to (lambda nil 1). 
The arguments PAIRS is a list of cons cells of the form '(KEY . CMD),
defining keys in `outline-minor-mode-map' to be overridden.
The keybindings in `comint-outline-override-keys' will always be used.
Also `outshine-hook-function' will be called.
e.g: (comint-outline-start \">>> \" (lambda nil 1)
			      '(\"<M-up>\" . comint-previous-prompt)
			      '(\"<M-down>\" . comint-next-prompt))"
  (outline-minor-mode 1)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map outline-minor-mode-map)
    (cl-loop for (key . func) in comint-outline-override-keys
	     do (define-key map (kbd key) func))
    (cl-loop for (key . func) in pairs
	     do (define-key map (kbd key) func))
    (push (cons 'outline-minor-mode map) minor-mode-overriding-map-alist))
  (setq outline-regexp regexp
	outline-level levelfn))

;; altered version of `outline-on-heading-p' that works even if heading is in a different field
;; (e.g. in some comint buffers), so that you can hide/show their contents.
(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (forward-line 0)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (looking-at outline-regexp))))



(provide 'comint-outline)

;; (org-readme-sync)
;; (magit-push)

;;; comint-outline.el ends here
