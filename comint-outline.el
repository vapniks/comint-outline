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
;; blocks in `comint-mode' buffers, e.g. ESS, shell, python, and SQL buffers.
;; After adding `comint-outline-start' to `comint-mode-hook' (see below), when `comint-mode'
;; is started the keys defined in `comint-outline-override-keys' will be bound,
;; and `outline-regexp' will be set according to the values in `comint-outline-regexp'.
;; Both `comint-outline-override-keys' and `comint-outline-regexp' can be customized.
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
;; (add-hook 'comint-mode-hook 'comint-outline-start)

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
  "Alist of '(KEY . CMD) cons cells defining keybindings to override the ones in `outline-minor-mode-map'
when `comint-outline-start' is called."
  :group 'comint
  :type '(alist :key-type (string :tag "Keybinding")
		:value-type (function :tag "Command")))

(defcustom comint-outline-regexp
  '((py-ipython-shell-mode . "In \\[[0-9]+\\]: .*")
    (py-python-shell-mode . ">>> .*")
    (inferior-ess-mode ((equal ess-dialect "R")
			. "> .*")
		       ((equal ess-dialect "stata")
			. "\\. .*"))
    (shell-mode . shell-prompt-pattern)
    (sql-interactive-mode ((memq sql-dialect '(sqlite sqlcipher)) . "sqlite> .*")
			  ((eq sql-dialect 'postgres) . "\\w*\\(=\\|[-(]\\)[#>] ")))
  "Alist of regexp's to be used for `outline-regexp' in different comint modes.
The car of each element is the `major-mode' symbol for the comint buffer,
and the cdr can be either a regexp, a function of one argument (the buffer name), 
a variable containing a regexp, or a list of (SEXP . REGEXP/VAR) pairs.
In the latter case each SEXP will be evalled in the comint buffer until one of
them returns non-nil, in which case the corresponding regexp or variable will be used.

For buffers not matching any of the entries in this list, `comint-prompt-regexp' will be used."
  :comint 'group
  :type '(alist :key-type (symbol :tag "Major mode")
		:value-type (choice regexp
				    (function :tag "Function")
				    (symbol :tag "Variable")
				    (repeat
				     (cons (sexp :tag "Predicate sexp")
					   (choice regexp
						   (symbol :tag "Variable")))))))

;;;###autoload
(cl-defun comint-outline-start (&optional regexp levelfn &rest pairs)
  "Enable `outline-minor-mode-map' in the current buffer, and override some keys.
By default `outline-regexp' is set using `comint-outline-regexp' or `comint-prompt-regexp'
 (with any initial \"^\" removed), and `outline-level' is set to (lambda nil 1). 
You can change this with the REGEXP and LEVELFN arguments respectively, or interactively.
The arguments PAIRS is a list of cons cells of the form '(KEY . CMD),
defining keys in `outline-minor-mode-map' to be overridden. This can also be set interactively.
The keybindings in `comint-outline-override-keys' will always be used.
Also `outshine-hook-function' will be called.
e.g: (comint-outline-start \">>> \" (lambda nil 1)
			      '(\"<M-up>\" . outline-previous-visible-heading)
			      '(\"<M-down>\" . comint-next-prompt))

Note: for some buffers `comint-previous-prompt' &/or `comint-next-prompt' dont work well, 
you could try `outline-previous-visible-heading' & `outline-next-visible-heading' instead."
  (interactive (list (read-regexp "Regexp to match outline headings: "
				  (or (let ((val (cdr (assoc major-mode comint-outline-regexp))))
					(cond ((stringp val) val)
					      ((functionp val) (funcall val (buffer-name)))
					      ((symbolp val) (eval val))
					      ((listp val)
					       (cdr (cl-assoc-if (lambda (v) (eval v)) val)))))
				      (substring comint-prompt-regexp
						 ;; remove initial "^" if necessary
						 (if (eq (aref comint-prompt-regexp 0) ?^)
						     1
						   0))))
		     (read-from-minibuffer "Function to determine outline level (default = level 1 for all headers): "
					   nil nil t nil "(lambda nil 1)")
		     (let (pairs)
		       (condition-case err
			   (while t
			     (let (key cmd)
			       (setq key (read-key-sequence "Key sequence (C-g to finish): " nil nil nil t))
			       (if (member key '("" "")) (signal 'args-out-of-range nil))
			       (setq cmd (read-command "Command: "))
			       (push (cons (key-description key) cmd) pairs)))
			 (args-out-of-range pairs)
			 (error (signal (car err) (cdr err)))))))
  (if (called-interactively-p 'any) (setq pairs (car pairs)))
  (outline-minor-mode 1)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map outline-minor-mode-map)
    (cl-loop for (key . func) in comint-outline-override-keys
	     do (define-key map (kbd key) func))
    (cl-loop for (key . func) in pairs
	     do (define-key map (kbd key) func))
    (push (cons 'outline-minor-mode map) minor-mode-overriding-map-alist))
  (setq outline-regexp (or regexp
			   (let ((val (cdr (assoc major-mode comint-outline-regexp))))
			     (cond ((stringp val) val)
				   ((functionp val) (funcall val (buffer-name)))
				   ((symbolp val) (eval val))
				   ((listp val)
				    (cdr (cl-assoc-if (lambda (v) (eval v)) val)))))
			   (substring comint-prompt-regexp
				      ;; remove initial "^" if necessary
				      (if (eq (aref comint-prompt-regexp 0) ?^)
					  1
					0)))
	outline-level (or levelfn (lambda nil 1))))

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
