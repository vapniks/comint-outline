# comint-outline
outline-minor-mode for comint buffers for hiding/showing output blocks

This package allows you to use `outline-minor-mode` for hiding/showing output
blocks in `comint-mode` buffers, e.g. ESS, shell, python buffers.
The function `comint-outline-start` should be called from a hook function for the
`comint-mode` buffer, e.g:
```
(add-hook 'py-ipython-shell-mode-hook
	  (lambda nil (comint-outline-start "In \\[[0-9]+\\]: .*")))
```
If you don't supply any arguments to `comint-outline-start` it should still work
in most comint buffers as it uses `comint-prompt-regexp` as the default value for 
`outline-regexp`. However, if you find that it treats some output lines as headers, 
you should specify the value of `outline-regexp` in the first argument to `comint-outline-start`.

The option `comint-outline-override-keys` contains a list of keys to override default
bindings in `outline-minor-mode-map`, and you can add more as arguments to `comint-outline-start`.

WARNING: this package overwrites `outline-on-heading-p` which is defined in outline.el.
         This shouldn't cause any problems since it only makes a minor change to allow it to
         detect outline headings in text fields such as comint prompts. 
