# setags

Minimalistic ctags tags file parser for GNU Emacs. To use, register it
as an xref backend:
```elisp
(add-hook 'xref-backend-functions #'setags-xref-backend)
```
and execute `M-x setags-visit-tags-table` for each ctags file of
interest.

## Related projects

* **etags**

  Supports only Emacs-style TAGS files. These are unsorted meaning
  binary search cannot be employed, making it unusably slow for
  medium-sized projects such as the Linux kernel.
* **[Citre]**:

  Complex tags frontend with multiple backends of which ctags is one.
  Depends on the external `readtags(1)` program, unlike setags which
  is pure Emacs Lisp and has to load the entire tags file into memory.
* **[fastctags]**

  Uses an expensive first-character partitioning preprocessing step
  instead of simple binary search. Lacks an xref backend, but has a
  `completion-at-point-functions` backend.

[Citre]: https://github.com/universal-ctags/citre
[fastctags]: https://github.com/redguardtoo/fastctags
