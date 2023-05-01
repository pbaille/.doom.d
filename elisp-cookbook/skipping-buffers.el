;;; elisp-cookbook/skipping-buffers.el -*- lexical-binding: t; -*-

(defcustom pb/buffer-skip-regexp
  (rx bos (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
                  "*Messages*" "*package*" "*Warnings*"
                  "*Async-native-compile-log*")
              (seq "magit-diff" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "magit-revision" (zero-or-more anything))
              (seq "magit-stash" (zero-or-more anything))
              (seq ":~/" (zero-or-more anything)))
      eos)
  "Regular expression matching buffers ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp)

(defun pb/buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `pb/buffer-skip-regexp'."
  (string-match-p pb/buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'pb/buffer-skip-p)
