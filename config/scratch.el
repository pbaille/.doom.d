;;; config/scratch.el -*- lexical-binding: t; -*-

(pb_comment
 :scratch-temp

 '(load "~/.doom.d/parts/color-picker/color-picker.el")

 ;; TOODO

 '(use-package! copilot
    ;; :hook (prog-mode . copilot-mode)
    :bind (("C-TAB" . 'copilot-accept-completion-by-word)
           ("<backtab>" . 'copilot-accept-completion-by-word)
           :map copilot-completion-map
           ("<tab>" . 'copilot-accept-completion)
           ("TAB" . 'copilot-accept-completion)))

 ;; a simple repl that just echo the given prompt until receiving exit
 (use-package! gpt
   :config
   (setq gpt-openai-key "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")
   (setq gpt-openai-engine "text-davinci-003")
   :bind (:map evil-normal-state-map ("g p" . gpt-dwim)))

 ;; setup G binding for gpt/dwim
 '(use-package centaur-tabs
    :ensure t
    :config
    (centaur-tabs-mode t)
    (setq centaur-tabs-style "wave")
    (setq centaur-tabs-set-bar 'left)
    (setq centaur-tabs-gray-out-icons 'buffer)
    (setq centaur-tabs-set-icons t)
    (setq centaur-tabs-close-button "x")
    (setq centaur-tabs-set-modified-marker t)
    (setq centaur-tabs-modified-marker "*")
    :bind
    ("C-<left>" . centaur-tabs-backward)
    ("C-<right>" . centaur-tabs-forward)))

(pb_comment
 (length (window-prev-buffers))
 (length (window-next-buffers)))

(pb_comment

 ;; Setting window parameters
 (set-window-parameter nil 'my-param "my-value")

 ;; Retrieving window parameters
 (let ((params (window-parameters))
       (value (window-parameter nil 'my-param)))
   (message "All window parameters: %S" params)
   (message "Value of 'my-param': %S" value)))
