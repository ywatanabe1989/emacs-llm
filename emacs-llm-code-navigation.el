;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:34:32>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-code-navigation.el

(require 'emacs-llm-scroll)

(defconst --el-code-block-start-delimiter
  "^\\s-*```\\s-*\\([a-zA-Z0-9_+-]+\\)$"
  "Regex for code block start delimiter.")

(defconst --el-code-block-end-delimiter
  "^\\s-*```\\s-*$"
  "Regex for code block end delimiter.")

(defun --el-navigate-code-blocks
    (direction)
  "Navigate to code blocks in DIRECTION (either 'next or 'previous).
Returns the region of the block found or nil."
  (let
      (block-start block-end found-block)
    (save-excursion
      (cond
       ((eq direction 'next)
        (end-of-line)
        (when
            (looking-at-p --el-code-block-end-delimiter)
          (forward-line 1))
        (when
            (re-search-forward --el-code-block-start-delimiter nil t)
          (setq block-start
                (point))
          (forward-line -1)
          (when
              (re-search-forward --el-code-block-end-delimiter nil t)
            (setq block-end
                  (match-beginning 0))
            (setq found-block t))))

       ((eq direction 'previous)
        (beginning-of-line)
        (when
            (looking-at-p --el-code-block-start-delimiter)
          (re-search-backward --el-code-block-start-delimiter nil t))
        (when
            (re-search-backward --el-code-block-start-delimiter nil t)
          (forward-line 1)
          (setq block-start
                (point))
          (when
              (re-search-forward --el-code-block-end-delimiter nil t)
            (setq block-end
                  (match-beginning 0))
            (setq found-block t))))))

    (when found-block
      (goto-char block-start)
      (set-mark block-end)
      (activate-mark)
      (kill-ring-save block-start block-end)
      (pulse-momentary-highlight-region block-start block-end)
      (message "Code block copied.")
      (cons block-start block-end))))

(defun --el-copy-current-code-block
    ()
  "Copy the code block at point without changing position."
  (interactive)
  (save-excursion
    (let
        ((start nil)
         (end nil))
      ;; Find the enclosing code block
      (when
          (or
           (looking-at --el-code-block-start-delimiter)
           (re-search-backward --el-code-block-start-delimiter nil t))
        (forward-line 1)
        (setq start
              (point))
        (when
            (re-search-forward --el-code-block-end-delimiter nil t)
          (setq end
                (match-beginning 0))
          (kill-ring-save start end)
          (pulse-momentary-highlight-region start end)
          (message "Code block copied.")
          t)))))

(defun --el-next-code-block
    ()
  "Navigate to the next code block, copy it, and pulse highlight it."
  (interactive)
  (with-current-buffer --el-buffer-name
    (--el-navigate-code-blocks 'next)))

(defun --el-previous-code-block
    ()
  "Navigate to the previous code block, copy it, and pulse highlight it."
  (interactive)
  (with-current-buffer --el-buffer-name
    (--el-navigate-code-blocks 'previous)))

(provide 'emacs-llm-code-navigation)

(when
    (not load-file-name)
  (message "emacs-llm-code-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))