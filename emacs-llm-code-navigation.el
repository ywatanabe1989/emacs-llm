;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:32:42>
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

(defun --el-copy-with-syntax
    (start end)
  "Copy the region from START to END with syntax properties preserved."
  (interactive "r")
  (let
      ((text
        (buffer-substring start end))
       (lang
        (save-excursion
          (goto-char start)
          (when
              (re-search-backward --el-code-block-start-delimiter nil t)
            (match-string-no-properties 1)))))
    (kill-new text)
    (when lang
      (message "Copied %s code block." lang))
    text))

;; (defun --el-copy-all-code-blocks
;;     ()
;;   "Copy all code blocks in the last LLM output with language info."
;;   (interactive)
;;   (with-current-buffer
;;       (get-buffer --el-buffer-name)
;;     (goto-char
;;      (point-max))
;;     (let
;;         ((blocks nil)
;;          (count 0))
;;       (when
;;           (search-backward --el-separator nil t)
;;         (goto-char
;;          (match-end 0))
;;         (while
;;             (re-search-forward --el-code-block-start-delimiter nil t)
;;           (let
;;               ((lang
;;                 (match-string-no-properties 1)))
;;             (forward-line 1)
;;             (let
;;                 ((start
;;                   (point)))
;;               (when
;;                   (re-search-forward --el-code-block-end-delimiter nil t)
;;                 (let
;;                     ((end
;;                       (match-beginning 0))
;;                      (block-text
;;                       (buffer-substring-no-properties
;;                        (save-excursion
;;                          (forward-line -1)
;;                          (point))
;;                        end)))
;;                   (push
;;                    (cons lang
;;                          (buffer-substring-no-properties start end))
;;                    blocks)
;;                   (setq count
;;                         (1+ count)))))))

;;         (when blocks
;;           (let
;;               ((all-blocks
;;                 (mapconcat
;;                  (lambda
;;                    (block)
;;                    (format "```%s\n%s\n```"
;;                            (car block)
;;                            (cdr block)))
;;                  (nreverse blocks)
;;                  "\n\n")))
;;             (kill-new all-blocks)
;;             (message "Copied %d code blocks with language information." count)))))))

;; (defun --el-quick-menu
;;     ()
;;   "Show a quick menu of available LLM actions."
;;   (interactive)
;;   (let*
;;       ((actions
;;         '(("Run on region/prompt" . el-run)
;;           ("Switch LLM provider" . el-switch)
;;           ("Show history" . el-show-history)
;;           ("Copy last response" . --el-copy-last-response)
;;           ("Copy all code blocks" . --el-copy-all-code-blocks)
;;           ("Navigate to next code block" . --el-next-code-block)
;;           ("Navigate to previous code block" . --el-previous-code-block)
;;           ("Scroll to bottom" . --el-scroll-to-bottom)))
;;        (choice
;;         (completing-read "LLM action: "
;;                          (mapcar #'car actions)
;;                          nil t)))
;;     (when choice
;;       (funcall
;;        (cdr
;;         (assoc choice actions))))))

(provide 'emacs-llm-code-navigation)

(when
    (not load-file-name)
  (message "emacs-llm-code-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))