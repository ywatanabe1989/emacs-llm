;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 21:23:34>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-display.el

;; (defun --el-display-buffer
;;     (prompt provider engine &optional template-name)
;;   "Prepare the LLM buffer for displaying a response.
;; PROMPT is the user input.
;; PROVIDER is the name of the LLM provider.
;; ENGINE is the engine name.
;; Optional TEMPLATE-NAME is the name of the template used, if any."
;;   (let
;;       ((buffer-name
;;         (get-buffer-create --el-buffer-name)))
;;     (with-current-buffer buffer-name
;;       (unless
;;           (derived-mode-p 'markdown-mode)
;;         (markdown-mode))
;;       (goto-char
;;        (point-max))
;;       (unless
;;           (=
;;            (point-min)
;;            (point-max))
;;         (insert "\n\n"))
;;       (insert --el-separator)
;;       (insert "\n\n")
;;       (insert "| USER")
;;       (when template-name
;;         (insert
;;          (format " [Template: %s]" template-name)))
;;       (insert "\n\n")
;;       (insert "| ")
;;       (insert prompt)
;;       (insert "\n\n")
;;       (insert --el-separator)
;;       (insert "\n\n")
;;       (insert "| ")
;;       ;; Place spinner at the end of engine name for better visual effect
;;       (insert
;;        (upcase engine))
;;       ;; Create spinner marker right after engine name
;;       (setq --el-spinner-marker
;;             (point-marker))
;;       (set-marker-insertion-type --el-spinner-marker t)
;;       (insert "\n\n")
;;       ;; Make sure to scroll to show the latest content
;;       (--el-scroll-to-last-separator)
;;       (display-buffer
;;        (current-buffer)))
;;     buffer-name))

;; (defun --el-display-buffer
;;     (prompt provider engine &optional template-name)
;;   "Prepare the LLM buffer for displaying a response.
;; PROMPT is the user input.
;; PROVIDER is the name of the LLM provider.
;; ENGINE is the engine name.
;; Optional TEMPLATE-NAME is the name of the template used, if any."
;;   (let
;;       ((buffer-name
;;         (get-buffer-create --el-buffer-name)))
;;     (with-current-buffer buffer-name
;;       (unless
;;           (derived-mode-p 'markdown-mode)
;;         (markdown-mode))
;;       (goto-char
;;        (point-max))
;;       (unless
;;           (=
;;            (point-min)
;;            (point-max))
;;         (insert "\n\n"))
;;       (insert --el-separator)
;;       (insert "\n\n")
;;       (insert "| USER")
;;       (when template-name
;;         (insert
;;          (format " [Template: %s]" template-name)))
;;       (insert "\n\n")
;;       (insert "| ")
;;       (insert prompt)
;;       (insert "\n\n")
;;       (insert --el-separator)
;;       (insert "\n\n")
;;       (insert "| ")
;;       ;; Place engine name
;;       (insert
;;        (upcase engine))
;;       ;; Create spinner marker right after engine name
;;       (insert " ")
;;       (emacs-spinner-start)
;;       (sleep-for 0.3)
;;       (insert "\n\n")
;;       ;; Make sure to scroll to show the latest content
;;       (--el-scroll-to-last-separator)
;;       (display-buffer
;;        (current-buffer)))
;;     buffer-name))

(defun --el-display-buffer
    (prompt provider engine &optional template-name)
  "Prepare the LLM buffer for displaying a response.
PROMPT is the user input.
PROVIDER is the name of the LLM provider.
ENGINE is the engine name.
Optional TEMPLATE-NAME is the name of the template used, if any."
  (let
      ((buffer-name
        (get-buffer-create --el-buffer-name)))
    (with-current-buffer buffer-name
      (unless
          (derived-mode-p 'markdown-mode)
        (markdown-mode))
      (goto-char
       (point-max))
      (unless
          (=
           (point-min)
           (point-max))
        (insert "\n\n"))
      (insert --el-separator)
      (insert "\n\n")
      (insert "| USER")
      (when template-name
        (insert
         (format " [Template: %s]" template-name)))
      (insert "\n\n")
      (insert "| ")
      (insert prompt)
      (insert "\n\n")
      (insert --el-separator)
      (insert "\n\n")
      (insert "| ")
      ;; Place engine name
      (insert
       (upcase engine))

      (insert " ")
      ;; Create spinner marker right after engine name
      ;; Start spinner at set position
      (setq spinner-position
            (emacs-spinner-set-position))
      (setq spinner-id
            (emacs-spinner-start spinner-position))
      (insert "\n\n\n")
      (beginning-of-line)
      (goto-char
       (point-max))
      (delete-char -1)
      ;; Make sure to scroll to show the latest content
      (--el-scroll-to-last-separator)
      (display-buffer
       (current-buffer)))
    buffer-name))

;; Error running timer: (end-of-buffer) [117 times]

;; (setq spinner-position (emacs-spinner-set-position⠼
;; (emacs-spinner-start spinner-position)
;; (emacs-spinner-stop spinner-id)
;; (emacs-spinner-stop-all)

(provide 'emacs-llm-display)

(when
    (not load-file-name)
  (message "emacs-llm-display.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))