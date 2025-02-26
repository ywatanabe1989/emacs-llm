;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:31:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-scroll.el

(defun --el-scroll-to-bottom
    (&optional buffer-name)
  "Scrolls to the end of the LLM buffer.
Optional BUFFER-NAME can be provided, defaults to \"*Emacs-LLM*\"."
  (interactive)
  (let*
      ((buffer-name
        (or buffer-name --el-buffer-name))
       (buffer
        (get-buffer buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (goto-char
         (point-max))
        (let
            ((window
              (get-buffer-window buffer 0)))
          (when window
            (with-selected-window window
              (goto-char
               (point-max))
              (recenter -1))))))))

(defun --el-scroll-to-point
    (position)
  "Scroll to POSITION in LLM buffer."
  (let
      ((buffer
        (get-buffer-create --el-buffer-name)))
    (with-current-buffer buffer
      (goto-char position)
      (let
          ((window
            (get-buffer-window buffer 0)))
        (when window
          (with-selected-window window
            (recenter)))))))

(defun --el-scroll-to-bottom
    ()
  "Scroll to the end of the LLM buffer."
  (interactive)
  (let
      ((buffer
        (get-buffer-create --el-buffer-name)))
    (with-current-buffer buffer
      (goto-char
       (point-max))
      (let
          ((window
            (get-buffer-window buffer 0)))
        (when window
          (with-selected-window window
            (recenter -1)))))))

(defun --el-scroll-to-last-separator
    ()
  "Scroll to the most recent separator in the LLM buffer."
  (interactive)
  (let
      ((buffer
        (get-buffer-create --el-buffer-name)))
    (with-current-buffer buffer
      (goto-char
       (point-max))
      (when
          (search-backward --el-separator nil t)
        (let
            ((pos
              (point)))
          (let
              ((window
                (get-buffer-window buffer 0)))
            (when window
              (with-selected-window window
                (set-window-point window pos)
                (recenter 0)))))))))

(provide 'emacs-llm-scroll)

(when
    (not load-file-name)
  (message "emacs-llm-scroll.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))