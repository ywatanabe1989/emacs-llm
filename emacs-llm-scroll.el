;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:30:38>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-scroll.el

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