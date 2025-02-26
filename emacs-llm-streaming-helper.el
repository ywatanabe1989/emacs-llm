;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 13:01:41>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-streaming-helper.el

;;;; Generic Streaming Helper Functions

(defun el--process-chunk
    (proc chunk parse-func)
  "Process CHUNK from PROC using PARSE-FUNC to extract content."
  (message "Raw chunk: %s"
           (substring chunk 0
                      (min 100
                           (length chunk))))

  ;; Handle partial data from previous chunks
  (let*
      ((partial
        (or
         (process-get proc 'partial-data)
         ""))
       (combined
        (concat partial chunk))
       (lines
        (split-string combined "\n"))
       (incomplete
        (if
            (string-suffix-p "\n" combined)
            ""
          (car
           (last lines)))))

    (unless
        (string= incomplete "")
      (setq lines
            (butlast lines)))
    (process-put proc 'partial-data incomplete)

    ;; Process each complete line
    (dolist
        (line lines)
      (when
          (string-prefix-p "data: " line)
        (let*
            ((json-data
              (substring line 6))
             (text
              (when
                  (not
                   (string= json-data "[DONE]"))
                (funcall parse-func json-data))))

          (when text
            (message "Got text: %s"
                     (substring text 0
                                (min 30
                                     (length text))))
            (with-current-buffer
                (process-get proc 'target-buffer)
              (save-excursion
                (goto-char
                 (point-max))
                (insert text))))

          ;; Accumulate content regardless of display
          (when text
            (process-put proc 'content
                         (concat
                          (or
                           (process-get proc 'content)
                           "")
                          text))))))))

(provide 'emacs-llm-streaming-helper)

(when
    (not load-file-name)
  (message "emacs-llm-streaming-helper.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))