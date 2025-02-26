;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:58:39>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-dired.el

(defun --el-dired-get-contents
    (&optional include-hidden-p)
  "Get contents of marked files recursively, handling only safe files. If no files specified, just call ordinal el-on-region"
  (interactive)
  (let*
      ((marked-files
        (dired-get-marked-files nil nil
                                (lambda
                                  (f)
                                  (dired-file-marker f))))
       (size-limit
        (* 1024 1024))
       (contents ""))
    (if
        (null marked-files)
        (read-string "Enter prompt: " "")
      (cl-labels
          ((process-file
             (file)
             (cond
              ((file-directory-p file)
               (let
                   ((dir-files
                     (directory-files file t)))
                 ;; Filter the directory contents using our filter function
                 (setq dir-files
                       (el-filter-files dir-files
                                        el-whitelist-extensions
                                        el-whitelist-expressions
                                        el-blacklist-extensions
                                        el-blacklist-expressions
                                        include-hidden-p))
                 (dolist
                     (f dir-files)
                   (process-file f))))
              ((and
                (file-regular-p file)
                (<
                 (file-attribute-size
                  (file-attributes file))
                 size-limit))
               ;; Check if the file passes our filters
               (when
                   (car
                    (el-filter-files
                     (list file)
                     el-whitelist-extensions
                     el-whitelist-expressions
                     el-blacklist-extensions
                     el-blacklist-expressions
                     include-hidden-p))
                 (setq contents
                       (concat contents
                               (format "\n\n;;; ----- %s -----\n\n" file)
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string)))))))))
        (dolist
            (file marked-files)
          (process-file file)))
      contents)))

(provide 'emacs-llm-dired)

(when
    (not load-file-name)
  (message "emacs-llm-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))