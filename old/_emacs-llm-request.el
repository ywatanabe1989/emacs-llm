;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-21 03:24:01>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-request.el

(defun el-request-reload
    ()
  "Reload request.el library after clearing its state."
  (interactive)
  (when
      (file-exists-p
       (request--curl-cookie-jar))
    (delete-file
     (request--curl-cookie-jar)))
  (clrhash request--curl-capabilities-cache)
  (dolist
      (buf
       (buffer-list))
    (when
        (string-match-p " \\*request"
                        (buffer-name buf))
      (kill-buffer buf)))
  (when
      (get-buffer request-log-buffer-name)
    (kill-buffer request-log-buffer-name))
  (unload-feature 'request t)
  (require 'request))

(defun el-request-initialize
    ()
  "Initialize or reset request.el states."
  (unless
      (file-directory-p request-storage-directory)
    (make-directory request-storage-directory t)))

(defun el-request-reset-states
    ()
  "Reset all states of request.el to their initial values.
This includes clearing cookie jar, response buffers, and caches."
  (interactive)
  (when
      (file-exists-p
       (request--curl-cookie-jar))
    (delete-file
     (request--curl-cookie-jar)))
  (clrhash request--curl-capabilities-cache)
  (dolist
      (buf
       (buffer-list))
    (when
        (string-match-p " \\*request"
                        (buffer-name buf))
      (kill-buffer buf)))
  (when
      (get-buffer request-log-buffer-name)
    (with-current-buffer request-log-buffer-name
      (let
          ((inhibit-read-only t))
        (erase-buffer))))
  (el-request-initialize))

(el-request-initialize)

(provide 'el-request)

(when
    (not load-file-name)
  (message "el-request.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))