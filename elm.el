;;; elm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Hamza Hamud
;;
;; Author: Hamza Hamud <self@hamzahamud.com>
;; Maintainer: Hamza Hamud <self@hamzahamud.com>
;; Created: March 24, 2024
;; Modified: March 24, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/user/elm
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'request)

(defconst elm--claude-key "")

(defvar elm--header
    `(("x-api-key" . ,elm--claude-key)
      ("anthropic-version" . "2023-06-01")
      ("content-type" . "application/json")))

(defconst elm--url "https://api.anthropic.com/v1/messages")

(defun elm--construct-content (content)
  "Contruct the content to send to claude."
  `(("model" . "claude-3-opus-20240229")
    ("max_tokens" . "1024")
    ("message" . (("role" . "user")
                   ("content" . ,content)))))

(defun elm-send-request (data)
  "send the DATA request to CLAUDE."
  (request elm--url
    :type "POST"
    :header elm--header
    :data (json-encode (elm--construct-content data))
    :parser 'json-read
    :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "Response :%S" data)))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (let* ((error-data (request-response-data response))
                     (error-type (cdr (assoc 'type (cdr (assoc 'error error-data)))))
                     (error-message (cdr (assoc 'message (cdr (assoc 'error error-data))))))
                (message "Error: %s -%s" error-type error-message))))))


(provide 'elm)
;;; elm.el ends here
