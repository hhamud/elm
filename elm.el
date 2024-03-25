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

(defconst elm--claude-key "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")

(defvar elm--header
    `(("x-api-key" . ,elm--claude-key)
      ("anthropic-version" . "2023-06-01")
      ("Content-Type" . "application/json")))

(defconst elm--url "https://api.anthropic.com/v1/messages")

(defun elm--construct-content (content)
  "Contruct the CONTENT to send to claude."
  `(("model" . "claude-3-opus-20240229")
    ("max_tokens" . 1024)
    ("messages" . ((("role" . "user")
                   ("content" . ,content))))))

(defun elm-send-request (data)
  "Send the DATA request to CLAUDE."
  (request elm--url
    :type "POST"
    :headers elm--header
    :data (json-encode (elm--construct-content data))
    :parser 'json-read
    :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* (
                        (resp-text (cdr (assoc 'content data)))
                        (final-resp (cdr (assoc 'text (aref resp-text 0)))))
                    (message "Response :%s" final-resp))))))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (let* ((error-data (request-response-data response))
                     (error-type (cdr (assoc 'type (cdr (assoc 'error error-data)))))
                     (error-message (cdr (assoc 'message (cdr (assoc 'error error-data))))))
                (message "Error: %s -%s" error-type error-message))))




(elm-send-request "testing")
(provide 'elm)
