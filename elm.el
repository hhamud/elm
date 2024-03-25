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
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'request)
(require 'auth-source)


(defvar elm--claude-key nil "API key for Claude API.")

(defun elm--get-api-key ()
  "Retrieve the API key for Claude API.
The key is first looked up in the auth-sources, then the user is prompted if not found."
  (unless elm--claude-key
      (let ((secret (auth-source-user-and-password "api.anthropic.com")))
        (if secret
            (setq elm--claude-key (auth-info-password secret))
          (setq elm--claude-key
                (read-passwd "Enter your Claude API key: ")))
        elm--claude-key)))

(defun elm--save-api-key ()
  "Save the API key for Claude API to the auth-sources."
  (when elm--claude-key
    (auth-source-forget+ :host "api.anthropic.com")
    (let ((spec `(:host "api.anthropic.com" :user "claude-api"))
          (found `(:secret ,elm--claude-key)))
      (auth-source-remember spec found))))


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


(defun elm--parse-response (input output)
  "Parse the INPUT and OUTPUT into an org compatible format."
  (with-current-buffer (get-buffer-create "*elm*")
    (goto-char (point-max))
    (newline)
    (newline)
    (org-mode)
    (insert (format "* %s" input))
    (newline)
    (newline)
    (insert (elm--parse-code-blocks output))
    (switch-to-buffer-other-window "*elm*")))


(defun elm--parse-code-blocks (content)
  "Convert any code CONTENT from markdown to org-code-blocks."
  (shell-command-to-string (format "pandoc -f markdown -t org <(echo %s)" (shell-quote-argument content))))


(defun elm--process-request (input)
  "Send the INPUT request to CLAUDE."
  (request elm--url
    :type "POST"
    :headers elm--header
    :data (json-encode (elm--construct-content input))
    :parser 'json-read
    :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((resp-text (cdr (assoc 'content data)))
                        (final-resp (cdr (assoc 'text (aref resp-text 0)))))
                    (elm--parse-response input final-resp))))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (let* ((error-data (request-response-data response))
                     (error-type (cdr (assoc 'type (cdr (assoc 'error error-data)))))
                     (error-message (cdr (assoc 'message (cdr (assoc 'error error-data))))))
                (message "Error: %s -%s" error-type error-message))))))


(defun elm-rewrite (prompt start end)
  "Rewrite specific using the PROMPT and area from START to END requested."
  (interactive "sPrompt: \nr")
  (let ((input (buffer-substring-no-properties start end)))
    (elm--process-request (concat prompt " " input))))

(defun elm-send-request (input)
  "Send the INPUT request to CLAUDE."
  (interactive "sPrompt: ")
  (elm--process-request input))

(provide 'elm)
;;; elm.el ends here
