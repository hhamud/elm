;;; elm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Hamza Hamud
;;
;; Author: Hamza Hamud <self@hamzahamud.com>
;; Maintainer: Hamza Hamud <self@hamzahamud.com>
;; Created: March 24, 2024
;; Modified: March 24, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/hhamud/elm
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
;;;
(require 'request)
(require 'transient)


(defvar elm--claude-key nil "API key for Claude API.")
(defvar elm--progress-reporter nil "Progress reporter for ELM.")
(defconst elm--claude-models '(("haiku" . "claude-3-haiku-20240307")
                               ("sonnet" . "claude-3-sonnet-20240229")
                               ("opus" . "claude-3-opus-20240229")) "List of Claude Models.")

(defvar elm--model "claude-3-haiku-20240307" "Model to be used.")

(defun elm--select-model ()
  "Prompt the user to select a Claude model from the list."
  (interactive)
  (let ((model-options elm--claude-models))
    (let ((selected-model (assoc (completing-read "Select Claude Model: " model-options nil t) model-options)))
      (setq elm--model (cdr selected-model)))))


(defun elm--update-key (key)
  "Update the CLAUDE API KEY."
  (interactive "sKey: ")
  (let ((env-file (expand-file-name "~/.elm/.env"))
        (regexp (format "^%s=.*$" "CLAUDE")))
    (with-temp-file env-file
      (insert-file-contents env-file)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (replace-match (format "CLAUDE=%s" key))
        (goto-char (point-max))
        (insert (format "CLAUDE=%s
" key))))
    (message "API key updated successfully.")))

(transient-define-prefix elm-transient()
        ["Arguments" ("m" "Claude Model" elm--select-model) ("s" "save claude key" elm--update-key)])

(defun elm--progress-reporter (operation)
  "Progress reporter for ELM.
OPERATION should be \\='start, or \\='done."
  (pcase operation
    ('start (setq elm--progress-reporter (make-progress-reporter "ELM: Waiting for response from servers..." nil nil)))
    ('done (progress-reporter-done elm--progress-reporter))))

(defun elm--get-api-key ()
  "Retrieve the API key for Claude API from ENV."
  (let ((env-file (expand-file-name "~/.elm/.env"))
        (regexp (format "^%s=\\(.*\\)$" "CLAUDE")))
    (if (file-exists-p env-file)
        (with-temp-buffer
          (insert-file-contents env-file)
          (goto-char (point-min))
          (if (re-search-forward regexp nil t)
              (setq elm--claude-key (match-string 1))
            (message (format "%s key not found in .env file" "CLAUDE"))))
      (message "ENV file (.env) not found"))))

(defun elm--set-api-key ()
  "Set the api key for claude."
  (unless elm--claude-key
    (elm--get-api-key)
    elm--claude-key))

(defvar elm--header
  (let* ((headers `(("x-api-key" . ,(elm--set-api-key))
                    ("anthropic-version" . "2023-06-01")
                    ("Content-Type" . "application/json"))))
    headers))

(defconst elm--url "https://api.anthropic.com/v1/messages")

(defun elm--construct-content (content)
  "Construct the CONTENT to send to claude."
  `(("model" . ,elm--model)
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
  (elm--progress-reporter 'start)
  (request elm--url
    :type "POST"
    :headers elm--header
    :data (json-encode (elm--construct-content input))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (elm--progress-reporter 'done)
                (let* ((resp-text (cdr (assoc 'content data)))
                       (final-resp (cdr (assoc 'text (aref resp-text 0)))))
                  (elm--parse-response input final-resp))))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (elm--progress-reporter 'done)
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

(defun elm-menu()
  "Select the menu for elm."
  (interactive)
  (elm-transient))

(provide 'elm)
;;; elm.el ends here
