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
(defconst elm--models '(("haiku" . "claude-3-haiku-20240307")
                               ("sonnet" . "claude-3-sonnet-20240229")
                               ("opus" . "claude-3-opus-20240229")
                               ("LLaMA3 8b" . "llama3-8b-8192")
                               ("LLaMA3 70b" . "llama3-70b-8192")
                               ("Mixtral 8x7b" . "mixtral-8x7b-32768")
                               ("Gemma 7b" . "gemma-7b-it")) "List of Models.")


(defvar elm--model "llama3-70b-8192" "Model to be used.")

(defun elm--select-model ()
  "Prompt the user to select a Claude model from the list."
  (interactive)
  (let ((model-options elm--models))
    (let ((selected-model (assoc (completing-read "Select Model: " model-options nil t) model-options)))
      (setq elm--model (cdr selected-model)))))


(defun elm--update-key (key company)
  "Update COMPANY API KEYs."
  (interactive "sKey: ")
  (let ((env-file (expand-file-name "~/.elm/.env"))
        (regexp (format "^%s=.*$" company)))
    (with-temp-file env-file
      (insert-file-contents env-file)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (replace-match (format "%s=%s" company key))
        (goto-char (point-max))
        (insert (format "%s=%s
" company key))))
    (message "API key updated successfully.")))

(transient-define-prefix elm-transient()
        ["Arguments" ("m" "Model" elm--select-model) ("s" "save API key" elm--update-key)])

(defun elm--progress-reporter (operation)
  "Progress reporter for ELM.
OPERATION should be \\='start, or \\='done."
  (pcase operation
    ('start (setq elm--progress-reporter (make-progress-reporter "ELM: Waiting for response from servers..." nil nil)))
    ('done (progress-reporter-done elm--progress-reporter))))

(defun elm--get-api-key (company)
  "Retrieve the API key for COMPANY API from ENV."
  (let ((env-file (expand-file-name "~/.elm/.env"))
        (regexp (format "^%s=\\(.*\\)$" company)))
    (if (file-exists-p env-file)
        (with-temp-buffer
          (insert-file-contents env-file)
          (goto-char (point-min))
          (if (re-search-forward regexp nil t)
              (setq elm--claude-key (match-string 1))
            (message (format "%s key not found in .env file" company))))
      (message "ENV file (.env) not found"))))

(defun elm--set-api-key ()
  "Set the api key for claude."
  (unless elm--claude-key
    (elm--get-api-key "CLAUDE")
    elm--claude-key))

(defvar elm--claude-header
  (let* ((headers `(("x-api-key" . ,(elm--set-api-key))
                    ("anthropic-version" . "2023-06-01")
                    ("Content-Type" . "application/json"))))
    headers))

(defconst elm--claude-url "https://api.anthropic.com/v1/messages")

(defun elm--construct-content (content)
  "Construct the CONTENT to send to the API."
  `(("model" . ,elm--model)
    ("messages" . ((("role" . "user")
                    ("content" . ,content))))
    ,@(when (string-prefix-p "claude" elm--model)
        '(("max_tokens" . 1024)))))

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
    (elm--process-request (concat input "\n" prompt))))

(defun elm-send-request (input)
  "Send the INPUT request to CLAUDE."
  (interactive "sPrompt: ")
  (elm--process-request input))

(defun elm-menu()
  "Select the menu for elm."
  (interactive)
  (elm-transient))

(defvar elm--groq-key nil "API key for GROQ API.")
(defconst elm--groq-url "https://api.groq.com/openai/v1/chat/completions")

(defun elm--set-groq-api-key ()
  "Set the api key for claude."
  (unless elm--groq-key
    (elm--get-api-key "GROQ")
    elm--groq-key))

(defvar elm--groq-header
  (let* ((headers `(("Authorization: Bearer" . ,(elm--set-groq-api-key))
                    ("Content-Type" . "application/json"))))
    headers))




(provide 'elm)
;;; elm.el ends here
