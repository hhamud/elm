;;; elm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Hamza Hamud
;;
;; Author: Hamza Hamud <self@hamzahamud.com>
;; Maintainer: Hamza Hamud <self@hamzahamud.com>
;; Created: March 24, 2024
;; Modified: May 13, 2024
;; Version: 0.0.2
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
(require 'auth-source)

;; Constants and Variables

(defgroup elm nil
  "Emacs Language Model interface."
  :group 'tools
  :prefix "elm-")

(defcustom elm-env-file (expand-file-name "~/.authinfo.gpg")
  "Path to the .env file containing API keys."
  :type 'file
  :group 'elm)

(defvar elm--api-keys (make-hash-table :test 'equal)
  "Hash table to store API keys.")

(defvar elm--current-model "llama-3.1-70b-versatile"
  "Currently selected model.")

;; Utility Functions
(defun elm--read-auth (&rest keys)
  "Read the authsource file using the KEYS as selector."
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
        nil)))

(defun elm--read-auth-source ()
  "Read the auth source and fetches keys."
  (let ((api-keys elm--api-keys))
    (puthash "CLAUDE" (elm--read-auth :host "elm" :user "CLAUDE") api-keys)
    (puthash "GROQ" (elm--read-auth :host "elm" :user "GROQ") api-keys)
    api-keys))

(defun elm--create-dir ()
  "Create the hidden .elm directory in the home directory and creates an additional
  models.json file populated with the provider urls and models list"
  (let ((dir (expand-file-name "~/.elm"))
        (model-json "model.json")
        (json-data
                '(:claude (:url "https://api.anthropic.com/v1/messages"
                         :models ["claude-3-haiku-20240307"
                                  "claude-3-sonnet-20240229"
                                  "claude-3-5-sonnet-20240620"
                                  "claude-3-opus-20240229"])
                        :groq (:baseurl "https://api.groq.com/openai/v1" :geturl "/models" :chaturl "/chat/completions" :models [])
                        :ollama (:url "http://localhost:11434" :geturl "/api/tags" :chaturl "/api/chat" :models []))))
    (when (not (file-exists-p dir))
      (make-directory dir)
      (with-temp-buffer
        (json-insert json-data)
        (write-file (expand-file-name model-json dir))))))

;; make a call to each provider
;; parse json response
;; store data into json file
(defun elm--update-models (models)
  "update the MODELS list for each provider."

  )

(defun elm--get-api-key (service)
  "Get the API key for SERVICE."
  (or (gethash service elm--api-keys)
      (user-error "API key for %s not found. Please set it in %s" service elm-env-file)))

(defun elm--get-api-url ()
  "Get the API URL for the current model."
  (cdr (assoc (if (string-prefix-p "claude" elm--current-model) "claude" "groq") elm--api-urls)))


(defun elm--select-model ()
  "Prompt the user to select a Claude model from the list."
  (interactive)
  (let ((model-options elm--models))
    (let ((selected-model (assoc (completing-read "Select Model: " model-options nil t) model-options)))
      (setq elm--current-model (cdr selected-model)))))

(defun elm--construct-headers ()
  "Construct headers for API request."
  (let ((headers '(("Content-Type" . "application/json"))))
    (cond
        ((string-prefix-p "claude" elm--current-model)
        (progn
          (push `("x-api-key" . ,(elm--get-api-key "CLAUDE")) headers)
          (push '("anthropic-version" . "2023-06-01") headers)))
        ((string-prefix-p "llama-3.1" elm--current-model)
      (push `("Authorization" . ,(concat "Bearer " (elm--get-api-key "GROQ"))) headers))
    (t nil))
    headers))

(defvar elm--progress-reporter nil "Progress reporter for ELM.")

(transient-define-prefix elm-transient()
        ["Arguments" ("m" "Model" elm--select-model)])

(defun elm--progress-reporter (operation)
  "Progress reporter for ELM.
OPERATION should be \\='start, or \\='done."
  (pcase operation
    ('start (setq elm--progress-reporter (make-progress-reporter "ELM: Waiting for response from servers..." nil nil)))
    ('done (progress-reporter-done elm--progress-reporter))))

(defun elm--construct-content (content)
  "Construct the CONTENT to send to the API."
  `(("model" . ,elm--current-model)
    ("messages" . ((("role" . "user")
                    ("content" . ,content))))
    ,@(when (string-prefix-p "claude" elm--current-model)
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


(defun elm--process-claude-response (input response)
  "Process the RESPONSE from the CLAUDE API with original INPUT."
  (let* ((resp-text (cdr (assoc 'content response)))
        (final-resp (cdr (assoc 'text (aref resp-text 0)))))
        (elm--parse-response input final-resp)))

(defun elm--process-groq-response (input response)
  "Extract the content from the RESPONSE and add the original INPUT."
  (let* ((choices (cdr (assoc 'choices response)))
         (first-choice (aref choices 0))
         (message (cdr (assoc 'message first-choice)))
         (content (cdr (assoc 'content message))))
    (elm--parse-response input content)))

(defun elm--process-request (input)
  "Send the INPUT request to CLAUDE."
  (elm--read-auth-source)
  (elm--progress-reporter 'start)
  (let ((url (elm--get-api-url))
        (headers (elm--construct-headers)))
  (request url
    :type "POST"
    :headers headers
    :data (json-encode (elm--construct-content input))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (elm--progress-reporter 'done)
                (if (string-prefix-p "claude" elm--current-model)
                    (elm--process-claude-response input data)
                    (elm--process-groq-response input data))))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (elm--progress-reporter 'done)
              (let* ((error-data (request-response-data response))
                     (error-type (cdr (assoc 'type (cdr (assoc 'error error-data)))))
                     (error-message (cdr (assoc 'message (cdr (assoc 'error error-data))))))
                (message "Error: %s -%s" error-type error-message)))))))


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


(provide 'elm)
;;; elm.el ends here
