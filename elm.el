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
(require 'cl-lib)
(require 'transient)
(require 'auth-source)
(require 'json)
(require 'org)

;; Constants and Variables
(defgroup elm nil
  "Emacs Language Model interface."
  :group 'tools
  :prefix "elm-")

(defcustom elm-env-file (expand-file-name "~/.authinfo.gpg")
  "Path to the .env file containing API keys."
  :type 'file
  :group 'elm)

;; set json-false to nil as stream is set to nil
(setq json-false nil)

(defvar elm--api-keys (make-hash-table :test 'equal)
  "Hash table to store API keys.")

(defvar elm--api-urls (make-hash-table :test 'equal)
  "Hash table to store api urls.")

(defvar elm--models-file (expand-file-name "~/.elm/model.json")
  "File location of the models file.")

(defvar elm--current-model nil "Current model that is running.")

(defvar elm--current-provider nil "Current model that is running.")

(defvar elm--progress-reporter nil "Progress reporter for ELM.")

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
  "Create the hidden .elm directory in the home directory.
Create an additional models.json file populated with the
provider urls and models list."
  (let* ((dir (expand-file-name "~/.elm"))
        (json-data
                '(:claude (:baseurl "https://api.anthropic.com/v1"
                         :geturl ""
                         :chaturl "/messages"
                         :models ["claude-3-haiku-20240307"
                                  "claude-3-sonnet-20240229"
                                  "claude-3-5-sonnet-20240620"
                                  "claude-3-opus-20240229"])
                        :groq (:baseurl "https://api.groq.com/openai/v1" :geturl "/models" :chaturl "/chat/completions" :models [])
                        :ollama (:baseurl "http://localhost:11434" :geturl "/api/tags" :chaturl "/api/chat" :models [])))
    (json-encoding-pretty-print t))
    (unless (file-exists-p dir)
      (make-directory dir)
      (with-temp-file elm--models-file
          (insert (json-encode json-data))))))


(defun elm--create-url (model get)
  "Create the MODEL GET url from the json file."
  (let* ((json-data (elm--read-json))
         (urls (elm--extract-urls json-data get))
         (model-url (assoc model urls)))
    (if model-url
        (cadr model-url)
      (error "Model URL not found: %s" model))))


(defun elm--extract-urls (data get)
  "Extract DATA from the model json file.
Choose either the GET url or the chat url"
  (let ((result '()))
    (dolist (provider data)
      (let* ((baseurl (cdr (assoc 'baseurl provider)))
             (endpoint (cdr (assoc get provider)))
             (full-url (if endpoint
                           (concat baseurl endpoint)
                         baseurl)))
        (push (list (car provider) full-url) result)))
    (nreverse result)))


(defun elm--read-json ()
  "Read json file from .elm folder."
  (with-temp-buffer
    (insert-file-contents elm--models-file)
    (goto-char (point-min))
    (json-read)))

(defun elm--get-api-key (service)
  "Get the API key for SERVICE."
  (or (gethash service elm--api-keys)
      (user-error "API key for %s not found. Please set it in %s" service elm-env-file)))

(defun elm--construct-headers (model)
  "Construct headers for API request for each MODEL."
  (let ((headers '(("Content-Type" . "application/json"))))
    (cond
        ((string= model "claude")
        (progn
          (push `("x-api-key" . ,(elm--get-api-key "CLAUDE")) headers)
          (push '("anthropic-version" . "2023-06-01") headers)))
        ((string= model "groq")
      (push `("Authorization" . ,(concat "Bearer " (elm--get-api-key "GROQ"))) headers))
    (t nil))
    headers))

(defun elm--select-model ()
  "Prompt the user to select a model from all available providers."
  (interactive)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json-file elm--models-file) ; Replace with actual path
         (json-data (json-read-file json-file))
         (model-options '()))

    ;; Collect all models from all providers
    (maphash (lambda (provider provider-data)
               (let ((models (gethash "models" provider-data)))
                 (dolist (model models)
                     (push (cons model (cons model provider))
                           model-options))))
             json-data)

    ;; Sort model options alphabetically
    (setq model-options (sort model-options (lambda (a b) (string< (car a) (car b)))))

    ;; Prompt user to select a model
    (let* ((selected-option (completing-read "Select Model: " model-options nil t))
           (selected-model-data (cdr (assoc selected-option model-options))))
      (setq elm--current-model (car selected-model-data))
      (setq elm--current-provider (cdr selected-model-data))
      (message "Selected model: %s (Provider: %s)" elm--current-model elm--current-provider))))


(transient-define-prefix elm-transient ()
  ["Arguments"
   ("m" "Model" elm--select-model)])


(defun elm--progress-reporter (operation)
  "Progress reporter for elm OPERATION should be 'start, or 'done."
  (pcase operation
    ('start (setq elm--progress-reporter (make-progress-reporter "ELM: Waiting for response from servers..." nil nil)))
    ('done (progress-reporter-done elm--progress-reporter))))

(defun elm--construct-content (content)
  "Construct the CONTENT to send to the API."
  `(("model" . ,elm--current-model)
    ("messages" . ((("role" . "user")
                    ("content" . ,content))))
    ,@(cond ((string= "claude" elm--current-provider)
        '(("max_tokens" . 1024)))
        ((string= "ollama" elm--current-provider)
         '(("stream" . nil))))))


(defun elm--update-model-list (json-file provider lisp-data)
  "Extract model list from LISP-DATA and update JSON-FILE for the specified PROVIDER."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         ;;(data-array (cdr (assoc 'models lisp-data)))
         (provider-key (symbol-name provider))
         (json-data (json-read-file json-file))
         (model-list (if (string= provider-key "groq")
                         (progn
                         ;; groq parser
                           (mapcar (lambda (item)
                                     (let ((result (list)))
                                       (dolist (pair item)
                                         (setq result (cons pair result)))
                                       result)) (cdr (assoc 'data lisp-data))))
                         ;; ollama parse
                         (mapcar (lambda (item)
                                   (cdr (assoc 'model item))) (cdr (assoc 'models lisp-data))))))
    (if (gethash provider-key json-data)
        (progn
          (if (string= provider-key "groq")
              ;; groq model parser
              (progn
                (puthash "models"
                         (mapcar (lambda (x) (cdr (assoc 'id x))) model-list)
                                (gethash provider-key json-data)))
            ;; ollama model parser
            (progn
                (puthash "models" model-list
                         (gethash provider-key json-data))))
          (with-temp-file json-file
            (insert (json-encode json-data)))

          (message "Updated model list for %s in %s" provider-key json-file))
      (error "Provider %s not found in the JSON file" provider-key))))

(defun elm--update-models (model)
  "Update the MODEL list for each provider."
  (let ((url (elm--create-url model 'geturl)))
    (request url
      :type "GET"
      :headers (elm--construct-headers model)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                (elm--update-model-list elm--models-file model data)))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (let* ((error-data (request-response-data response))
                     (error-type (cdr (assoc 'type (cdr (assoc 'error error-data)))))
                     (error-message (cdr (assoc 'message (cdr (assoc 'error error-data))))))
                (message "Error: %s -%s" error-type error-message)))))))


(defun elm--parse-response (input output &optional code-block)
  "Parse the INPUT and OUTPUT into an org compatible format."
  (with-current-buffer (get-buffer-create "*elm*")
    (goto-char (point-max))
    (newline)
    (newline)
    (org-mode)
    (if (null code-block)
        (insert (format "* %s" input))
        (insert (format "* %s\n%s" input code-block)))
    (newline)
    (newline)
    (insert (elm--parse-code-blocks output))
    (switch-to-buffer-other-window (format "*elm--%s--%s*" elm--current-provider elm--current-model))))

(defun elm--parse-code-blocks (content)
  "Convert any code CONTENT from markdown to org-code-blocks."
  (shell-command-to-string (format "pandoc -f markdown -t org <(echo %s)" (shell-quote-argument content))))

(defun elm--process-ollama-response (input response &optional code-block)
  "Process the RESPONSE from the CLAUDE API with original INPUT."
        (elm--parse-response input (cdr (assoc 'content (cdr (assoc 'message response)))) code-block))

(defun elm--process-claude-response (input response &optional code-block)
  "Process the RESPONSE from the CLAUDE API with original INPUT."
  (let* ((resp-text (cdr (assoc 'content response)))
        (final-resp (cdr (assoc 'text (aref resp-text 0)))))
        (elm--parse-response input final-resp code-block)))

(defun elm--process-groq-response (input response &optional code-block)
  "Extract the content from the RESPONSE and add the original INPUT."
  (let* ((choices (cdr (assoc 'choices response)))
         (first-choice (aref choices 0))
         (message (cdr (assoc 'message first-choice)))
         (content (cdr (assoc 'content message))))
    (elm--parse-response input content code-block)))

(defun elm--process-request (input &optional code-block)
  "Send the INPUT request to CLAUDE."
  (elm--read-auth-source)
  (elm--progress-reporter 'start)
  (let* ((model (intern elm--current-provider))
         (url (elm--create-url model 'chaturl))
        (headers (elm--construct-headers model)))
  (request url
    :type "POST"
    :headers headers
    :data (json-encode (elm--construct-content input))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (elm--progress-reporter 'done)
                (cond
                 ((string= "claude" elm--current-provider)(elm--process-claude-response input data code-block))
                  ((string= "groq" elm--current-provider)(elm--process-groq-response input data code-block))
                  (t (elm--process-ollama-response input data code-block)))))
    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (elm--progress-reporter 'done)
              (let* ((error-data (request-response-data response))
                     (error-type (cdr (assoc 'type (cdr (assoc 'error error-data)))))
                     (error-message (cdr (assoc 'message (cdr (assoc 'error error-data))))))
                (message "Error: %s -%s" error-type error-message)))))))


(defun elm--get-buffer-language ()
  "Extract the language name from the current buffer's major mode."
  (let* ((mode-name (symbol-name major-mode))
         (lang-name (replace-regexp-in-string "-mode$" "" mode-name)))
        lang-name))

(defun elm-rewrite (prompt start end)
  "Rewrite specific using the PROMPT and area from START to END requested."
  (interactive "sPrompt: \nr")
  (let ((code-block (format "#+begin_src %s\n%s\n#+end_src" (elm--get-buffer-language)
                       (buffer-substring-no-properties start end))))
    (elm--process-request prompt code-block)))

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
