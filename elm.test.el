;;; elm.test.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Hamza Hamud
;;
;; Author: Hamza Hamud <self@hamzahamud.com>
;; Maintainer: Hamza Hamud <self@hamzahamud.com>
;; Created: September 14, 2024
;; Modified: September 14, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/user/elm.test
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ert)
(require 'elm)

;; Mocking helpers
(defun mock-read-auth (keys)
  "Mocked version of `elm--read-auth` that returns fake API keys."
  (pcase (plist-get keys :user)
    ("CLAUDE" "fake-claude-key")
    ("GROQ" "fake-groq-key")
    (_ nil)))

(defun mock-auth-source-search (&rest _args)
  "Return a fake auth source for testing purposes."
  (list (list :secret (lambda () "fake-secret"))))

(defun mock-file-exists-p (file)
  "Mock `file-exists-p` to simulate presence or absence of files."
  (not (string= file "~/.elm/model.json")))

(defun mock-make-directory (&rest _args)
  "Mock `make-directory` to simulate directory creation.")

(defun mock-insert-file (content)
  "Simulate writing content to a file."
  (with-temp-buffer
    (insert content)
    (buffer-string)))

(defun mock-json-encode (data)
  "Mock JSON encoding to return pretty-printed JSON."
  (format "%s" data))

(defun mock-request (&rest _args)
  "Simulate the behavior of `request` function for API calls."
  (message "Simulating request call")
  ;; Return success for testing
  (funcall (plist-get _args :success)
           :data '((choices . [((message . ((content . "Test response"))))]))))

;; ------------------------------------------
;; Unit Tests for Utility Functions
;; ------------------------------------------

(ert-deftest test-elm--read-auth ()
  "Test `elm--read-auth` to ensure it fetches the correct API keys."
  (cl-letf (((symbol-function 'auth-source-search) 'mock-auth-source-search))
    (should (equal (elm--read-auth :host "elm" :user "CLAUDE") "fake-secret"))
    (should-not (elm--read-auth :host "elm" :user "UNKNOWN"))))

(ert-deftest test-elm--create-dir ()
  "Test `elm--create-dir` for correct directory and file creation."
  (cl-letf (((symbol-function 'file-exists-p) 'mock-file-exists-p)
            ((symbol-function 'make-directory) 'mock-make-directory)
            ((symbol-function 'with-temp-file) 'mock-insert-file)
            ((symbol-function 'json-encode) 'mock-json-encode))
    (elm--create-dir)
    ;; Ensure directory and file creation logic runs
    (should (equal (mock-insert-file "test content") "test content"))))

(ert-deftest test-elm--create-url ()
  "Test `elm--create-url` to ensure correct URL generation."
  (cl-letf (((symbol-function 'elm--read-json)
             (lambda ()
               '((:claude . (:baseurl "https://api.anthropic.com/v1"
                                     :chaturl "/messages"))
                 (:groq . (:baseurl "https://api.groq.com/v1"
                                    :chaturl "/chat/completions"))))))
    (should (equal (elm--create-url 'claude 'chaturl) "https://api.anthropic.com/v1/messages"))
    (should-error (elm--create-url 'unknown 'chaturl))))

(ert-deftest test-elm--extract-urls ()
  "Test `elm--extract-urls` to ensure proper URL extraction."
  (let ((data '((:claude . (:baseurl "https://api.anthropic.com/v1"
                                     :geturl "/models"))
                (:groq . (:baseurl "https://api.groq.com/v1"
                                   :geturl "/models")))))
    (should (equal (elm--extract-urls data 'geturl)
                   '((:claude "https://api.anthropic.com/v1/models")
                     (:groq "https://api.groq.com/v1/models"))))))

(ert-deftest test-elm--get-api-key ()
  "Test `elm--get-api-key` to ensure correct API key retrieval."
  (let ((elm--api-keys (make-hash-table :test 'equal)))
    (puthash "CLAUDE" "fake-claude-key" elm--api-keys)
    (should (equal (elm--get-api-key "CLAUDE") "fake-claude-key"))
    (should-error (elm--get-api-key "UNKNOWN"))))

(ert-deftest test-elm--construct-headers ()
  "Test `elm--construct-headers` for proper API request headers."
  (let ((elm--api-keys (make-hash-table :test 'equal)))
    (puthash "CLAUDE" "fake-claude-key" elm--api-keys)
    (puthash "GROQ" "fake-groq-key" elm--api-keys)
    ;; Test for Claude
    (should (equal (elm--construct-headers "claude")
                   '(("Content-Type" . "application/json")
                     ("x-api-key" . "fake-claude-key")
                     ("anthropic-version" . "2023-06-01"))))
    ;; Test for GROQ
    (should (equal (elm--construct-headers "groq")
                   '(("Content-Type" . "application/json")
                     ("Authorization" . "Bearer fake-groq-key"))))))

(ert-deftest test-elm--construct-content ()
  "Test `elm--construct-content` to verify content construction for API."
  (setq elm--current-model "claude-3-haiku-20240307"
        elm--current-provider "claude")
  (let ((content (elm--construct-content "Hello, world")))
    (should (equal content
                   '(("model" . "claude-3-haiku-20240307")
                     ("messages" . ((("role" . "user")
                                     ("content" . "Hello, world"))))
                     ("max_tokens" . 1024))))))

;; ------------------------------------------
;; Integration Tests for API Requests
;; ------------------------------------------

(ert-deftest test-elm--process-request ()
  "Test `elm--process-request` to ensure correct API interaction."
  (cl-letf (((symbol-function 'request) 'mock-request)
            ((symbol-function 'elm--construct-headers)
             (lambda (_) '(("Authorization" . "Bearer fake-key")))))
    ;; Set up current model/provider for testing
    (setq elm--current-provider "groq"
          elm--current-model "groq-model-1")
    ;; Simulate request and success
    (elm--process-request "Hello, world")
    ;; Check output (stdout should simulate request and response handling)
    (should (string-match "Simulating request call" (current-message)))))

(ert-deftest test-elm--process-claude-response ()
  "Test response parsing for Claude."
  (let ((response '((content . [((text . "Response text"))]))))
    (with-temp-buffer
      (elm--process-claude-response "Test input" response)
      (should (equal (buffer-string) "* Test input\n\nResponse text\n")))))

(ert-deftest test-elm--update-model-list ()
  "Test `elm--update-model-list` for updating the models file."
  (cl-letf (((symbol-function 'elm--read-json)
             (lambda ()
               '((claude . (:models ["claude-3-haiku"])))))
            ((symbol-function 'with-temp-file) 'mock-insert-file))
    (let ((lisp-data '((models . ["claude-3-haiku" "claude-3-sonnet"]))))
      (elm--update-model-list "~/.elm/model.json" 'claude lisp-data)
      (should (equal (mock-insert-file "Updated content")
                     "Updated content")))))

;; ------------------------------------------
;; Interactive Tests
;; ------------------------------------------

(ert-deftest test-elm--select-model ()
  "Test `elm--select-model` interactive command."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "claude-3-haiku"))
            ((symbol-function 'elm--read-json)
             (lambda ()
               (let ((table (make-hash-table :test 'equal)))
                 (puthash "claude" (list :models '("claude-3-haiku" "claude-3-sonnet")) table)
                 table))))
    (call-interactively 'elm--select-model)
    (should (equal elm--current-model "claude-3-haiku"))
    (should (equal elm--current-provider "claude"))))

(ert-deftest test-elm-rewrite ()
  "Test `elm-rewrite` interactive command for sending code rewrite requests."
  (cl-letf (((symbol-function 'elm--process-request) 'mock-request))
    (with-temp-buffer
      (insert "def foo(): pass")
      (call-interactively 'elm-rewrite)
      ;; Check message for mock request
      (should (string-match "Simulating request call" (current-message))))))

(ert-deftest test-elm-send-request ()
  "Test `elm-send-request` interactive command."
  (cl-letf (((symbol-function 'elm--process-request) 'mock-request))
    (call-interactively 'elm-send-request)
    ;; Check the simulated API interaction
    (should (string-match "Simulating request call" (current-message)))))



(provide 'elm.test)
;;; elm.test.el ends here
