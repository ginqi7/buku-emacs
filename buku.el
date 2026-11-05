;;; buku.el --- Emacs frontend for the buku bookmark manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/ginqi7/buku-emacs
;; Package-Requires: ((emacs "29.1") (vui "1.0.0"))
;; Keywords: hypermedia, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A thin wrapper around the `buku' command line bookmark manager.  It
;; provides three layers:
;;
;; - asynchronous process helpers (`buku-*-async') that accept a
;;   callback receiving the raw output of the `buku' executable;
;; - interactive commands (`buku-add', `buku-search', `buku-delete',
;;   `buku-open', `buku-edit') built on top of them;
;; - the UI side lives in `buku-vui.el', which renders bookmarks with
;;   the `vui' toolkit.
;;
;; `buku-command-path' points at the executable; set it if `buku' is not
;; on `exec-path'.

;;; Code:

(require 'ansi-color)
(require 'transient)

;;; Custom Variables
(defgroup buku nil
  "Emacs frontend for the buku bookmark manager."
  :group 'external
  :prefix "buku-")

(defcustom buku-command-path (executable-find "buku")
  "Path to the `buku' executable.
Defaults to the executable found on the variable `exec-path', if any."
  :type '(choice (const :tag "Not found" nil) file)
  :group 'buku)

(defcustom buku-process-async-debug nil
  "Whether to log `buku' subprocess invocations and their output."
  :type 'boolean
  :group 'buku)

;;; Internal Variables

(defvar buku--edit-flags '(("title" . "--title")
                           ("uri" . "--url")
                           ("description" . "-c")
                           ("tags" . "--tag"))
  "Mapping of field names to their corresponding buku command-line flags.")

;;; Process Functions
(defun buku--process-async-start (name program callback err-callback &rest program-args)
  "Start an asynchronous process NAME running PROGRAM with PROGRAM-ARGS.
CALLBACK is called with stdout on success, ERR-CALLBACK with stderr on error."
  (let* ((buf (generate-new-buffer (format "*%s*" name)))
         (buf-err (generate-new-buffer (format "*%s:err*" name))))
    (when buku-process-async-debug
      (print (format "%s: %s" name program-args)))
    (make-process
     :name name
     :buffer buf
     :stderr buf-err
     :command (cons program program-args)
     :noquery t
     :sentinel (lambda (proc event)
                 (buku--process-sentinel proc event callback err-callback)))))

(defun buku--process-sentinel (proc _event callback err-callback)
  "Handle process PROC finishing.
On success CALLBACK is called with the standard output, otherwise
ERR-CALLBACK is called with the standard error output."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc))
          (buf-err (process-get proc 'stderr)))
      (unwind-protect
          (progn
            ;; Handle standard output
            (when (and callback (buffer-live-p buf))
              (with-current-buffer buf
                (ansi-color-apply-on-region (point-min) (point-max))
                (funcall callback (buffer-string))))
            ;; Handle error output
            (when (and err-callback (buffer-live-p buf-err))
              (with-current-buffer buf-err
                (ansi-color-apply-on-region (point-min) (point-max))
                (let ((err-content (buffer-string)))
                  (unless (string-empty-p err-content)
                    (funcall err-callback err-content))))))
        ;; Clean up resources (unless debugging mode is enabled)
        (unless buku-process-async-debug
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (buffer-live-p buf-err) (kill-buffer buf-err)))))))

;;; Internal Functions

(defun buku--message (title msg)
  "Prints a message with the given TITLE and MSG."
  (if (functionp 'knockknock-notify)
      (knockknock-notify :title title :message msg)
      (message (format "[%s]:\n%s" title msg))))

(defun buku--json-parse-string (str)
  "Parse the given JSON STR and return the result as list-based data structures.
Return nil if STR is not a valid JSON string."
  (when buku-process-async-debug
    (print (format "output is %s" str)))
  (condition-case nil
      (json-parse-string str :array-type 'list)
    (error nil)))

(defun buku--format-item (item)
  "Format a bookmark ITEM (hash table) into a readable string representation."
  (format "[%s](%s): %s %s"
          (gethash "title" item)
          (gethash "uri" item)
          (gethash "description" item)
          (gethash "tags" item)))

(defun buku--completing-read (callback json-str)
  "Prompt for a bookmark from JSON-STR and call CALLBACK with it."
  (let ((data (buku--json-parse-string json-str))
        (hash (make-hash-table :test #'equal))
        (callback (or callback #'print)))
    (mapc (lambda (item) (puthash (buku--format-item item) item hash)) data)
    (funcall
     callback
     (gethash
      (completing-read "Buku: " (mapcar #'buku--format-item data))
      hash))))

;;; Async Functions
(defun buku-delete-async (id &optional callback)
  "Asynchronously delete the bookmark ID.
CALLBACK is called with the output of `buku' once the process finishes."
  (buku--process-async-start
   "buku-delete" buku-command-path callback nil
   "-d" id "--tacit"))

(defun buku-list-async (&optional callback search)
  "Asynchronously list all bookmarks as JSON.
CALLBACK is called with the JSON string once the process finishes.
SEARCH, when non-nil and non-empty, restricts the listing to matching
bookmarks."
  (when (string-empty-p search)
    (setq search nil))
  (buku--process-async-start
   "buku-list" buku-command-path callback nil
   (or search "-p") "-j"))

(defun buku-add-async (url &optional callback)
  "Asynchronously add the bookmark URL.
CALLBACK is called with the output of `buku' once the process finishes."
  (buku--process-async-start
   "buku-add" buku-command-path callback nil
   "-a" url))

(defun buku-edit-async (args &optional callback)
  "Asynchronously edit a bookmark with the command line ARGS.
ARGS is a list containing the bookmark ID, the flag and the new value.
CALLBACK is called with the output of `buku' once the process finishes."
  (apply #'buku--process-async-start
         "buku-edit" buku-command-path callback nil
         (cons "-u" args)))

;;; Interactive Functions
(defun buku-add (&optional url)
  "Prompt for a URL and asynchronously add it as a bookmark.
URL can also be passed in directly."
  (interactive "sURL: ")
  (buku-add-async
   url
   (apply-partially #'buku--message "BUKU Add")))

(defun buku-search (&optional callback)
  "Prompt for a bookmark and call CALLBACK with the selected entry."
  (interactive)
  (buku-list-async (apply-partially #'buku--completing-read callback)))

(cl-defun buku-get (&key id url callback)
  "Retrieve a bookmark by ID or URL and invoke CALLBACK with it."
  (interactive)
  (ignore id url)
  (buku-list-async (apply-partially #'buku--completing-read callback)))

(defun buku-delete (&optional id)
  "Delete a bookmark by ID, prompting for confirmation before removal."
  (interactive)
  (let ((id (or id (read-string "Bookmark ID: "))))
    (when (yes-or-no-p (format "Delete bookmark %s? " id))
      (buku-delete-async (format "%s" id)
                         (lambda (msg) (buku--message "Delete" msg))))))

(defun buku-open (&optional id)
  "Open the bookmark ID in the default browser.
ID is prompted for when not given."
  (interactive)
  (let ((id (or id (read-string "Bookmark ID: "))))
    (shell-command-to-string (format "%s -o %s" buku-command-path id))))

(cl-defun buku-edit (&key id type old-value new-value tag callback)
  "Edit the field TYPE of the bookmark ID.
OLD-VALUE is offered as the default when prompting for the new value,
TAG is an optional modification flag (e.g. \"+\" for adding tags), and
CALLBACK is invoked once `buku' finishes."
  (interactive)
  (let* ((id (format "%s" (or id (read-string "Bookmark ID: "))))
         (type (or type (completing-read "Edit Type: " buku--edit-flags)))
         (flag (alist-get type buku--edit-flags nil nil #'equal))
         (new-value (or new-value (read-string (format "Edit(%s)[%s]: " id type) old-value))))
    (buku-edit-async (remove nil (list id flag tag new-value)) callback)))

(provide 'buku)
;;; buku.el ends here
