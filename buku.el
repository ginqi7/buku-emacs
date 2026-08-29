;;; buku.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'transient)

;;; Custom Variables
(defcustom buku-command-path (executable-find "buku")
  "Specifies the path to the “buku” executable, defaulting to what is found on the system.")

(defcustom buku-process-async-debug nil "Specifies whether to enable debug logging for asynchronous operations with the Buku process.")

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

(defun buku--process-sentinel (proc event callback err-callback)
  "Handle process finish or death."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc))
          (buf-err (process-get proc 'stderr)))
      (unwind-protect
          (progn
            ;; Handle standard output
            (when (and callback (buffer-live-p buf))
              (with-current-buffer buf
                (ansi-color-apply-buffer)
                (funcall callback (buffer-string))))
            ;; Handle error output
            (when (and err-callback (buffer-live-p buf-err))
              (with-current-buffer buf-err
                (ansi-color-apply-buffer)
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
  "Parses the given JSON STR and returns the result as list-based data structures.
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
  "Parse JSON-STR and present bookmark entries for completion, invoking CALLBACK with selected entry."
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
  "Deletes the bookmark with the given ID in an asynchronous manner using the Buku command, optionally invoking the provided callback when the process finishes."
  (buku--process-async-start
   "buku-delete" buku-command-path callback nil
   "-d" id "--tacit"))

(defun buku-list-async (&optional callback search)
  "Asynchronously lists all bookmarks in JSON format, optionally invoking the provided callback when the process finishes."
  (when (string-empty-p search)
    (setq search nil))
  (buku--process-async-start
   "buku-list" buku-command-path callback nil
   (or search "-p") "-j"))

(defun buku-add-async (url &optional callback)
  "Asynchronously adds a bookmark with the given URL to Buku, optionally invoking the provided callback when the process finishes."
  (buku--process-async-start
   "buku-add" buku-command-path callback nil
   "-a" url))

(defun buku-edit-async (args &optional callback)
  "Asynchronously edits a bookmark with the specified arguments using Buku, optionally invoking the provided callback when the process completes.
ARGS is a list containing the bookmark ID, flag, and new value."
  (print args)
  (apply #'buku--process-async-start
         "buku-edit" buku-command-path callback nil
         (cons "-u" args)))

(defun buku-list--edit (arg)
  "Updates the specified field of a bookmark in the Buku list by prompting for a new value, then asynchronously applying the edit and refreshing the list."
  (let* ((entry (append (buku--list-get-entry) nil))
         (id (int-to-string (buku--list-get-id)))
         (header (car (alist-get arg buku--list-headers nil nil #'equal)))
         (index (cl-position-if
                 (lambda (item) (equal arg (car item)))
                 buku--list-headers))
         (old-value (nth index entry))
         (flag (alist-get arg buku--edit-flags nil nil #'equal))
         (new-value (read-string (format "Update %s: "header) old-value)))
    (buku-edit-async (list id flag new-value) (lambda (_) (buku-list)))))

;;; Interactive Functions
(defun buku-add (&optional url)
  "Interactively prompts for a URL and asynchronously adds it to Buku, printing the result."
  (interactive "sURL: ")
  (buku-add-async
   url
   (apply-partially #'buku--message "BUKU Add")))

(defun buku-list (&optional search)
  "Interactively lists bookmarks from Buku, rendering them via the buku--list-render function."
  (interactive)
  (buku-list-async #'buku--list-render search))

(defun buku-search (&optional callback)
  "Interactively searches Buku bookmarks, retrieving them asynchronously and using buku--completing-read to handle completion."
  (interactive)
  (buku-list-async (apply-partially #'buku--completing-read callback)))

(cl-defun buku-get (&key id url callback)
  "Retrieve a bookmark by ID or URL and invoke CALLBACK with the selected entry."
  (interactive)
  (buku-list-async (apply-partially #'buku--completing-read callback)))

(cl-defun buku-list-delete (&key id name)
  "Delete a bookmark by ID or NAME, prompting for confirmation before removal."
  (interactive)
  (let ((id (or id (read-string "Bookmark ID: "))))
    (when (yes-or-no-p (format "Are you sure you want to delete this? [%s]"
                               id))
      (buku-delete-async (format "%s" id) (lambda (msg) (buku--message "Delete" msg))))))

(defun buku-open (&optional id)
  "Opens the selected bookmark by invoking buku-command-path with the bookmark’s ID."
  (interactive)
  (let ((id (or id (read-string "Bookmark ID: "))))
    (shell-command-to-string (format "%s -o %s" buku-command-path id))))

(cl-defun buku-edit (&key id type old-value new-value callback)
  "Edit a bookmark field TYPE for entry ID, using OLD-VALUE as the default and invoking CALLBACK on completion."
  (interactive)
  (let* ((id (format "%s" (or id (read-string "Bookmark ID: "))))
         (type (or type (completing-read "Edit Type: " buku--edit-flags)))
         (flag (alist-get type buku--edit-flags nil nil #'equal))
         (new-value (or new-value (read-string (format "Edit(%s)[%s]: " id type) old-value))))
    (buku-edit-async (list id flag new-value) callback)))

(provide 'buku)
;;; buku.el ends here
