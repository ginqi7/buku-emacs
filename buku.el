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
(defvar buku--list-headers '(("index". ("ID" 5 t))
                             ("title" . ("Title" (lambda () (/ (window-width) 3)) t))
                             ("uri" . ("URL" (lambda () (/ (window-width) 3)) t))
                             ("tags" . ("Tags" 10 t))
                             ("description" . ("Desc" (lambda () (/ (window-width) 3)) t))))

(defvar buku--list-key "index")

(defvar buku--edit-flags '(("title" . "--title")
                           ("uri" . "--url")
                           ("description" . "-c")
                           ("tags" . "--tag")))

(defvar buku--buffer-name "*buku*")

;;; Process Functions
(defun buku--process-async-start (name program callback err-callback &rest program-args)
  (let* ((buf (generate-new-buffer (format "*%s*" name)))
         (buf-err (generate-new-buffer (format "*%s:err*" name))))
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
(defun buku--list-get-id ()
  "Returns the ID of the current tabulated list entry."
  (tabulated-list-get-id))

(defun buku--list-get-entry ()
  "Returns the tabulated list entry from the current line."
  (tabulated-list-get-entry))

(defun buku--message (title msg)
  "Prints a message with the given TITLE and MSG."
  (print (format "[%s]:\n%s" title (string-replace "\\n" "\n" msg))))

(defun buku--json-parse-string (str)
  "Parses the given JSON STR and returns the result as list-based data structures."
  (json-parse-string str :array-type 'list))

(defun buku--completing-read (json-str)
  "Prompts the user with a completing-read list built from the Buku items in the given JSON string, formatting each item’s title, URI, description, and tags into a single string."
  (let ((data (buku--json-parse-string json-str)))
    (completing-read "Buku: "
                     (mapcar
                      (lambda (item)
                        (format "%s %s %s %s"
                                (gethash "title" item)
                                (gethash "uri" item)
                                (gethash "description" item)
                                (gethash "tags" item)))
                      data))))

(defun buku--hash-to-list (hash)
  "Converts the given HASH table into a list of string values based on buku--list-headers, retrieving each header key from the hash and formatting it as a string. If a key is not found, an empty string is used instead."
  (mapcar
   (lambda (key) (format "%s" (gethash (car key) hash "")))
   buku--list-headers))

(defun buku--header-width (header)
  "Returns the HEADER width. If the third element of the header is a function, call it to obtain the width; otherwise return its value directly."
  (let ((width-exp (caddr header)))
    (if (functionp width-exp)
        (funcall width-exp)
      width-exp)))

(defun buku--build-header (item)
  "Builds a list from the cdr of item by calling each element if it is a function, then returning it."
  (mapcar (lambda (e)
            (if (functionp e)
                (funcall e)
              e))
          (cdr item)))

(defun buku--build-tabulated-list-entries (data)
  "Builds a list of tabulated-list entries from the given DATA. Each entry is a list where the first element is the index key from the item, and the second element is a vector of values returned by buku--hash-to-list on that item."
  (mapcar
   (lambda (item)
     (list (gethash "index" item)
           (vconcat (buku--hash-to-list item))))
   data))

(defun buku--list-render (data)
  "Renders a tabulated list of the DATA in buku--buffer-name. It sets up the tabulated-list format using buku--header-width, parses the JSON data into a list of entries, initializes and prints the tabulated list in buku-list-mode, and then displays the buffer."
  (with-current-buffer (get-buffer-create buku--buffer-name)
    (setq tabulated-list-format
          (vconcat (mapcar #'buku--build-header buku--list-headers)))
    (setq tabulated-list-entries (buku--build-tabulated-list-entries
                                  (buku--json-parse-string data)))
    (buku-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; Async Functions
(defun buku-delete-async (id &optional callback)
  "Deletes the bookmark with the given ID in an asynchronous manner using the Buku command, optionally invoking the provided callback when the process finishes."
  (buku--process-async-start
   "buku-delete" buku-command-path callback nil
   "-d" id "--tacit"))

(defun buku-list-async (&optional callback)
  "Asynchronously lists all bookmarks in JSON format, optionally invoking the provided callback when the process finishes."
  (buku--process-async-start
   "buku-list" buku-command-path callback nil
   "-p" "-j"))

(defun buku-add-async (url &optional callback)
  "Asynchronously adds a bookmark with the given URL to Buku, optionally invoking the provided callback when the process finishes."
  (buku--process-async-start
   "buku-add" buku-command-path callback nil
   "-a" url))

(defun buku-edit-async (args &optional callback)
  "Asynchronously edits a bookmark with the specified arguments using Buku, optionally invoking the provided callback when the process completes."
  (print args)
  (apply #'buku--process-async-start
         "buku-edit" buku-command-path callback nil
         (append (list "-u") args)))

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
(defun buku-add ()
  "Interactively prompts for a URL and asynchronously adds it to Buku, printing the result."
  (interactive)
  (buku-add-async
   (read-string "URL: ")
   #'print))

(defun buku-list ()
  "Interactively lists bookmarks from Buku, rendering them via the buku--list-render function."
  (interactive)
  (buku-list-async #'buku--list-render))

(defun buku-search ()
  "Interactively searches Buku bookmarks, retrieving them asynchronously and using buku--completing-read to handle completion."
  (interactive)
  (buku-list-async #'buku--completing-read))

(defun buku-list-delete ()
  "Deletes the selected bookmark from the Buku list after user confirmation, then asynchronously performs the delete operation and displays a status message."
  (interactive)
  (let ((id (buku--list-get-id))
        (entry (buku--list-get-entry)))
    (when (yes-or-no-p (format "Are you sure you want to delete this? [%s]"
                               (elt entry 1)))
      (buku-delete-async (int-to-string id) (lambda (msg) (buku--message "Delete" msg))))))

(defun buku-list-open ()
  "Opens the selected bookmark by invoking buku-command-path with the bookmark’s ID."
  (interactive)
  (shell-command-to-string (format "%s -o %s" buku-command-path (buku--list-get-id))))

(transient-define-prefix buku-list-edit ()
  "Invokes a transient menu providing commands to edit different fields of the selected bookmark, including its link, tagset, title, and description."
  ["Edit:"
   ("u" "bookmark link" (lambda () (interactive) (buku-list--edit "uri")))
   ("a" "bookmark tagset" (lambda () (interactive) (buku-list--edit "tags")))
   ("t" "bookmark title" (lambda () (interactive) (buku-list--edit "title")))
   ("c" "bookmark description" (lambda () (interactive) (buku-list--edit "description")))])

(transient-define-prefix  buku-list-actions ()
  "Provides a transient menu for performing actions on the current bookmark, such as deleting, opening, or editing it."
  ["Actions"
   ("d" "Delete" buku-list-delete)
   ("o" "Open by browser" buku-list-open)
   ("e" "Edit" buku-list-edit)])

;;; Define Mode
(define-derived-mode buku-list-mode tabulated-list-mode "buku-list"
  "Defines a major mode based on tabulated-list-mode for displaying and interacting with Buku bookmarks, with RET bound to buku-list-actions."
  (keymap-set buku-list-mode-map "RET" #'buku-list-actions))

(provide 'buku)
;;; buku.el ends here
