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
  "")
(defcustom buku-process-async-debug nil "")

;;; Internal Variables
(defvar buku--list-headers '(("index". ("ID" 5 t))
                             ("title" . ("Title" (lambda () (/ (window-width) 3)) t))
                             ("uri" . ("URL" (lambda () (/ (window-width) 3)) t))
                             ("tags" . ("Tags" 10 t))
                             ("description" . ("Desc" 1 t))))

(defvar buku--list-key "index")

(defvar buku--edit-flags '(("title" . "--title")
                           ("uri" . "--url")
                           ("description" . "-c")
                           ("tags" . "--tag")))

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
(defun buku--message (title msg)
  (print (format "[%s]:\n%s" title
                 (string-replace "\\n" "\n" msg))))

(defun buku--completing-read (json-str)
  (let ((data (json-parse-string json-str
                                 :array-type 'list)))
    (completing-read "Buku: "
                     (mapcar
                      (lambda (item)
                        (format "%s %s %s %s"
                                (gethash "title" item)
                                (gethash "uri" item)
                                (gethash "description" item)
                                (gethash "tags" item)))
                      data))))

(defun buku--build-tabulated-list-entries (data)
  (mapcar
   (lambda (item)
     (list (gethash "index" item)
           (vconcat (mapcar (lambda (key)
                              (format "%s"
                                      (or (gethash (car key) item) "")))
                            buku--list-headers))))
   data))

(defun buku--list-render (data)
  (with-current-buffer (get-buffer-create "*buku*")
    (setq tabulated-list-format
          (vconcat (mapcar
                    (lambda (item)
                      (mapcar
                       (lambda (e)
                         (if (functionp e)
                             (funcall e)
                           e))
                       (cdr item)))
                    buku--list-headers)))
    (setq tabulated-list-entries (buku--build-tabulated-list-entries
                                  (json-parse-string data
                                                     :array-type 'list)))
    (buku-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; Async Functions
(defun buku-delete-async (id &optional callback)
  (buku--process-async-start
   "buku-delete" buku-command-path callback nil
   "-d" id "--tacit"))

(defun buku-list-async (&optional callback)
  (buku--process-async-start
   "buku-list" buku-command-path callback nil
   "-p" "-j"))

(defun buku-add-async (url &optional callback)
  (buku--process-async-start
   "buku-add" buku-command-path callback nil
   "-a" url))

(defun buku-edit-async (args &optional callback)
  (apply #'buku--process-async-start
         "buku-edit" buku-command-path callback nil
         (append (list "-u") args)))

;;; Interactive Functions
(defun buku-add ()
  (interactive)
  (buku-add-async
   (read-string "URL: ")
   #'print))

(defun buku-list ()
  (interactive)
  (buku-list-async #'buku--list-render))

(defun buku-search ()
  (interactive)
  (buku-list-async #'buku--completing-read))

(defun buku-list-delete ()
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (when (yes-or-no-p (format "Are you sure you want to delete this? [%s]"
                               (elt entry 1)))
      (buku-delete-async (int-to-string id) (lambda (msg) (buku--message "Delete" msg))))))

(defun buku-list-open ()
  (interactive)
  (shell-command-to-string (format "%s -o %s" buku-command-path (tabulated-list-get-id))))

(defun buku-list--edit (arg)
  (let* ((entry (append (tabulated-list-get-entry) nil))
         (id (int-to-string (tabulated-list-get-id)))
         (header (car (alist-get arg buku--list-headers nil nil #'equal)))
         (index (cl-position-if
                 (lambda (item) (equal arg (car item)))
                 buku--list-headers))
         (old-value (nth index entry))
         (flag (alist-get arg buku--edit-flags nil nil #'equal))
         (new-value (read-string (format "Update %s: "header) old-value)))
    (buku-edit-async (list id flag new-value) (lambda (_) (buku-list)))))

(transient-define-prefix buku-list-edit ()
  ["Edit:"
   ("u" "bookmark link" (lambda () (interactive) (buku-list--edit "uri")))
   ("a" "bookmark tagset" (lambda () (interactive) (buku-list--edit "tags")))
   ("t" "bookmark title" (lambda () (interactive) (buku-list--edit "title")))
   ("c" "bookmark description" (lambda () (interactive) (buku-list--edit "description")))])

(transient-define-prefix  buku-list-actions ()
  ["Actions"
   ("d" "Delete" buku-list-delete)
   ("o" "Open by browser" buku-list-open)
   ("e" "Edit" buku-list-edit)])

;;; Define Mode
(define-derived-mode buku-list-mode tabulated-list-mode "buku-list"
  ""
  (keymap-set buku-list-mode-map "RET" #'buku-list-actions))

(provide 'buku)
;;; buku.el ends here
