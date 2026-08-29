;;; buku-vui.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

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
(require 'buku)

(require 'vui)

;;; Custom Variables
(defcustom buku-vui-buffer-name "*buku-vui*"
  "Name of the buffer used for displaying the buku VUI interface.")

(defcustom buku-vui-table-headers
  '(("index" . (:title "" :key t :width 5 :checkbox t :on-click (lambda (_) (buku-vui-table-perform-actions-in-selected))))
    ("title" . (:title "Title" :width 10  :on-click buku-vui-table-edit-title))
    ("uri" . (:title "URL" :width 10 :on-click buku-vui-table-edit-url))
    ("tags" . (:title "Tags" :width 10 :on-click buku-vui-table-edit-tags))
    ("description" . (:title "Desc" :width 20 :on-click buku-vui-table-edit-desc)))
  "Table column definitions for buku-vui displaying index, title, URL, tags, and description.")

;;; Internal Variables
(defvar-local buku-vui--instance nil
  "The VUI component instance for the buku interface.")

(defvar-local buku-vui--rows nil
  "List of rows (hash tables) currently displayed in the table.")

(defvar-local buku-vui--search nil
  "Current search query string for filtering bookmarks.")

(defvar-local buku-vui--selected nil
  "List of currently selected row ID keys for batch operations.")

;;; Internal Functions
(cl-defun buku-vui--to-row (&key headers table row)
  "Build a vui row from ROW using HEADERS, TABLE, and ID-KEY.

Each row contains a checkbox, cell values for each header, and an
optional on-click handler per column."
  (mapcar
   (lambda (header)
     (let* ((key (car header))
            (value (format "%s" (gethash key row)))
            (props (cdr header))
            (on-click (plist-get props :on-click)))
       (cond
        ((buku-vui--header-checkbox-p header) (buku-vui--format-select header (format "%s" value)))
        (on-click (buttonize value (lambda (_) (funcall on-click row))))
        (t value))))
   headers))

(cl-defun buku-vui--to-rows (&key headers table)
  "Convert TABLE rows into a list of vui rows sorted by the active sort header.

HEADERS defines the column structure, TABLE is a list of hash tables,
and ID-KEY identifies each row uniquely for selection."
  (mapcar
   (apply-partially #'buku-vui--to-row
                    :headers headers
                    :table table
                    :row)
   table))

(defun buku-vui--format-column (header)
  "Format HEADER into a clickable column label with optional sort indicator.

HEADER is a cons cell (name . props) where props may contain :sorted
and :reversed keys to control the sort arrow display."
  (let ((name (car header))
        (props (cdr header)))
    (concat
     (buttonize name #'buku-vui--set-sort)
     (if (plist-get props :sorted)
         (if (plist-get props :reversed) " ↓" " ↑")
       ""))))

(defun buku-vui--format-select (header value)
  "Return a checkbox button for KEY, toggled if already selected."
  (let ((map (make-sparse-keymap))
        (on-click (buku-vui--header-prop header :on-click)))
    (when on-click
      (define-key map (kbd "RET") (lambda () (interactive) (funcall on-click value))))
    (define-key map (kbd "SPC") #'buku-vui--select)
    (propertize (buttonize value nil)
                'display (if (member value buku-vui--selected) "☑" "☐")
                'keymap map)))

(defun buku-vui--header-checkbox-p (header)
  (buku-vui--header-prop header :checkbox))

(defun buku-vui--header-prop (header prop)
  "Get property PROP from HEADER's property list."
  (let ((props (cdr header)))
    (plist-get props prop)))

(defun buku-vui--select ()
  "Toggle selection state for the checkbox at point."
  (interactive)
  (let* ((data (button-at (point)))
         (begin (button-start data))
         (end (button-end data))
         (key (buffer-substring-no-properties begin end))
         (selected-p (member key buku-vui--selected))
         (buffer-read-only nil)
         (inhibit-read-only t))
    (if selected-p
        (progn
          (message "Unselect %s" key)
          (setq buku-vui--selected (remove key buku-vui--selected)))
      (message "Select %s" key)
      (add-to-list 'buku-vui--selected key))
    (put-text-property begin end 'display (if selected-p "☐" "☑"))))

(defun buku-vui--to-column (header)
  "Convert a single HEADER (a cons cell) into a vui column spec."
  (let* ((props (cdr header))
         (title (plist-get props :title))
         (width (nth 1 (cdr header))))
    (list :header title
          :width 25
          :truncate t
          :align :left)))

(defun buku-vui--to-columns (headers)
  "Convert HEADERS into a vui column spec list, prepending an empty checkbox column."
  (mapcar #'buku-vui--to-column headers))

(defun buku-vui-render (json-str)
  "Parse JSON-STR and render the buku VUI table in the dedicated buffer."
  (with-current-buffer (get-buffer-create buku-vui-buffer-name)
    (let* ((columns (buku-vui--to-columns buku-vui-table-headers))
           (json-data (buku--json-parse-string json-str))
           (rows (buku-vui--to-rows
                  :headers buku-vui-table-headers
                  :table json-data))
           (props (list :columns columns :rows rows))
           (pos (point)))
      (if buku-vui--instance
          (vui-update-props buku-vui--instance props)
        (setq-local buku-vui--instance (vui-mount (apply #'vui-component 'buku-vui props)  buku-vui-buffer-name)))
      (goto-char pos))))

(defun buku-vui-table-edit (row type)
  "Edit field TYPE for ROW, prompting for new value and refreshing the VUI on completion."
  (buku-edit :id (gethash "index" row)
             :type type
             :old-value (gethash type row)
             :callback (lambda (_) (buku-vui buku-vui--search))))

(defun buku-vui-table-edit-desc (row)
  "Edit the description field for ROW."
  (buku-vui-table-edit row "description"))

(defun buku-vui-table-edit-tags (row)
  "Edit the tags field for ROW."
  (buku-vui-table-edit row "tags"))

(defun buku-vui-table-edit-title (row)
  "Edit the title field for ROW."
  (buku-vui-table-edit row "title"))

(defun buku-vui-table-edit-url (row)
  "Edit the URL field for ROW."
  (buku-vui-table-edit row "uri"))

(vui-defcomponent buku-vui (columns rows)
  :state ((search buku-vui--search))
  :render
  (vui-vstack
   (vui-heading "Buku: Personal mini-web in text")
   (vui-component 'buku-vui-add)
   (vui-component 'buku-vui-search)
   (vui-component 'buku-vui-table :columns columns :rows rows)))

(vui-defcomponent buku-vui-add ()
  :state ((url buku-vui--search))
  :render
  (vui-hstack
   (vui-field :value url
              :size 30
              :placeholder "Bookmark URL"
              :on-change (lambda (v) (vui-set-state :url v)))
   (vui-button "Add"
     :face 'custom-button
     :on-click (lambda () (buku-add url) (vui-set-state :url "")))))

(vui-defcomponent buku-vui-search (search on-search-change)
  :state ((search ""))
  :render
  (vui-hstack
   (vui-field
    :value search
    :size 30
    :placeholder "Search"
    :on-change (lambda (v)
                 (setq buku-vui--search v)
                 (vui-set-state :search v)))
   (vui-button "Search"
     :face 'custom-button
     :on-click (lambda ()
                 (buku-vui search)))))

(vui-defcomponent buku-vui-table (columns rows)
  :render
  (vui-table
   :columns columns
   :rows rows
   :sticky-header t
   :border :unicode))

(transient-define-prefix  buku-vui-table-perform-actions-in-selected ()
  "Perform actions on selected entries in buku-vui table."
  [:description
   (lambda () (format "Selected %s items." (length buku-vui--selected)))
   ("d" "Delete" (lambda () (interactive)))
   ("RET" "Open by browser" (lambda () (interactive) (mapcar #'buku-open buku-vui--selected)))])

(defun buku-vui (&optional search)
  "Display the buku VUI interface, optionally filtered by SEARCH query."
  (interactive)
  (buku-list-async #'buku-vui-render search))

(provide 'buku-vui)

;;; buku-vui.el ends here
