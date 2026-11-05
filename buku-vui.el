;;; buku-vui.el --- VUI interface for the buku bookmark manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/ginqi7/buku-emacs
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

;; A `vui'-based front end for the `buku' bookmark manager.  The
;; bookmarks are fetched with `buku-list-async' and rendered as a table
;; with sortable, clickable columns; batch actions on selected rows are
;; exposed through the `buku-vui-table-perform-actions-in-selected'
;; transient.

;;; Code:
(require 'buku)

(require 'vui)
;; `vui-heading' and friends live in `vui-components', which `vui' itself
;; does not load.
(require 'vui-components)

;;; Custom Variables
(defgroup buku-vui nil
  "VUI interface for the buku bookmark manager."
  :group 'buku
  :prefix "buku-vui-")

(defcustom buku-vui-buffer-name "*buku-vui*"
  "Name of the buffer used for displaying the buku VUI interface."
  :type 'string
  :group 'buku-vui)

(defcustom buku-vui-table-headers
  '(("index" . (:title "" :key t :width 5 :checkbox t :on-click (lambda (_) (buku-vui-table-perform-actions-in-selected))))
    ("title" . (:title "TITLE" :width 50  :on-click buku-vui-table-edit-title))
    ("uri" . (:title "BASE URL" :width 20 :on-click buku-vui-table-edit-url :format buku-vui--get-domain))
    ("tags" . (:title "TAGS" :width 10 :on-click buku-vui-table-edit-tags))
    ("description" . (:title "DESC" :width 20 :on-click buku-vui-table-edit-desc)))
  "Table column definitions for the buku VUI interface.
Each element is a cons cell of the column key in the `buku' JSON
output and a property list with the column title, width and optional
click and formatting handlers."
  :type '(alist :key-type string :value-type plist)
  :group 'buku-vui)

;;; Internal Variables
(defvar-local buku-vui--instance nil
  "The VUI component instance for the buku interface.")

(defvar-local buku-vui--rows nil
  "List of rows (hash tables) currently displayed in the table.")

(defvar-local buku-vui--data nil
  "Bookmarks currently displayed, used to re-render the table on sorting.")

(defvar-local buku-vui--sort nil
  "Current sort as a cons cell (KEY . REVERSE), or nil when unsorted.")

(defvar-local buku-vui--search nil
  "Current search query string for filtering bookmarks.")

(defvar-local buku-vui--selected nil
  "List of currently selected row ID keys for batch operations.")

;;; Internal Functions

(defun buku-vui--get-domain (url)
  "Extract just the domain from URL."
  (let ((parsed (url-generic-parse-url url)))
    (url-host parsed)))

(cl-defun buku-vui--to-row (&key headers row)
  "Build a vui row for ROW using HEADERS.

Each cell is built from the column definition: the checkbox column
renders a toggle, columns with an :on-click handler are buttonized and
an optional :format handler may transform the raw value first."
  (mapcar
   (lambda (header)
     (let* ((key (car header))
            (value (format "%s" (gethash key row)))
            (props (cdr header))
            (on-click (plist-get props :on-click))
            (format-fun (plist-get props :format)))
       (cond
        ((buku-vui--header-checkbox-p header) (buku-vui--format-select header (format "%s" value)))
        (on-click (buttonize (if format-fun (funcall format-fun value) value)
                             (lambda (_) (funcall on-click row))))
        (t value))))
   headers))

(cl-defun buku-vui--to-rows (&key headers table)
  "Convert TABLE into a list of vui rows.

HEADERS defines the column structure and TABLE is a list of bookmark
hash tables as returned by `buku--json-parse-string'."
  (mapcar
   (apply-partially #'buku-vui--to-row
                    :headers headers
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

(defun buku-vui--set-sort (&optional key)
  "Sort the table by column KEY, reversing the order on repeated call.
KEY is the column key passed in by the clickable column headers."
  (interactive)
  (let ((key (or key (car-safe buku-vui--sort))))
    (setq buku-vui--sort
          (if (equal key (car-safe buku-vui--sort))
              (cons key (not (cdr buku-vui--sort)))
            (cons key nil)))
    (when buku-vui--data
      (buku-vui--render-rows))))

(defun buku-vui--sorted-rows (rows)
  "Return ROWS sorted according to `buku-vui--sort'."
  (if (null buku-vui--sort)
      rows
    (let ((key (car buku-vui--sort))
          (reverse (cdr buku-vui--sort)))
      (sort (copy-sequence rows)
            (lambda (row-1 row-2)
              (let ((value-1 (format "%s" (gethash key row-1)))
                    (value-2 (format "%s" (gethash key row-2))))
                (if reverse
                    (string> value-1 value-2)
                  (string< value-1 value-2))))))))

(defun buku-vui--headers-with-sort (headers)
  "Return HEADERS annotated with the :sorted and :reversed properties."
  (mapcar
   (lambda (header)
     (let ((props (copy-sequence (cdr header))))
       (when (equal (car header) (car-safe buku-vui--sort))
         (setq props (plist-put props :sorted t))
         (when (cdr buku-vui--sort)
           (setq props (plist-put props :reversed t))))
       (cons (car header) props)))
   headers))

(defun buku-vui--format-select (header value)
  "Return a checkbox button for VALUE of the checkbox column HEADER.
The box is checked when VALUE is in `buku-vui--selected'."
  (let ((map (make-sparse-keymap))
        (on-click (buku-vui--header-prop header :on-click)))
    (when on-click
      (define-key map (kbd "RET") (lambda () (interactive)
                                    (when (not (member value buku-vui--selected)) (buku-vui--select))
                                    (funcall on-click value))))
    (define-key map (kbd "SPC") #'buku-vui--select)
    (propertize (buttonize value nil)
                'display (if (member value buku-vui--selected) "☑" "☐")
                'keymap map)))

(defun buku-vui--header-checkbox-p (header)
  "Return non-nil if HEADER is the checkbox column."
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
         (width (plist-get props :width)))
    (list :header title
          :width width
          :truncate t
          :align :left)))

(defun buku-vui--to-columns (headers)
  "Convert HEADERS into a vui column spec list."
  (mapcar #'buku-vui--to-column headers))

(defun buku-vui--render-rows ()
  "Render `buku-vui--data' in the current buffer."
  (let* ((headers (buku-vui--headers-with-sort buku-vui-table-headers))
         (columns (buku-vui--to-columns headers))
         (rows (buku-vui--to-rows
                :headers headers
                :table (buku-vui--sorted-rows buku-vui--data)))
         (props (list :columns columns :rows rows))
         (position (point)))
    (setq-local buku-vui--rows (buku-vui--sorted-rows buku-vui--data))
    (if buku-vui--instance
        (vui-update-props buku-vui--instance props)
      (setq-local buku-vui--instance
                  (vui-mount (apply #'vui-component 'buku-vui props)
                             buku-vui-buffer-name)))
    (sit-for 0.2)
    (goto-char position)))

(defun buku-vui-render (json-str)
  "Parse JSON-STR and render the buku VUI table in the dedicated buffer."
  (with-current-buffer (get-buffer-create buku-vui-buffer-name)
    (setq-local buku-vui--data (buku--json-parse-string json-str))
    (buku-vui--render-rows)))

(defun buku-vui-table-edit (row type)
  "Edit the field TYPE of ROW, refreshing the VUI once `buku' finishes."
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

(defun buku-vui-table-add-tags (lst)
  "Prompt for tags and add them to every bookmark in LST."
  (let ((tags (read-string "Add tags: ")))
    (mapc (lambda (id)
            (buku-edit :id id
                       :type "tags"
                       :new-value tags
                       :tag "+"
                       :callback (lambda (_) (buku-vui buku-vui--search))))
          lst)))

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

(vui-defcomponent buku-vui-search ()
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
   ("t" "Add Tag"
    (lambda ()
      (interactive)
      (buku-vui-table-add-tags buku-vui--selected)
      (setq buku-vui--selected nil)
      (buku-vui buku-vui--search)))
   ("d" "Delete"
    (lambda ()
      (interactive)
      (mapc #'buku-delete buku-vui--selected)
      (setq buku-vui--selected nil)
      (buku-vui buku-vui--search)))
   ("RET" "Open by browser"
    (lambda ()
      (interactive)
      (mapc #'buku-open buku-vui--selected)
      (setq buku-vui--selected nil)
      (buku-vui buku-vui--search)))])

(defun buku-vui (&optional search)
  "Display the buku VUI interface, optionally filtered by SEARCH query."
  (interactive)
  (buku-list-async #'buku-vui-render search))

(provide 'buku-vui)

;;; buku-vui.el ends here
