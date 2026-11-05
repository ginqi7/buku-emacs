;;; buku-test.el --- Tests for buku.el and buku-vui.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;; These tests only exercise the parts of buku.el and buku-vui.el that do
;; not need the external `buku' executable: parsing the JSON that
;; `buku -p -j' prints, formatting bookmarks into completion candidates and
;; turning bookmarks into VUI table rows and columns.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'buku)
(require 'buku-vui)

(defconst buku-test--json
  (concat "[{\"index\": 0,"
          "\"uri\": \"https://www.gnu.org/software/emacs/\","
          "\"title\": \"GNU Emacs\","
          "\"tags\": [\"editor\", \"emacs\"],"
          "\"description\": \"The extensible editor\","
          "\"status\": \"unread\"},"
          "{\"index\": 1,"
          "\"uri\": \"https://github.com/ginqi7/buku-emacs\","
          "\"title\": \"buku-emacs\","
          "\"tags\": [],"
          "\"description\": \"Buku frontend\","
          "\"status\": \"unread\"}]")
  "Sample of the JSON emitted by `buku -p -j'.")

(defun buku-test--bookmarks ()
  "Parse `buku-test--json' into a list of bookmark hash tables."
  (buku--json-parse-string buku-test--json))

(defun buku-test--bookmark (&rest pairs)
  "Create a bookmark hash table out of PAIRS."
  (let ((bookmark (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) bookmark))
    bookmark))

(describe "buku.el"
  (describe "buku--json-parse-string"
    (it "parses the output of `buku -p -j' into a list of hash tables"
      (let ((bookmarks (buku-test--bookmarks)))
        (expect (length bookmarks) :to-equal 2)
        (expect (hash-table-p (car bookmarks)) :to-be-truthy)
        (expect (gethash "title" (car bookmarks)) :to-equal "GNU Emacs")
        (expect (gethash "tags" (car bookmarks))
                :to-equal '("editor" "emacs"))))

    (it "returns nil for invalid JSON"
      (expect (buku--json-parse-string "not json") :to-be nil))

    (it "returns nil for empty output"
      (expect (buku--json-parse-string "") :to-be nil)))

  (describe "buku--format-item"
    (it "formats title, URL, description and tags into one line"
      (let ((bookmark (buku-test--bookmark
                       "index" 0
                       "title" "GNU Emacs"
                       "uri" "https://www.gnu.org/software/emacs/"
                       "description" "The extensible editor"
                       "tags" '("editor" "emacs"))))
        (expect (buku--format-item bookmark)
                :to-equal (concat "[GNU Emacs](https://www.gnu.org/software/emacs/)"
                                  ": The extensible editor (editor emacs)"))))

    (it "copes with missing fields"
      (expect (buku--format-item (buku-test--bookmark "title" "only a title"))
              :to-equal "[only a title](nil): nil nil")))

  (describe "buku--completing-read"
    (it "passes the selected bookmark to the callback"
      (let (selected)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (car candidates))))
          (buku--completing-read
           (lambda (bookmark) (setq selected bookmark))
           buku-test--json))
        (expect (gethash "title" selected) :to-equal "GNU Emacs"))))

  (describe "buku"
    (it "loads without the `buku' executable on `exec-path'"
      (expect (featurep 'buku) :to-be-truthy)
      (expect (boundp 'buku-command-path) :to-be-truthy)))

  (describe "buku--edit-flags"
    (it "maps every editable field to a buku command line flag"
      (expect (alist-get "title" buku--edit-flags nil nil #'equal)
              :to-equal "--title")
      (expect (alist-get "uri" buku--edit-flags nil nil #'equal)
              :to-equal "--url")
      (expect (alist-get "description" buku--edit-flags nil nil #'equal)
              :to-equal "-c")
      (expect (alist-get "tags" buku--edit-flags nil nil #'equal)
              :to-equal "--tag"))))

(describe "buku-vui.el"
  (describe "buku-vui--get-domain"
    (it "extracts the host from a URL"
      (expect (buku-vui--get-domain "https://www.gnu.org/software/emacs/")
              :to-equal "www.gnu.org"))

    (it "handles URLs without a path"
      (expect (buku-vui--get-domain "https://github.com")
              :to-equal "github.com")))

  (describe "buku-vui--header-prop"
    (it "reads properties from a header cell"
      (expect (buku-vui--header-prop '("title" . (:title "TITLE" :width 50))
                                     :title)
              :to-equal "TITLE")
      (expect (buku-vui--header-prop '("title" . (:title "TITLE" :width 50))
                                     :width)
              :to-equal 50))

    (it "returns nil for missing properties"
      (expect (buku-vui--header-prop '("title" . (:title "TITLE")) :checkbox)
              :to-be nil))

    (it "recognizes the checkbox column"
      (expect (buku-vui--header-checkbox-p '("index" . (:checkbox t)))
              :to-be-truthy)
      (expect (buku-vui--header-checkbox-p '("title" . (:width 50)))
              :to-be nil)))

  (describe "buku-vui--to-column"
    (it "converts a header into a VUI column"
      (expect (buku-vui--to-column '("title" . (:title "TITLE" :width 50)))
              :to-equal '(:header "TITLE" :width 50 :truncate t :align :left))))

  (describe "buku-vui--to-columns"
    (it "converts every header, keeping the declared order"
      (let ((columns (buku-vui--to-columns buku-vui-table-headers)))
        (expect (length columns) :to-equal (length buku-vui-table-headers))
        (expect (plist-get (nth 1 columns) :header) :to-equal "TITLE")
        (expect (plist-get (car columns) :header) :to-equal ""))))

  (describe "buku-vui--format-column"
    (it "shows no arrow when the column is not sorted"
      (expect (substring-no-properties
               (buku-vui--format-column '("title" . (:title "TITLE"))))
              :to-equal "title"))

    (it "shows an upward arrow when sorted ascending"
      (expect (substring-no-properties
               (buku-vui--format-column '("title" . (:title "TITLE" :sorted t))))
              :to-equal "title ↑"))

    (it "shows a downward arrow when sorted descending"
      (expect (substring-no-properties
               (buku-vui--format-column
                '("title" . (:title "TITLE" :sorted t :reversed t))))
              :to-equal "title ↓")))

  (describe "buku-vui--format-select"
    (it "renders an unchecked box by default"
      (let* ((buku-vui--selected nil)
             (cell (buku-vui--format-select '("index" . (:checkbox t)) "0")))
        (expect (substring-no-properties cell) :to-equal "0")
        (expect (get-text-property 0 'display cell) :to-equal "☐")))

    (it "renders a checked box for selected rows"
      (let* ((buku-vui--selected '("0"))
             (cell (buku-vui--format-select '("index" . (:checkbox t)) "0")))
        (expect (get-text-property 0 'display cell) :to-equal "☑"))))

  (describe "buku-vui--to-row"
    (it "builds one cell per header"
      (let* ((row (car (buku-test--bookmarks)))
             (cells (buku-vui--to-row :headers buku-vui-table-headers
                                      :row row)))
        (expect (length cells) :to-equal (length buku-vui-table-headers))
        (expect (substring-no-properties (nth 0 cells)) :to-equal "0")
        (expect (substring-no-properties (nth 1 cells)) :to-equal "GNU Emacs")
        (expect (substring-no-properties (nth 2 cells)) :to-equal "www.gnu.org")
        (expect (substring-no-properties (nth 3 cells))
                :to-equal "(editor emacs)")
        (expect (substring-no-properties (nth 4 cells))
                :to-equal "The extensible editor")))

    (it "buttonizes cells that have an on-click handler"
      (let* ((row (buku-test--bookmark
                   "index" 1 "title" "buku-emacs" "uri" "https://github.com"
                   "tags" '() "description" "Buku frontend"))
             (cells (buku-vui--to-row
                     :headers '(("title" . (:title "TITLE"
                                            :on-click ignore)))
                     :row row)))
        (expect (get-text-property 0 'button (car cells)) :to-be-truthy))))

  (describe "buku-vui--to-rows"
    (it "converts every bookmark into a row"
      (let* ((table (buku-test--bookmarks))
             (rows (buku-vui--to-rows :headers buku-vui-table-headers
                                      :table table)))
        (expect (length rows) :to-equal 2)
        (expect (substring-no-properties (nth 1 (nth 1 rows)))
                :to-equal "buku-emacs"))))

  (describe "buku-vui--sorted-rows"
    (it "keeps the original order when nothing is sorted"
      (let* ((buku-vui--sort nil)
             (rows (buku-vui--sorted-rows (buku-test--bookmarks))))
        (expect (gethash "title" (car rows)) :to-equal "GNU Emacs")))

    (it "sorts rows by the chosen column"
      (let* ((buku-vui--sort '("title" . nil))
             (rows (buku-vui--sorted-rows (buku-test--bookmarks))))
        (expect (mapcar (lambda (row) (gethash "title" row)) rows)
                :to-equal '("GNU Emacs" "buku-emacs"))))

    (it "reverses the order when sorting descending"
      (let* ((buku-vui--sort '("title" . t))
             (rows (buku-vui--sorted-rows (buku-test--bookmarks))))
        (expect (mapcar (lambda (row) (gethash "title" row)) rows)
                :to-equal '("buku-emacs" "GNU Emacs")))))

  (describe "buku-vui--set-sort"
    (it "toggles the direction on repeated clicks of the same column"
      (let ((buku-vui--sort nil)
            (buku-vui--data nil))
        (buku-vui--set-sort "title")
        (expect buku-vui--sort :to-equal '("title" . nil))
        (buku-vui--set-sort "title")
        (expect buku-vui--sort :to-equal '("title" . t))
        (buku-vui--set-sort "title")
        (expect buku-vui--sort :to-equal '("title" . nil))))

    (it "starts ascending when switching to another column"
      (let ((buku-vui--sort '("title" . t))
            (buku-vui--data nil))
        (buku-vui--set-sort "uri")
        (expect buku-vui--sort :to-equal '("uri" . nil)))))

  (describe "buku-vui--headers-with-sort"
    (it "annotates the sorted column"
      (let ((buku-vui--sort '("title" . t)))
        (let ((headers (buku-vui--headers-with-sort buku-vui-table-headers)))
          (expect (buku-vui--header-prop (nth 1 headers) :sorted)
                  :to-be-truthy)
          (expect (buku-vui--header-prop (nth 1 headers) :reversed)
                  :to-be-truthy)
          (expect (buku-vui--header-prop (nth 2 headers) :sorted)
                  :to-be nil))))

    (it "leaves every column untouched when unsorted"
      (let ((buku-vui--sort nil)
            (headers (buku-vui--headers-with-sort buku-vui-table-headers)))
        (expect (seq-some (lambda (header)
                            (buku-vui--header-prop header :sorted))
                          headers)
                :to-be nil))))

  (describe "buku-vui-table-add-tags"
    (it "edits every bookmark in the list with the entered tags"
      (let (calls)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "newtag"))
                  ((symbol-function 'buku-edit)
                   (lambda (&rest args) (push args calls))))
          (buku-vui-table-add-tags '("0" "1")))
        (expect (length calls) :to-equal 2)
        (expect (plist-get (car calls) :new-value) :to-equal "newtag")
        (expect (plist-get (car calls) :tag) :to-equal "+")
        (expect (mapcar (lambda (args) (plist-get args :id)) calls)
                :to-equal '("1" "0")))))

  (describe "buku-vui-render"
    (it "renders bookmarks into the VUI buffer"
      (let ((buku-vui-buffer-name (generate-new-buffer-name "*buku-vui-test*")))
        (unwind-protect
            (progn
              (buku-vui-render buku-test--json)
              (with-current-buffer buku-vui-buffer-name
                (expect (buffer-string) :to-match "GNU Emacs")
                (expect (buffer-string) :to-match "buku-emacs")))
          (when (get-buffer buku-vui-buffer-name)
            (kill-buffer buku-vui-buffer-name)))))))

;;; buku-test.el ends here
