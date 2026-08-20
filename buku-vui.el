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

(defvar-local buku-vui--instance nil)

(defvar-local buku-vui--rows nil)

(vui-defcomponent vui-buku (columns rows)
  :state ((url "")
          (search ""))
  :render
  (vui-vstack
   (vui-heading "Buku: Personal mini-web in text")
   (vui-hstack
    (vui-field :value url :size 30 :placeholder "Bookmark URL"
               :on-change (lambda (v) (vui-set-state :url v)))
    (vui-button "Add"
      :face 'custom-button
      :on-click (lambda () (buku-add url) (vui-set-state :url ""))))
   (vui-hstack
    (vui-field :value search :size 30 :placeholder "Search"
               :on-change (lambda (v) (vui-set-state :search v)))
    (vui-button "Search"
      :face 'custom-button
      :on-click (lambda () (buku-list search) (vui-set-state :search ""))))
   (vui-table
    :sticky-header t
    :columns columns
    :rows rows
    :border :unicode)))

(defun buku-vui--list-render (json-str)
  "Render JSON search results into the Buku VUI buffer.
Parses JSON-STR into row data, computes the table column model from
`buku--list-headers`, stores the parsed rows in `buku-vui--rows`, and
either updates the existing VUI instance or mounts a new `vui-buku`
component in `buku--buffer-name`."
  (with-current-buffer (get-buffer-create buku--buffer-name)
    (buku-vui-list-mode 1)
    (let* ((columns
            (mapcar
             (lambda (item)
               (list
                :header (cadr item)
                :truncate t
                :width (buku--header-width item)))
             buku--list-headers))
           (json-data
            (buku--json-parse-string json-str))
           (rows
            (mapcar #'buku--hash-to-list json-data)))
      (setq buku-vui--rows rows)
      (if buku-vui--instance
          (vui-update-props buku-vui--instance (list :columns columns :rows rows))
        (setq-local buku-vui--instance (vui-mount (vui-component 'vui-buku :columns columns :rows rows)  buku--buffer-name))))))

(defun buku-vui--list-get-id ()
  "Return the numeric bookmark ID from the current line."
  (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
         (items (string-split line "│" t " +")))
    (string-to-number (car items))))

(defun buku-vui--list-get-entry ()
  "Return the bookmark row in `buku-vui--rows` whose ID matches the current line."
  (let* ((id (buku-vui--list-get-id)))
    (cl-find-if (lambda (row) (equal (string-to-number (car row)) id))
                buku-vui--rows)))

(defun buku-vui--add-advices ()
  "Install VUI overrides for Buku list rendering and entry lookup."
  (unless (advice-member-p #'buku-vui--list-render 'buku--list-render)
    (advice-add 'buku--list-render :override #'buku-vui--list-render))
  (unless (advice-member-p #'buku-vui--list-get-id 'buku--list-get-id)
    (advice-add 'buku--list-get-id :override #'buku-vui--list-get-id))
  (unless (advice-member-p #'buku-vui--list-get-entry 'buku--list-get-entry)
    (advice-add 'buku--list-get-entry :override #'buku-vui--list-get-entry)))

(defun buku-vui--remove-advices ()
  "Remove VUI overrides for Buku list rendering and entry lookup."
  (when (advice-member-p #'buku-vui--list-render 'buku--list-render)
    (advice-remove 'buku--list-render #'buku-vui--list-render))
  (when (advice-member-p #'buku-vui--list-get-id 'buku--list-get-id)
    (advice-remove 'buku--list-get-id #'buku-vui--list-get-id))
  (when (advice-member-p #'buku-vui--list-get-entry 'buku--list-get-entry)
    (advice-remove 'buku--list-get-entry #'buku-vui--list-get-entry)))

(define-minor-mode buku-vui-mode
  "Globally override `buku--list-render` with `buku-vui--list-render`."
  :global t
  :lighter " BukuVui"
  (if buku-vui-mode
      (buku-vui--add-advices)
    (buku-vui--remove-advices)))

(define-minor-mode buku-vui-list-mode
  "Minor mode for Buku VUI list buffers, binding RET to `buku-list-actions`."
  :global nil
  :lighter " BukuVuiList"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") #'buku-list-actions)
            map))

(provide 'buku-vui)
;;; buku-vui.el ends here
