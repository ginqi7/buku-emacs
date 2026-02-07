;;; buku-ctable.el ---                               -*- lexical-binding: t; -*-

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
(require 'ctable)
(defun buku-ctable--list-render (json-str)
  "Renders a ctable list from the provided JSON string by building a table model from the parsed data, creating a ctable component, and displaying it in a buffer."
  (let* ((column-model ; column model
          (mapcar
           (lambda (item)
             (make-ctbl:cmodel
              :title
              (cadr item)
              :max-width
              (buku--header-width item)
              :align 'left))
           buku--list-headers))
         (json-data
          (buku--json-parse-string json-str))
         (data
          (mapcar #'buku--hash-to-list json-data))
         (model ; data model
          (make-ctbl:model
           :column-model column-model :data data))
         (component ; ctable component
          (ctbl:create-table-component-buffer
           :model model
           :buffer (get-buffer-create buku--buffer-name))))
    (ctbl:cp-add-click-hook
     component
     #'buku-list-actions)
    (pop-to-buffer (ctbl:cp-get-buffer component))))

(defun buku-ctable--list-get-id ()
  "Returns the ID of the current ctable list entry based on the position of buku--list-key in buku--list-headers."
  (let ((id-idx (cl-position buku--list-key
                             (mapcar #'car buku--list-headers)
                             :test #'equal)))
    (string-to-number (elt (buku-ctable--list-get-entry) id-idx))))

(defun buku-ctable--list-get-entry ()
  "Returns the currently selected data row from the ctable component as a vector."
  (vconcat (ctbl:cp-get-selected-data-row (ctbl:cp-get-component))))

(defun buku-ctable--add-advices ()
  (unless (advice-member-p #'buku-ctable--list-render 'buku--list-render)
    (advice-add 'buku--list-render :override #'buku-ctable--list-render))
  (unless (advice-member-p #'buku-ctable--list-get-id 'buku--list-get-id)
    (advice-add 'buku--list-get-id :override #'buku-ctable--list-get-id))
  (unless (advice-member-p #'buku-ctable--list-get-entry 'buku--list-get-entry)
    (advice-add 'buku--list-get-entry :override #'buku-ctable--list-get-entry)))

(defun buku-ctable--remove-advices ()
  (when (advice-member-p #'buku-ctable--list-render 'buku--list-render)
    (advice-remove 'buku--list-render #'buku-ctable--list-render))
  (when (advice-member-p #'buku-ctable--list-get-id 'buku--list-get-id)
    (advice-remove 'buku--list-get-id #'buku-ctable--list-get-id))
  (when (advice-member-p #'buku-ctable--list-get-entry 'buku--list-get-entry)
    (advice-remove 'buku--list-get-entry #'buku-ctable--list-get-entry)))

(define-minor-mode buku-ctable-mode
  "Globally override `buku--list-render` with `buku-ctable--list-render`."
  :global t
  :lighter " BukuCT"
  (if buku-ctable-mode
      (buku-ctable--add-advices)
    (buku-ctable--remove-advices)))

(provide 'buku-ctable)
;;; buku-ctable.el ends here
