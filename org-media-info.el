;;; org-media-info.el --- Fetch information about media items -*- lexical-binding: t; -*-

;; Author: Florian Schroedl <flo.schroedl@gmail.com>
;; Url: https://github.com/floscr/org-media-info
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4") (dash "2.12") (org "9.1.9") (s "20180406.808") (json "1.4") (ivy "0.12.0"))
;; Keywords: bookmaking

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Manage books in org mode

;;; Code:

;;;; Require

(require 'dash)
(require 's)
(require 'json)
(require 'ivy)
(require 'org)

;;; Main

(defcustom org-media-language "en" "Language for the results.")

(defun counsel-org-media--request-google-books-data (query)
  "Curl QUERY agains google books api."
  (shell-command-to-string
   (concat
    "curl -s -G https://www.googleapis.com/books/v1/volumes"
    ;; Encode the query
    " --data-urlencode \"q=" query "\""
    " --data-urlencode \"langRestrict=" org-media-language "\"")))

(defun counsel-org-media--process-google-books-data (query)
  "Process QUERY and return items."
  (-some->> (counsel-org-media--request-google-books-data query)
    (json-read-from-string)
    (alist-get 'items)
    (-map (lambda (x) (let* ((volume-info (alist-get 'volumeInfo x))
                             (authors (-some->> (alist-get 'authors volume-info)
                                        (s-join ", ")))
                             (title (alist-get 'title volume-info))
                             (published-date (alist-get 'publishedDate volume-info))
                             (ivy-string (concat "(" published-date ") " title
                                                 "\n" authors
                                                 "\n")))
                        (cons ivy-string x))))))

(defun counsel-org-media--date-string-to-org-timestamp (str &optional active)
  "Convert string to org timestamp.
STR for the date string
ACTIVE for active timestamp"
  (--> (org-read-date nil 'totime str)
       (let* ((org-user-format (cdr org-time-stamp-formats))
              (format (unless active (concat "[" (substring org-user-format 1 -1) "]"))))
         (format-time-string format it))))

(defun counsel-org-media--update-org-item--set-property-alist (property key alist)
  "Set org PROPERTY when KEY in ALIST exists."
  (-some->> (alist-get key alist)
    (format "%s")
    (org-set-property property)))

(defun counsel-org-media--update-org-item--set-property (property value)
  "Set org PROPERTY when VALUE exists."
  (-some->> value
    (format "%s")
    (org-set-property property)))

(defun counsel-org-media--update-org-item (data)
  "Update current org headline properties with DATA."
  (interactive)
  (let* ((volume-info (alist-get 'volumeInfo data)))
    (->> (format-time-string "%Y-%m-%d")
         (counsel-org-media--date-string-to-org-timestamp)
         (org-set-property "CREATED"))
    (-some->> (alist-get 'authors volume-info)
      (s-join ", ")
      (counsel-org-media--update-org-item--set-property "AUTHORS"))
    (-some->> (alist-get 'authors volume-info)
      (s-join ", ")
      (counsel-org-media--update-org-item--set-property "AUTHORS"))
    (counsel-org-media--update-org-item--set-property-alist "DESCRIPTION" 'description volume-info)
    (-some->> (alist-get 'publishedDate volume-info)
      (counsel-org-media--date-string-to-org-timestamp)
      (counsel-org-media--update-org-item--set-property "PUBLISHED"))
    (counsel-org-media--update-org-item--set-property-alist "PAGE_COUNT" 'pageCount volume-info)
    (counsel-org-media--update-org-item--set-property-alist "LINK" 'infoLink volume-info)
    (-some--> (alist-get 'imageLinks volume-info)
      (or (alist-get 'large it)
          (alist-get 'medium it)
          (alist-get 'small it)
          (alist-get 'thumbnail it)
          (alist-get 'smallThumbnail it))
      (counsel-org-media--update-org-item--set-property "COVER_IMAGE" it))))

(defun counsel-org-media-books (query)
  "Search the google books database for QUERY and update the current org headline."
  (interactive "sFetch Book: ")
  (ivy-read "Select Book: " (counsel-org-media--process-google-books-data query)
            :action #'counsel-org-media--update-org-item))

(defun counsel-org-media-books-on-entry ()
  "Search the google books database for the current headline and update it's properties."
  (interactive)
  (let ((query (-some->> (om-parse-this-headline)
                    (om-get-property :raw-value))))
    (counsel-org-media-books query)))

(defun counsel-org-media-books-german ()
  "Insert book from german db."
  (interactive)
  (let ((org-media-language "de"))
    (call-interactively #'counsel-org-media-books)))

(provide 'org-media-info)
;;; org-media-info.el ends here
