;;; org-media-info.el --- Fetch information about media items -*- lexical-binding: t; -*-

;; Author: Florian Schroedl <flo.schroedl@gmail.com>
;; Url: https://github.com/floscr/org-media-info
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4") (dash "2.12") (org "9.1.9") (s "20180406.808") (json "1.4") (ivy "0.12.0") (om "1.2.0"))
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
(require 'om)

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

(defun counsel-org-media--update-org-item (x)
  "Query for a book and insert entry as org item."
  (interactive)
  (let* ((volume-info (alist-get 'volumeInfo x))
         (authors (-some->> (alist-get 'authors volume-info)
                    (s-join ", "))))
    (org-set-property "AUTHORS" authors)
    (org-set-property "AUTHORS" authors)))

(defun counsel-org-media-books (query)
  (interactive "sFetch Book: ")
  (ivy-read "Select Book: " (counsel-org-media--process-google-books-data query)
            :action #'counsel-org-media--update-org-item))

(defun counsel-org-media-books-german ()
  "Insert book from german db."
  (interactive)
  (let ((org-media-language "de"))
    (call-interactively #'counsel-org-media-books)))

(provide 'org-media-info)
;;; org-media-info.el ends here
