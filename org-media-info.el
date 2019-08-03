;;; org-media-info.el --- Fetch information about media items -*- lexical-binding: t; -*-

;; Author: Florian Schroedl <flo.schroedl@gmail.com>
;; Url: https://github.com/floscr/org-media-info
;; Version: 1.0.0
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

(defun org-media--book-data (query)
  "Retrieve information of book using google books api.
Reduce json with jq, since it's way easier too manage.
QUERY for the search query"
  (let ((json-array-type 'list))
    (json-read-from-string
     (shell-command-to-string
      (concat
       "curl -s -G https://www.googleapis.com/books/v1/volumes"
       ;; Encode the query
       " --data-urlencode \"q=" query "\""
       " --data-urlencode \"langRestrict=" org-media-language "\""
       " | "
       "jq -r '.items | "
       "map({ "
          "id: .id, "
          "title: .volumeInfo.title, "
          "subtitle: .volumeInfo.subtitle, "
          "publishedDate: .volumeInfo.publishedDate, "
          "description: .volumeInfo.description, "
          "pageCount: .volumeInfo.pageCount, "
          "link: .volumeInfo.infoLink, "
          "authors: (.volumeInfo.authors | if length > 0 then . else [] end)"
       "})'")))))

(defun org-media--date-string-to-org-timestamp (str &optional active)
  "Convert string to org timestamp.
STR for the date string
ACTIVE for active timestamp"
  (--> str
       (org-read-date nil 'totime it)
       (let* ((org-user-format (cdr org-time-stamp-formats))
              (format (unless active (concat "[" (substring org-user-format 1 -1) "]"))))
         (format-time-string format it))))

(defun org-media--reduce-book-author-title (x)
  "Convert to string list in format AUTHOR: TITLE.
X for the cons list"
  (let ((title (alist-get 'title x))
        (authors (s-join ", " (alist-get 'authors x))))
    (if (string= "" authors)
        title
      (format "%s:\n%s\n" authors title))))

(defun org-media--ivy-book-data-cons (xs)
  "Convert to ivy cons with (entry . data)
XS for data"
  (--reduce-from (-snoc acc (cons (org-media--reduce-book-author-title it) it)) '() xs))

(defun org-media--insert-org-item (x)
  "Insert book as org item.
X for data"
  (org-insert-heading)
  (let* ((data (cdr x))
         (subtitle (if-let ((subtitle (alist-get 'subtitle data))) (concat " - " subtitle)))
         (title (concat (alist-get 'title data) subtitle))
         (authors (alist-get 'authors data)))
    (insert title)
    (if (not (null authors)) (org-set-property "AUTHORS" (s-join ", " authors)))
    (if-let ((description (alist-get 'description data))) (org-set-property "DESCRIPTION" description))
    (if-let ((pageCount (alist-get 'pageCount data))) (org-set-property "PAGES" (int-to-string pageCount)))
    (if-let ((publishedDate (alist-get 'publishedDate data)))
        (org-set-property "DATE_PUBLISHED" (org-media--date-string-to-org-timestamp publishedDate)))
    (org-set-property "DATE_ADDED" (org-media--date-string-to-org-timestamp (format-time-string "%Y-%m-%d")))
    (if-let ((link (alist-get 'link data))) (org-set-property "LINK" link))
    (org-beginning-of-line)))

(defun org-media--ivy-insert-book (query &optional action)
  "Show book titles in ivy.
QUERY for the search query
ACTION for an alternative action"
  (ivy-read "Book: " (org-media--ivy-book-data-cons (org-media--book-data query))
    :action 'org-media--insert-org-item))

(defun org-media-insert-book ()
  "Query for a book and insert entry as org item."
  (interactive)
  (let ((query (or (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
                   "")))
    (org-media--ivy-insert-book (read-string "Query: " query))))

(defun org-media-insert-book-german ()
  "Insert book from german db."
  (interactive)
  (let ((org-media-language "de"))
    (call-interactively #'org-media-insert-book)))

(provide 'org-media-info)
;;; org-media-info.el ends here
