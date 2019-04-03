;;; org-media-info ---  Org Mode -*- lexical-binding: t; -*-

;; Author: Florian Schroedl <flo.schroedl@gmail.com>
;; Url: https://github.com/floscr/org-media-info
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (dash "2.12") (f "0.18.1") (helm "1.9.4") (org "9.1.9") (helm-org-rifle "1.7.0-pre"))
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

(defun org-media--book-data (query)
  "Retrieve information of book using google books api.
Reduce json with jq, since it's way easier too manage.
QUERY for the search query"
  (let ((json-array-type 'list))
    (json-read-from-string
     (shell-command-to-string
      (concat
       "curl -s https://www.googleapis.com/books/v1/volumes\\?q\\="
       query
       " | "
       "jq -r '.items | "
       "map({ "
          "id: .id, "
          "title: .volumeInfo.title, "
          "authors: (.volumeInfo.authors | if length > 0 then . else [] end)"
       "})'")))))

(defun org-media--reduce-book-author-title (x)
  "Convert to string list in format AUTHOR: TITLE
X for the cons list"
  (let ((title (alist-get 'title x))
        (authors (s-join ", " (alist-get 'authors x))))
    (if (string= "" authors)
        title
      (format "%s:\n%s\n" authors title))))

(defun org-media--ivy-book-data-cons (xs)
  (--reduce-from (-snoc acc (cons (org-media--reduce-book-author-title it) it)) '() xs))

(org-media--ivy-book-data-cons json-data)

(defun org-media-insert-book (&optional action)
  "Show book titles in ivy.
QUERY for the search query
ACTION for an alternative action"
  (interactive)
  (ivy-read "Book: " (org-media--ivy-book-data-cons json-data)))

(org-media-insert-book)


(setq json-data (org-media--ivy-book-data-cons "the game"))
