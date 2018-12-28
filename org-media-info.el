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
(require 'json)
(require 'ivy)
(require 'org)
(require 'ht)


(defstruct book title authors publisher published-date)

(defun org-media-info--extract-json-values (item)
  "Extract attribute from result of api.
ITEM is each book information."
  (let ((title "")
        (author "")
        (publisher "")
        (publishedDate ""))
    (dolist (i item)
      (when (string= "volumeInfo" (car i))
        (dolist (j (cdr i))
          (when (string= "title" (car j))
            (setq title (cdr j)))
          (when (string= "authors" (car j))
            (setq author (cdr j)))
          (when (string= "publisher" (car j))
            (setq publisher (cdr j)))
          (when (string= "publishedDate" (car j))
            (setq publishedDate (cdr j))))))

    (make-book
     :title title
     :authors author
     :publisher publisher
     :published-date publishedDate)))

(defun org-media-info--query-google (query)
  "Retrieve information of book using google books api.
QUERY for the search query"
  (switch-to-buffer
   (url-retrieve-synchronously
    (concat "https://www.googleapis.com/books/v1/volumes?q=" query)))
  (let ((response-string (buffer-substring-no-properties
                          url-http-end-of-headers (point-max))))
    (kill-buffer (current-buffer))
    (json-read-from-string (decode-coding-string response-string 'utf-8))))

(defun org-media-info--retrieve-info (query)
  "Retrieve information of book using google books api.
QUERY for the search query"
  (mapcar 'org-media-info--extract-json-values (cdr (nth 2 (org-media-info--query-google query)))))

(setq items (org-media-info--retrieve-info "the game"))

(defun my-action (cons-item)
  (let ((item (cdr cons-item)))
    (message "authors: %s" (format "%s" (book-authors item)))))

(message "%s" (mapconcat 'identity (list "1" "2" "3") ", "))

(mapconcat 'identity (book-authors (cdr lol)) ", ")

(ivy-read "lol" (--map (cons (book-title it) it) items)
          :action 'my-action)

;;; Test

(defun org-media--get-book-data (query)
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

(defun org-media--ivy-book (query &optional action)
  "Show results in ivy
QUERY for the search query")

(--reduce-from (-snoc acc (alist-get 'title it)) '() json-data)

(setq json-data (org-media--get-book-data "the game"))
