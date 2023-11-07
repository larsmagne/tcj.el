;;; tcj.el --- The Comics Journal Archive Downloader -*- lexical-binding: t -*-
;; Copyright (C) 2016 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun, network

;; tcj.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; tcj.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this, first log into the Comics Journal web site by saying,
;; for instance,
;; M-x eww RET http://www.tcj.com/archive-viewer-issue-199/ RET

;; type in your user name and password, and then break.  You should
;; now have a login cookie that will then be used by the commands below.

;;; Code:

(defvar tcj-directory "~/tcj/")

(defun tcj-issue-url (issue)
  (cond
   ((= issue 222)
    "http://www.tcj.com/archive-viewer-issue-222-4/")
   ((= issue 30)
    "http://www.tcj.com/the-new-nostalgia-journal-no-30/")
   ((= issue 27)
    "http://www.tcj.com/issue-27/")
   ((= issue 46)
    "http://www.tcj.com/archive-image-viewer-issue-46/")
   ((= issue 33)
    "http://www.tcj.com/archive-viewer-the-comics-journal-no-33-april-1977/")
   ((= issue 34)
    "http://www.tcj.com/archive-issue-34/")
   ((= issue 35)
    ;; Different format
    "http://www.tcj.com/archive-issue-35/")
   ((= issue 190)
    "http://www.tcj.com/archive-viewer-issue-190/")
   ((= issue 228)
    "http://www.tcj.com/archive-viewer-228/")
   ((= issue 205)
    "http://www.tcj.com/archive-viewer-205/")
   ((= issue 263)
    "http://www.tcj.com/archive-viewer-issue-262-2/")
   ((= issue 271)
    "http://www.tcj.com/archive-viewer-issue-271/")
   ((= issue 280)
    "http://www.tcj.com/archive-viewer-issue-280-2/")
   (t
    (format "http://www.tcj.com/archive-viewer-issue-%s/"
	    issue))))

(defun tcj-download-issue (issue)
  "Download a Comics Journal issue and make a .cbr archive."
  (let ((url (tcj-issue-url issue))
	(urls nil)
	(page 1))
    (while (and url
		(not (string-match "00001/$" url))
		(not (member url urls)))
      (push url urls)
      (with-current-buffer (url-retrieve-synchronously url t)
	(goto-char (point-min))
	(when (re-search-forward "^$" nil t)
	  (let ((dom (libxml-parse-html-region (point) (point-max))))
	    (loop for next in (dom-by-class dom "ngg-browser-next")
		  for img = (dom-by-tag next 'img)
		  for src = (dom-attr img 'src)
		  when (and src
			    (not (member src urls)))
		  do (tcj-download-image issue page src)
		  (push src urls)		  
		  (setq url (dom-attr next 'href)))))
	(incf page)
	(kill-buffer (current-buffer))))
    (tcj-create-cbr issue)))

(defun tcj-download-range (from to)
  (loop for issue from from upto to
	do (tcj-download-issue issue)))

(defun tcj-download-image (issue page url)
  (message "Downloading page %s from issue %s" page issue)
  (let ((file (format "%s/tcj-%03d-%03d.jpg" tcj-directory issue page)))
    (unless (file-exists-p file)
      (with-current-buffer (url-retrieve-synchronously url t)
	(goto-char (point-min))
	(when (re-search-forward "\n\n" nil t)
	  (write-region (point) (point-max) file))
	(kill-buffer (current-buffer))))))

(defun tcj-create-cbr (issue)
  (let ((default-directory tcj-directory))
    (apply 'call-process "rar" nil nil nil "a"
	   (format "tcj-%03d.cbr" issue)
	   (directory-files tcj-directory nil
			    (format "tcj-%03d-[0-9]+.jpg$" issue)))))

(defun tcj-find-url (issue)
  (with-current-buffer 
      (url-retrieve-synchronously
       (tcj-issue-url issue)
       t)
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (let* ((dom (libxml-parse-html-region (point) (point-max)))
	     (next (dom-by-class dom "ngg-browser-next")))
	(dom-attr next 'href)))))

(defun tcj-find-urls ()
  (loop for issue from 27 upto 300
	for url = (tcj-find-url issue)
	do (message "%s" issue)
	collect (cons issue url)))

;; "http://www.tcj.com/archive-viewer-issue-183/nggallery/image/1003063103-i00002/"

(defun tcj-create-url-cdb (map)
  (setq map (loop for (issue . url) in map
		  when url
		  collect (cons issue
				(and (string-match
				      "/image/\\([0-9]+\\)-i\\([0-9]+\\)" url)
				     (cons (match-string 1 url)
					   (length (match-string 2 url)))))))
  (with-temp-buffer
    (dolist (file (directory-files "~/tcj/text/" nil "\\.txt\\'"))
      (when (string-match "tcj-\\([0-9]+\\)-\\([0-9]+\\).txt" file)
	(let ((issue (string-to-number (match-string 1 file)))
	      (page (string-to-number (match-string 2 file))))
	  (insert (format (format "/%%s %%snggallery/image/%%s-i%%0%dd/\n"
				  (or (cdr (cdr (assq issue map))) 5))
			  file
			  (tcj-issue-url issue)
			  (car (cdr (assq issue map)))
			  page)))))
    (write-region (point-min) (point-max) "/tmp/url.input")))

(defun tcj-find-issuu-urls ()
  (cl-loop for year from 1976 upto 2009
	   append
	   (with-current-buffer (url-retrieve-synchronously
				 (format "https://www.tcj.com/tcj-archive/%d/"
					 year))
	     (goto-char (point-min))
	     (search-forward "\n\n")
	     (let ((dom (libxml-parse-html-region (point) (point-max))))
	       (kill-buffer (current-buffer))
	       (cl-loop for issue in (dom-by-tag dom 'article)
			collect (dom-attr (dom-by-tag issue 'a) 'href))))))

(defun tcj-map-issuu-urls (issues)
  (cl-loop for url in issues
	   collect (cons (and (string-match "-\\([0-9]+\\)-.+$" url)
			      (match-string 1 url))
			 url)))
  

(provide 'tcj)

;;; tcj.el ends here
