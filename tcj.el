;;; tcj.el --- The Comics Journal Archive Downloaderm -*- lexical-binding: t -*-
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

(defun tcj-download-issue (issue)
  "Download a Comics Journal issue and make a .cbr archive."
  (let ((url (format "http://www.tcj.com/archive-viewer-issue-%s/"
		     issue))
	(page 1))
    (while (and url
		(not (string-match "00001/$" url)))
      (with-current-buffer (url-retrieve-synchronously url)
	(goto-char (point-min))
	(when (re-search-forward "^$" nil t)
	  (let ((dom (libxml-parse-html-region (point) (point-max))))
	    (loop for next in (dom-by-class dom "ngg-browser-next")
		  for img = (dom-by-tag next 'img)
		  when img
		  do (tcj-download-image issue page (dom-attr img 'src))
		  (setq url (dom-attr next 'href)))))
	(incf page)
	(kill-buffer (current-buffer))))
    (tcj-create-cbr issue)))

(defun tcj-download-image (issue page url)
  (let ((file (format "%s/tcj-%03d-%03d.jpg" tcj-directory issue page)))
    (unless (file-exists-p file)
      (with-current-buffer (url-retrieve-synchronously url)
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

(provide 'tcj)

;;; tcj.el ends here
