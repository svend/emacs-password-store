;;; password-store.el --- Password store (pass) support -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Svend Sorensen <svend@ciffer.net>

;; Author: Svend Sorensen <svend@ciffer.net>
;; Version: 0.1
;; Package-Requires: ((f "0.11.0") (s "1.9.0"))
;; Keywords: pass

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions for working with pass ("the
;; standard Unix password manager").
;;
;; http://www.zx2c4.com/projects/password-store/

;;; Code:

(require 'f)
(require 's)

(defvar password-store-kill-ring-pointer nil
  "The tail of of the kill ring ring whose car is the password.")

(defun password-store-dir ()
  "Return password store directory."
  (or (getenv "PASSWORD_STORE_DIR")
      "~/.password-store"))

(defun password-store-list (&optional subdir)
  "List password entries under SUBDIR."
  (unless subdir (setq subdir ""))
  (let ((dir (f-join (password-store-dir) subdir)))
    (if (f-directory? dir)
	(mapcar 'f-no-ext
		(mapcar (lambda (file) (f-relative file (password-store-dir)))
			(f-files dir (lambda (file) (equal (f-ext file) "gpg")) t))))))

;;;###autoload
(defun password-store-get (entry)
  "Return password for ENTRY.

Returns the first line of the password data."
  (car (s-lines (shell-command-to-string (concat "pass " entry)))))

;;;###autoload
(defun password-store-clear ()
  "Clear password in kill ring."
  (interactive)
  (if password-store-kill-ring-pointer
      (progn
	(setcar password-store-kill-ring-pointer "")
	(setq password-store-kill-ring-pointer nil)
	(message "Password cleared"))))

;;;###autoload
(defun password-store-copy (entry)
  "Add password for ENTRY to kill ring.

Clear previous password from kill ring.  Pointer to kill ring is
stored in `password-store-kill-ring-pointer'.  Password is cleared
after 45 seconds."
  (interactive (list (completing-read "Password entry: " (password-store-list))))
  (let ((password (password-store-get entry)))
    (password-store-clear)
    (kill-new password)
    (setq password-store-kill-ring-pointer kill-ring-yank-pointer)
    (run-at-time "45 sec" nil 'password-store-clear)))

;;;###autoload
(defun password-store-url (entry)
  "Browse URL stored in ENTRY.

This will only browse URLs that start with http:// or http:// to
avoid sending a password to the browser."
  (interactive (list (completing-read "Password entry: " (password-store-list))))
  (let ((url (password-store-get entry)))
    (if (or (string-prefix-p "http://" url)
	    (string-prefix-p "https://" url))
	(browse-url url)
      (message "Error: text does not look like a URL"))))

;;; password-store.el ends here
