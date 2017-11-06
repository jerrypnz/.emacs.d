;;; test-org-agenda.el --- Tests for org-agenda.el -*- lexical-binding: t ; -*-

;; Copyright (C) 2017 Marco Wahl

;; Author: Marco Wahl <marcowahlsoft@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;; Unit tests for Org Agenda.

;;; Code:

(require 'org-test)
(require 'org-agenda)


;; General auxilliaries

(defun org-test-agenda--agenda-buffers ()
  "Return agenda buffers in a list."
  (cl-remove-if-not (lambda (x)
		      (with-current-buffer x
			(eq major-mode 'org-agenda-mode)))
		    (buffer-list)))

(defun org-test-agenda--kill-all-agendas ()
  "Kill all agenda buffers."
  (mapc #'kill-buffer
	(org-test-agenda--agenda-buffers)))


;; Test the Agenda

(ert-deftest test-org-agenda/empty ()
  "Empty agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
        org-agenda-files)
    (org-agenda-list)
    (set-buffer org-agenda-buffer-name)
    (should (= 2 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/one-line ()
  "One informative line in the agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    (org-agenda-list nil  "<2017-03-10 Fri>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/scheduled-non-todo ()
  "One informative line in the agenda from scheduled non-todo-keyword-item."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org"
					       org-test-dir))))
    (org-agenda-list nil "<2017-07-19 Wed>")
    (set-buffer org-agenda-buffer-name)
    (should
     (progn (goto-line 3)
	    (looking-at " *agenda-file:Scheduled: *test agenda"))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-name ()
  "Agenda buffer name after having created one sticky agenda buffer."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (let ((org-agenda-span 'day)
	(buf (get-buffer org-agenda-buffer-name))
        org-agenda-files)
    (when buf (kill-buffer buf))
    (org-test-with-temp-text "<2017-03-17 Fri>"
      (org-follow-timestamp-link))	;creates a sticky agenda
    (org-test-agenda--kill-all-agendas)
    (org-agenda-list)
    (should (= 1 (length (org-test-agenda--agenda-buffers))))
    (should (string= "*Org Agenda*"
		     (buffer-name (car (org-test-agenda--agenda-buffers))))))
  (org-test-agenda--kill-all-agendas))

(ert-deftest test-org-agenda/sticky-agenda-name-after-reload ()
  "Agenda buffer name of sticky agenda after reload."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (org-test-agenda--agenda-buffers))
	     nil "precondition violation")
  (org-toggle-sticky-agenda)
  (let (org-agenda-files)
    (org-agenda-list)
    (let* ((agenda-buffer-name
	    (progn
	      (assert (= 1 (length (org-test-agenda--agenda-buffers))))
	      (buffer-name (car (org-test-agenda--agenda-buffers))))))
      (set-buffer agenda-buffer-name)
      (org-agenda-redo)
      (should (= 1 (length (org-test-agenda--agenda-buffers))))
      (should (string= agenda-buffer-name
                       (buffer-name (car (org-test-agenda--agenda-buffers)))))))
  (org-toggle-sticky-agenda)
  (org-test-agenda--kill-all-agendas))


(provide 'test-org-agenda)

;;; test-org-agenda.el ends here
