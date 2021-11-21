;;; file-template.el --- file template system  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  kotik

;; Author: kotik <kotik@kotik-one>
;; Keywords: convenience, tools

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

(require 'cl-lib)

(cl-defstruct ft:template name node)

(cl-defstruct ft:directory-node get-name children)

(cl-defstruct ft:file-node get-name content)

(cl-defstruct ft:snippet-content name mode)

(defun ft:register-template (sym template)
  (puthash sym template ft:templates-table))

(defun ft:expand-template (instance-name template)
  (let ((node (ft:template-node template)))
    (cond ((ft:directory-node-p node)
           (ft:expand-directory-node instance-name default-directory node))
          (t (error "unexpected template node %s" node)))))

(defun ft:expand-directory-node (instance-name parent-directory node)
  (let* ((directory-name (funcall (ft:directory-node-get-name node) instance-name))
         (directory-path (expand-file-name directory-name parent-directory)))
    (make-directory directory-path)
    (mapc (lambda (child-node)
            (cond ((ft:file-node-p child-node)
                   (ft:expand-file-node instance-name directory-path child-node))
                  ((ft:directory-node-p child-node)
                   (ft:expand-directory-node instance-name directory-path child-node))))
          (ft:directory-node-children node))))

(defun ft:expand-file-node (instance-name directory node)
  (let* ((file-name (funcall (ft:file-node-get-name node) instance-name))
         (content (ft:file-node-content node))
         (file-buffer (find-file-noselect (expand-file-name file-name directory))))
    (cond ((ft:snippet-content-p content)
           (let ((snippet (yas-lookup-snippet (ft:snippet-content-name content)
                                              (ft:snippet-content-mode content))))
             (with-current-buffer file-buffer
               (yas-expand-snippet snippet)
               (save-buffer)))))))


(defun ft:make-template-expander (template)
  (lambda ()
    (interactive)
    (let ((instance-name (read-string (format "Instance name for %s: " (ft:template-name template)))))
      (when (not (string= instance-name ""))
        (ft:expand-template instance-name template)))))

(provide 'file-template)
;;; file-template.el ends here
