;;; plantuml.el --- A plantuml plugin for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp, tools

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

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `plantuml--parse-headlines'
;;    Parse all headlines in current buffer (of org mode).
;;  `plantuml-org-to-mindmap'
;;    Convert org file to mindmap image.
;;  `plantuml-display-json'
;;    Convert json buffer to image.
;;  `plantuml-display-yaml'
;;    Convert yaml buffer to image.
;;  `plantuml-org-to-mindmap-open'
;;    Convert org file to mindmap image and open it.
;;  `plantuml-display-json-open'
;;    Convert json buffer to image and open it.
;;  `plantuml-display-yaml-open'
;;    Convert yaml buffer to image and open it.
;;  `plantuml-org-to-wbs'
;;    Convert org file to Work Breakdown Structure image.
;;  `plantuml-org-to-wbs-open'
;;    Convert org file to Work Breakdown Structure image and open it.
;;  `plantuml-auto-convert-open'
;;    Dependen current buffer major mode convert image.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:
(require 'org-element)
(require 'org-num)
(require 'subr-x) ;; for when-let

(defvar plantuml-jar-path nil)
(defvar plantuml-output-type "svg")
(defvar plantuml-cmd-template "java -jar %s -charset UTF-8 -t%s -o %s %s")
(defvar plantuml-relative-path "./")
(defvar plantuml-mindmap-contains-org-content nil)
(defvar plantuml-theme "_none_")
(defvar plantuml-font nil)
(defvar plantuml-add-index-number nil)
(defvar plantuml-log-command nil)
(defvar plantuml-org-headline-bold nil)
(defvar plantuml-cache-file (expand-file-name (locate-user-emacs-file "plantuml/tmp.plantuml")))

(defun plantuml--headline-txt (headline)
  "Parse org headline obj to plantuml text.
HEADLINE org headline obj."
  (let* ((stars)
         (paragraph
          (org-element-map
              headline
              'paragraph
            #'identity
            nil
            'first-match
            'no-recursion))

         (text-begin (org-element-property :contents-begin paragraph))
         (text-end (org-element-property :contents-end paragraph))
         (text
          (string-trim
           (substring-no-properties
            (buffer-substring text-begin text-end))
           "\n" "\n")))
    (dotimes (i (org-element-property :level headline))
      (setq stars (concat stars "*")))
    (concat
     stars
     (when plantuml-mindmap-contains-org-content ":")
     " "
     (when plantuml-org-headline-bold "<b>")
     (when plantuml-add-index-number
       (concat
        (number-to-string
         (car
          (org-num--current-numbering
           (org-element-property :level headline)
           nil)))
        ". "))
     (org-element-property :raw-value headline)
     (when plantuml-org-headline-bold "</b>")
     (when plantuml-mindmap-contains-org-content
       (concat ":\n\n" text ";")))))

(defun plantuml--parse-headlines ()
  "Parse all headlines in current buffer (of org mode)."
  (interactive)
  (setq org-num--numbering nil)
  (seq-reduce
   (lambda (a b) (concat a "\n" b))
   (org-element-map
       (org-element-parse-buffer)
       'headline
     (lambda (x) (plantuml--headline-txt x)))
   nil))

(defun plantuml--build-output-file ()
  "Building plantuml ouput file."
  (when (not (file-exists-p plantuml-relative-path))
    (when (y-or-n-p
           (format "Directory \"%s\" not exists, do you want create it ?"
                   (file-truename plantuml-relative-path)))
      (make-directory (file-truename plantuml-relative-path))))
  (format "%s%s.%s"
          (file-truename plantuml-relative-path)
          (file-name-sans-extension (buffer-name))
          plantuml-output-type))

(defun plantuml--build-source (type content)
  "Build plantuml source.
TYPE is plantuml supported type.
CONTENT is plantuml core content."
  (concat
   (format "@start%s \n" type)
   (format "!theme %s \n" plantuml-theme)
   (when plantuml-font
     (format "skinparam defaultFontName %s\n" plantuml-font))
   content
   (format "\n@end%s" type)))

(defun plantuml--check-jar-path ()
  "Check if 'plantuml-jar-path' is specified by user."
  (when (not plantuml-jar-path)
    (throw 'plantuml-error "Must specify 'plantuml-jar-path'")))

(defun plantuml--log-command (command)
  "Log COMMAND if user specified 'plantuml-log-command'."
  (when plantuml-log-command (print command)))

(defun plantuml--gen-plantuml-cache-file (src)
  (unless (f-directory-p (file-name-directory plantuml-cache-file))
    (f-mkdir (file-name-directory plantuml-cache-file)))
  ;; (message "plantuml--gen-plantuml-cache-file")
  (f-write-text src 'utf-8 plantuml-cache-file))

(defun plantuml--run-command (type content)
  "Run plantuml command.
TYPE is plantuml type.
CONTENT is source content."
  (plantuml--check-jar-path)
  (let* ((source (plantuml--build-source type content))
         (output-file (plantuml--build-output-file))
         (command
          (format plantuml-cmd-template plantuml-jar-path plantuml-output-type (file-name-directory plantuml-cache-file) plantuml-cache-file))
         result
         tmp-file)
    (plantuml--gen-plantuml-cache-file source)
    (plantuml--log-command command)
    (setq result (shell-command-to-string command))
    (if (string-equal result "")
        (progn
          (setq tmp-file (file-name-concat (file-name-directory plantuml-cache-file) (concat (file-name-base plantuml-cache-file) "." (file-name-extension output-file))))
          (copy-file tmp-file output-file t)
          (message " %s generate success " output-file)
          output-file)
      (message result)
      nil)))

(defun plantuml-org-to-mindmap ()
  "Convert org file to mindmap image."
  (interactive)
  (plantuml--run-command "mindmap" (plantuml--parse-headlines)))

(defun plantuml-display-json ()
  "Convert json buffer to image."
  (interactive)
  (plantuml--run-command "json"
                         (substring-no-properties
                          (buffer-substring (point-min) (point-max)))))

(defun plantuml-display-yaml ()
  "Convert yaml buffer to image."
  (interactive)
  (plantuml--run-command "yaml"
                         (substring-no-properties
                          (buffer-substring (point-min) (point-max)))))


(defun plantuml-org-to-mindmap-open ()
  "Convert org file to mindmap image and open it."
  (interactive)
      (plantuml--open-ouput-file (plantuml--run-command "mindmap" (plantuml--parse-headlines))))

(defun plantuml-display-json-open ()
  "Convert json buffer to image and open it."
  (interactive)
  (plantuml--open-ouput-file (plantuml--run-command "json"
                                (substring-no-properties
                                 (buffer-substring
                                  (point-min)
                                  (point-max))))))

(defun plantuml-display-yaml-open ()
  "Convert yaml buffer to image and open it."
  (interactive)
  (plantuml--open-ouput-file (plantuml--run-command "yaml"
                                (substring-no-properties
                                 (buffer-substring
                                  (point-min)
                                  (point-max))))))

(defun plantuml--open-ouput-file (file-name)
  ""
  (when file-name
    ;; (shell-command (format "open '%s'" file-name))
    ;; (find-file file-name)
    (browse-url file-name)))

(defun plantuml-org-to-wbs ()
  "Convert org file to Work Breakdown Structure image."
  (interactive)
  (plantuml--run-command "wbs" (plantuml--parse-headlines)))

(defun plantuml-org-to-wbs-open ()
  "Convert org file to Work Breakdown Structure image and open it."
  (interactive)
  (plantuml--open-ouput-file (plantuml--run-command "wbs" (plantuml--parse-headlines))))

(defun plantuml--auto-convert-org-open ()
  "Select a type will convert."
  (let ((selected-item
         (completing-read "Please choose a plantuml type you will convert"
                          '("Mind Map" "Work Breakdown Structure"))))
    (cond
     ((string= "Mind Map" selected-item)
      (plantuml-org-to-mindmap-open))
     ((string= "Work Breakdown Structure" selected-item)
      (plantuml-org-to-wbs-open)))))

(defun plantuml-auto-convert-open ()
  "Dependen current buffer major mode convert image."
  (interactive)
  (cond
   ((eq major-mode #'json-mode)
    (plantuml-display-json-open))
   ((eq major-mode #'yaml-mode)
    (plantuml-display-yaml-open))
   ((eq major-mode #'org-mode)
    (plantuml--auto-convert-org-open))
   (t
    (throw 'plantuml-error (format "not suport %s file" major-mode)))))

(provide 'plantuml)
;;; plantuml.el ends here

