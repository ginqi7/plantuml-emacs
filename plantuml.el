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
(defvar plantuml-cmd-template "cat <<EOF | java -jar %s -t%s -pipe > %s \n%s\nEOF")
(defvar plantuml-relative-path "./")
(defvar plantuml-mindmap-contains-org-content nil)
(defvar plantuml-theme "_none_")
(defvar plantuml-font nil)
(defvar plantuml-add-index-number nil)
(defvar plantuml-log-command nil)
(defvar plantuml-org-headline-bold nil)


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
          (if (and text-begin text-end)
            (string-trim
             (substring-no-properties
              (buffer-substring text-begin text-end))
             "\n" "\n")
            " "
            )
          ))
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


(defun plantuml--run-command (type content)
  "Run plantuml command.
TYPE is plantuml type.
CONTENT is source content."
  (plantuml--check-jar-path)
  (let* ((source (plantuml--build-source type content))
         (output-file (plantuml--build-output-file))
         (command
          (format plantuml-cmd-template plantuml-jar-path plantuml-output-type
                  output-file
                  source))
         (process
          (start-process-shell-command "plantuml" "plantuml" command)))
    (plantuml--log-command command)
    (process-put process 'output-file output-file)
    process))

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
  (let ((process
         (plantuml--run-command "mindmap" (plantuml--parse-headlines))))
    (set-process-sentinel process #'plantuml--open-ouput-file-sentinel)))

(defun plantuml-display-json-open ()
  "Convert json buffer to image and open it."
  (interactive)
  (let ((process
         (plantuml--run-command "json"
                                (substring-no-properties
                                 (buffer-substring
                                  (point-min)
                                  (point-max))))))
    (set-process-sentinel process #'plantuml--open-ouput-file-sentinel)))

(defun plantuml-display-yaml-open ()
  "Convert yaml buffer to image and open it."
  (interactive)
  (let ((process
         (plantuml--run-command "yaml"
                                (substring-no-properties
                                 (buffer-substring
                                  (point-min)
                                  (point-max))))))
    (set-process-sentinel process #'plantuml--open-ouput-file-sentinel)))

(defun plantuml--open-ouput-file-sentinel (process signal)
  "Define a sentinel, when process finish, open output file.
PROCESS is current process.
SIGNAL is current signal."
  (when (memq (process-status process) '(exit))
    (shell-command
     (format "open '%s'" (process-get process 'output-file)))))

(defun plantuml-org-to-wbs ()
  "Convert org file to Work Breakdown Structure image."
  (interactive)
  (plantuml--run-command "wbs" (plantuml--parse-headlines)))

(defun plantuml-org-to-wbs-open ()
  "Convert org file to Work Breakdown Structure image and open it."
  (interactive)
  (let ((process
         (plantuml--run-command "wbs" (plantuml--parse-headlines))))
    (set-process-sentinel process #'plantuml--open-ouput-file-sentinel)))

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

