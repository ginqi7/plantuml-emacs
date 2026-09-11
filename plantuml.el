;;; plantuml.el --- PlantUML integration   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; URL: https://github.com/ginqi7/plantuml-emacs
;; Keywords: lisp, tools
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

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
;; This package provides PlantUML integration for Emacs, allowing you to
;; convert org files, JSON, and YAML to PlantUML diagrams (mindmaps, WBS, etc.).

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `plantuml-auto-convert'
;;    Automatically convert based on current buffer major mode.
;;  `plantuml-org-to-mindmap'
;;    Convert org file to mindmap image.
;;  `plantuml-org-to-wbs'
;;    Convert org file to Work Breakdown Structure image.
;;  `plantuml-display-json'
;;    Convert json buffer to image.
;;  `plantuml-display-yaml'
;;    Convert yaml buffer to image.
;;  `plantuml-transient'
;;    Transient prefix for plantuml commands.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:
(require 'org-element)
(require 'org-num)
(require 'subr-x) ;; for when-let
(require 'transient)

(defcustom plantuml-java-executable (executable-find "java")
  "Path to Java executable."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-jar-path nil
  "Path to PlantUML JAR file."
  :type 'file
  :group 'plantuml)

(defcustom plantuml-output-type "svg"
  "Output type for PlantUML images (e.g., svg, png)."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-cmd-template "%s -jar %s -t%s %s; mv %s %s"
  "Shell command template for running PlantUML."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-relative-path "./"
  "Relative path for saving PlantUML output files."
  :type 'directory
  :group 'plantuml)

(defcustom plantuml-mindmap-contains-org-content nil
  "Non-nil means include org content in mindmap nodes."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-theme "_none_"
  "PlantUML theme to use."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-font nil
  "Font name for PlantUML diagrams."
  :type '(choice (const nil) string)
  :group 'plantuml)

(defcustom plantuml-add-index-number nil
  "Non-nil means add index numbers to mindmap nodes."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-log-command nil
  "Non-nil means log PlantUML commands before execution."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-org-headline-bold nil
  "Non-nil means make org headlines bold in mindmap."
  :type 'boolean
  :group 'plantuml)

(defun plantuml--headline-first-paragraph (headline)
  "Get the first paragraph from HEADLINE's section content."
  (org-element-map
      (seq-find
       (lambda (elem)
         (eq (org-element-type elem) 'section))
       (org-element-contents headline))
      'paragraph
    #'identity
    nil
    'first-match
    'no-recursion))

(defun plantuml--headline-txt (headline)
  "Parse org headline obj to plantuml text.
HEADLINE org headline obj."
  (let* ((stars)
         (paragraph (plantuml--headline-first-paragraph headline))
         (text-begin (org-element-property :contents-begin paragraph))
         (text-end (org-element-property :contents-end paragraph))
         (text
          (if (and text-begin text-end)
              (string-trim
               (substring-no-properties
                (buffer-substring text-begin text-end))
               "\n" "\n")
            " ")))
    (setq stars (make-string (org-element-property :level headline) ?*))
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
  (file-name-with-extension
   (file-name-concat
    (file-truename plantuml-relative-path)
    (file-name-base (buffer-file-name)))
   plantuml-output-type))

(defun plantuml--build-source-file (type content)
  "Build plantuml source.
TYPE is plantuml supported type.
CONTENT is plantuml core content."
  (let ((source-file (make-temp-file "plantuml-emacs-" nil (concat "." type))))
    (with-temp-file source-file
      (insert
       (concat
        (format "@start%s \n" type)
        (format "!theme %s \n" plantuml-theme)
        (when plantuml-font
          (format "skinparam defaultFontName %s\n" plantuml-font))
        content
        (format "\n@end%s" type))))
    source-file))

(defun plantuml--check-jar-path ()
  "Check if `plantuml-jar-path' is specified by user."
  (when (not plantuml-jar-path)
    (throw 'plantuml-error "Must specify 'plantuml-jar-path'"))
  (when (not (file-exists-p plantuml-jar-path))
    (throw 'plantuml-error (format "plantuml-jar-path(%s) not exist." plantuml-jar-path))))

(defun plantuml--log-command (command)
  "Log COMMAND if user specified `plantuml-log-command'."
  (when plantuml-log-command (print command)))

(defun plantuml--run-command (type content &optional callback)
  "Run plantuml command.
TYPE is plantuml type.
CONTENT is source content.
CALLBACK is an optional function called with the output file path when done."
  (plantuml--check-jar-path)
  (let* ((source-file (plantuml--build-source-file type content))
         (output-file (plantuml--build-output-file))
         (command
          (format plantuml-cmd-template
                  plantuml-java-executable
                  plantuml-jar-path plantuml-output-type
                  source-file
                  (file-name-with-extension
                   (file-name-sans-extension source-file)
                   plantuml-output-type)
                  output-file))
         (process
          (start-process-shell-command "plantuml" "plantuml" command)))
    (plantuml--log-command command)
    (process-put process 'output-file output-file)
    (set-process-sentinel
     process
     (lambda (_proc _)
       (when (memq (process-status process) '(exit))
         (message (format "PlantUML Convert Finished to %s" (process-get process 'output-file)))
         (when callback
           (funcall callback (process-get process 'output-file))))))
    process))

(defun plantuml-org-to-mindmap (&optional callback)
  "Convert org file to mindmap image.
CALLBACK is an optional function called with the output file path when done."
  (interactive)
  (plantuml--run-command "mindmap" (plantuml--parse-headlines) callback))

(defun plantuml-display-json (&optional callback)
  "Convert json buffer to image.
CALLBACK is an optional function called with the output file path when done."
  (interactive)
  (plantuml--run-command "json" (buffer-string) callback))

(defun plantuml-display-yaml (&optional callback)
  "Convert yaml buffer to image.
CALLBACK is an optional function called with the output file path when done."
  (interactive)
  (plantuml--run-command "yaml" (buffer-string) callback))

(defun plantuml-org-to-wbs (&optional callback)
  "Convert org file to Work Breakdown Structure image.
CALLBACK is an optional function called with the output file path when done."
  (interactive)
  (plantuml--run-command "wbs" (plantuml--parse-headlines) callback))

(defun plantuml--auto-convert-org (&optional callback)
  "Select a type to convert.
CALLBACK is an optional function called with the output file path when done."
  (let ((selected-item
         (completing-read "Please choose a plantuml type you will convert"
                          '("Mind Map" "Work Breakdown Structure"))))
    (cond
     ((string= "Mind Map" selected-item)
      (plantuml-org-to-mindmap callback))
     ((string= "Work Breakdown Structure" selected-item)
      (plantuml-org-to-wbs callback)))))

(defun plantuml-auto-convert (&optional callback)
  "Depend on current buffer major mode to convert image.
CALLBACK is an optional function called with the output file path when done."
  (interactive)
  (cond
   ((member major-mode '(json-ts-mode js-json-mode json-mode))
    (plantuml-display-json callback))
   ((member major-mode '(yaml-mode yaml-ts-mode))
    (plantuml-display-yaml callback))
   ((eq major-mode #'org-mode)
    (plantuml--auto-convert-org callback))
   (t
    (throw 'plantuml-error (format "not suport %s file" major-mode)))))

(defun plantuml--transient-arguments-parse (args)
  "Parse transient arguments for plantuml commands.
ARGS is the transient arguments list."
  (pcase (car args)
    ("--browser" #'browse-url)
    ("--find-file" #'find-file)))

(transient-define-suffix plantuml-auto-convert-suffix (args)
  "Auto convert with transient args."
  (interactive (list (transient-args 'plantuml-transient)))
  (plantuml-auto-convert (plantuml--transient-arguments-parse args)))

(transient-define-suffix plantuml-org-to-mindmap-suffix (args)
  "Org to mindmap with transient args."
  (interactive (list (transient-args 'plantuml-transient)))
  (plantuml-org-to-mindmap (plantuml--transient-arguments-parse args)))

(transient-define-suffix plantuml-org-to-wbs-suffix (args)
  "Org to WBS with transient args."
  (interactive (list (transient-args 'plantuml-transient)))
  (plantuml-org-to-wbs (plantuml--transient-arguments-parse args)))

(transient-define-suffix plantuml-display-yaml-suffix (args)
  "Display YAML with transient args."
  (interactive (list (transient-args 'plantuml-transient)))
  (plantuml-display-yaml (plantuml--transient-arguments-parse args)))

(transient-define-suffix plantuml-display-json-suffix (args)
  "Display JSON with transient args."
  (interactive (list (transient-args 'plantuml-transient)))
  (plantuml-display-json (plantuml--transient-arguments-parse args)))

(transient-define-prefix plantuml-transient ()
  "PlantUML Commands."
  ["Parameters"
   ("-b" "Open with browser" "--browser")
   ("-o" "Open with `find-file'" "--find-file")]
  ["Commands"
   ("a" "Auto Convert" plantuml-auto-convert-suffix)
   ("om" "Org to MindMap" plantuml-org-to-mindmap-suffix)
   ("ow" "Org to Work breakdown structure" plantuml-org-to-wbs-suffix)
   ("y" "Display YAML" plantuml-display-yaml-suffix)
   ("j" "Display JSON" plantuml-display-json-suffix)])

(provide 'plantuml)
;;; plantuml.el ends here
