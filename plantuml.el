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

(defun plantuml--build-source (content)
  "Build plantuml source.
CONTENT is plantuml core content."
  (concat "@startmindmap \n"
          (format "!theme %s \n" plantuml-theme)
          (when plantuml-font
            (format "skinparam defaultFontName %s\n" plantuml-font))
          (format "%s\n" content)
          "@endmindmap"))

(defun plantuml-org-to-mindmap ()
  "Convert org file to mindmap image."
  (interactive)
  (when (not plantuml-jar-path)
    (throw 'plantuml-error "Must specify 'plantuml-jar-path'"))
  (let* ((mindmap-str
          (plantuml--build-source (plantuml--parse-headlines)))
         (command
          (format plantuml-cmd-template plantuml-jar-path plantuml-output-type
                  (plantuml--build-output-file)
                  mindmap-str)))
    (when plantuml-log-command (print command))

    (start-process-shell-command "plantuml" "plantuml" command)))

(provide 'plantuml)
;;; plantuml.el ends here

