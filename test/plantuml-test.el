;;; plantuml-test.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

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

(require 'buttercup)
(require 'plantuml)

(describe "plantuml.el"
          (it "loads successfully"
              (expect (featurep 'plantuml) :to-be-truthy)))

(describe "plantuml-org"
          (it "org to mindmap"
              (let ((plantuml-jar-path "~/plantuml.jar")
                    (file-name "./example/just-headline.org")
                    (plantuml-log-command t))
                (with-current-buffer (find-file-noselect file-name)
                  (plantuml-org-to-mindmap)))
              (expect (featurep 'plantuml) :to-be-truthy))
          (it "org to wbs"
              (let ((plantuml-jar-path "~/plantuml.jar")
                    (file-name "./example/org-example.org")
                    (plantuml-log-command t))
                (with-current-buffer (find-file-noselect file-name)
                  (plantuml-org-to-wbs)))
              (expect (featurep 'plantuml) :to-be-truthy)))

(provide 'plantuml-test)
;;; plantuml-test.el ends here
