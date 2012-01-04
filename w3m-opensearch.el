;;; w3m-opensearch.el --- 

;; Copyright (C) 2012  Hironori OKAMOTO

;; Author: Hironori OKAMOTO <k.ten87@gmail.com>
;; Keywords: hypermedia

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

;; 

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m)
(require 'xml)

(defgroup w3m-opensearch nil
  ""
  :group 'w3m
  :prefix 'w3m-opensearch-)

(defcustom w3m-opensearch-directory
  (expand-file-name "opensearch" w3m-profile-directory)
  "OpenSearch Description ファイル の場所。"
  :group 'w3m-opensearch
  :type 'directory)

(defun w3m-opensearch-parse-template
    (template Language InputEncoding OutputEncoding)
  (setq Language (or Language
		    "*")
	InputEncoding (or InputEncoding
			   "UTF-8")
	OutputEncoding (or OutputEncoding
			    "UTF-8")
	template (replace-regexp-in-string (rx "{searchTerms}")
					   "%s"
					   template))
  (dolist (parameter '(Language InputEncoding OutputEncoding) template)
	 (setq template
	       (replace-regexp-in-string (rx "{"
					     (eval (symbol-name parameter))
					     "}")
					 (symbol-value parameter)
					 template))))

(defun w3m-opensearch-parse-file (file)
  "OpenSearch Description ファイルをパースして、 w3m-search-engine-alist形式の alist を返す。"
  (loop with xml = (car (xml-parse-file file))
     with language = (car (xml-node-children
			   (car (xml-get-children xml 'Language))))
     with input-encoding = (car (xml-node-children
				 (car (xml-get-children xml 'InputEncoding))))
     with output-encoding = (car (xml-node-children
				  (car (xml-get-children xml 'OutputEncoding))))
     with engine = (car (xml-node-children
			 (car (xml-get-children xml 'ShortName))))
     ;; w3m-search-engine-alist の coding は coding-system-alist と
     ;; IANA Character Set Assignments との対応を取らないと駄目なはず。
     with coding = (intern (downcase (or input-encoding
					 "utf-8")))
     for url in (xml-get-children xml 'Url)
     when (and (string= (xml-get-attribute url 'type) "text/html")
	       ;; POST method はまだ未対応。
	       (if (xml-get-attribute-or-nil url 'method)
		   (string= (xml-get-attribute url 'method) "GET")
		   t))
     collect (list engine
		   (w3m-opensearch-parse-template (xml-get-attribute url
								     'template)
						  language
						  input-encoding
						  output-encoding)
		   coding)))

;;;###autoload
(defun w3m-opensearch ()
  (loop for file in (directory-files w3m-opensearch-directory
				     'full
				     (rx ".xml" eol))
       append (w3m-opensearch-parse-file file)))

(provide 'w3m-opensearch)
;;; w3m-opensearch.el ends here
