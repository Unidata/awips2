;;; qpid-c++-mode.el --- Qpid specific c++-mode customizations.

;;
;; Licensed to the Apache Software Foundation (ASF) under one or more
;; contributor license agreements.  See the NOTICE file distributed
;; with this work for additional information regarding copyright
;; ownership.  The ASF licenses this file to you under the Apache
;; License, Version 2.0 (the * "License") ; you may not use this file
;; except in compliance with the License.  You may obtain a copy of
;; the License at
;; 
;;   http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.
;;

;;;=====================================================================
;;; Commentary:
;;
;; C++ customizations to make c++ mode follow the Qpid style guidelines,
;; along with some other handy functions to generate initial starting point
;; .h and .cpp files etc.
;;
;; I have this in my .emacs:
;; (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
;; (require 'qpid-c++-mode)
;; 
;; Written by Alan Conway: aconway@redhat.com
;;
;; For latest version, check
;;  http://svn.apache.org/repos/asf/incubator/qpid/trunk/qpid/cpp/etc/emacs/qpid-c++.el   
;;

(require 'cc-mode)

;; Increment the version number if you change this file.
(defconst qpid-c++-version "1.00" "Qpid C++ style support version number.")

(defun qpid-c++-version ()
  "Echo the current version of qpid-c++-mode in the minibuffer."
  (interactive)
  (message "Using qpid-c++-mode version %s" qpid-c++-version))

(defun qpid-c++-mode ()
  "Qpid C++ mode customizations"
  (c-add-style "qpid-c++"
	       '("gnu"
		 (indent-tabs-mode . nil)
		 (c-basic-offset . 4)
		 (c-offsets-alist .
				  ((statement-case-intro . *)
				   (statement-case-open . *)
				   (substatement-open . 0)
				   (case-label . *)
				   (access-label . /)
				   (friend . /)
				   (arglist-intro . +)
				   (arglist-cont . 0)
				   (arglist-close . 0)
				   (inline-open . 0)
				   (brace-list-open . 0)
				   (innamespace . 0)
				   ))) )
  (c-set-style "qpid-c++")
  (setq c-hungry-delete-key t)
  (setq c-tab-always-indent t)
  (setq c-hanging-braces-alist '((substatement-open . (after))
                                 (extern-lang-open . (after))
				 (defun-open . (after))
				 (class-open . (after))
				 (block-open . (after))

				 (inline-open . (after))
				 (defun-block-intro . (after))
				 (inclass . (after))
				 (topmost-intro . (after))

                                 (brace-list-open)
                                 (brace-list-close)
				 (namespace-open)
				 ))
  (setq c-hanging-colons-alist '((member-init-intro)
                                 (inher-intro)
                                 (case-label)
                                 (label)
                                 (access-label)))
  (setq mode-name "Qpid C++"))


(defun copyright ()
  (interactive)
  (insert "/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * \"License\"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */"))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun path-to-namespace (path)
  (replace-regexp-in-string "/" "::" (replace-regexp-in-string "/$" "" path)))

(defun src-subpath (path)
  (if (string-match "/src/\\(.*\\)$" path) (match-string 1 path) ""))

(defun namespace-for-file (file)
  (path-to-namespace (src-subpath (file-name-directory file))))

(defun cpp-guard-for-file (file)
  (upcase (replace-regexp-in-string "[/.-]" "_" (src-subpath file))))

(defun ask-for-namespace ()
  (read-from-minibuffer "Namespace: " (namespace-for-file (buffer-file-name))))

;;; Generate starting point code for new files

(defun insert-ns-open (namespaces)
    (mapcar (lambda (ns) (insert "namespace " ns " {\n")) namespaces))

(defun insert-ns-close (namespaces)
  (mapcar (lambda (ns) (insert "}")) namespaces)
  (insert " // namespace " (mapconcat 'identity namespaces "::") "\n"))

(defun ns-around-region (namespace)
  (interactive (list (ask-for-namespace)))
  (save-excursion
    (let ((namespaces (split-string namespace "::")))
      (if (< (mark) (point)) (exchange-point-and-mark))
      (insert "\n")
      (insert-ns-open namespaces)
      (goto-char (mark))
      (insert "\n")
      (insert-ns-close namespaces))))

(defun insert-class.h (class namespaces)
  "Insert class skeleton in .h file"
  (insert-ns-open namespaces)
  (insert "\n"
	  "/**\n     *\n     */\n"
	  "    class " class "\n"
	  "    {\n      public:\nprivate:\n};\n"
	  )
  (insert-ns-close namespaces))
  
(defun insert-platform.h (class namespaces)
  "Insert platform #include for platform class."
  (insert "#include <qpid/sys/platform.h>\n"
	  "#include QPID_PLATFORM_H(" class ".h)\n"))

(defun .h (namespace &optional content)
  "Initialize a .h file with Qpid copyright etc."
  (interactive (list (ask-for-namespace)))
  (copyright)
  (let ((content (or content  'insert-class.h))
	(class (file-name-nondirectory
		(file-name-sans-extension(buffer-file-name))))
	(namespaces (split-string namespace "::")))
    
    (insert "\n")
    (apply content class namespaces nil)
    (insert "\n"))
  (previous-line 1)
  (beginning-of-line)
  (indent-buffer)
  (save-excursion (cpp-guard)))

(defun .cpp (namespace)
  "Initialize an empty .cpp file with Qpid copyright etc."
  (interactive (list (ask-for-namespace)))
  (copyright)
  (insert "\n#include \"" (file-name-sans-extension
			   (file-name-nondirectory buffer-file-name))
	  ".h\"\n\n")
  (let ((namespaces (split-string namespace "::")))
    (insert-ns-open namespaces)
    (insert-ns-close namespaces))
  (indent-buffer))
						   
(defun cpp-guard ()
  "Insert C preprocessor macro guard to prevent file rescanning.
The guard macro is defined from the name of the immediate containing
directory and the name of the file."
  (interactive)
  (let ((name (cpp-guard-for-file (buffer-file-name))))
    (goto-char (point-min))
    (save-excursion
      (if (looking-at "#ifndef .*\n#define .*\n\n")
	  (let ((ifndef (match-data 0)))
	    (goto-char (point-max))
	    (previous-line 1)
	    (beginning-of-line)
	    (if (looking-at "#endif")
		(progn
		  (kill-line 1)
		  (kill-region (car ifndef) (cadr ifndef)))))))
    (insert "#ifndef " name "\n#define " name "\n\n")
    (goto-char (point-max))
    (beginning-of-line)
    (insert (format "#endif  /*!%s*/\n" name))))

(add-hook 'c++-mode-hook 'qpid-c++-mode)

(provide 'qpid-c++-mode)


