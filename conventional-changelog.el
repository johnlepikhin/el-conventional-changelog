;;; conventional-changelog --- Conventional changelogs generator

;;; Commentary:
;;; Inspired by https://github.com/conventional-changelog/standard-version

;;; Code:
(require 'seq)
(require 'subr-x)
(require 'org)

(defcustom conventional-changelog-file "Changelog.org" "The name of changelog file."
  :group 'ccl
  :type '(string))

(defcustom conventional-changelog-version-file "VERSION" "The name of file storing current verion."
  :group 'ccl
  :type '(string))

(defcustom conventional-changelog-top-heading "Changelog" "The name of top org mode heading."
  :group 'ccl
  :type '(string))

(defcustom conventional-changelog-items
  '(("BREAKING CHANGES" . (0 (lambda (elt) (gethash "breaking-changes" elt))))
    ("New features" . (1 (lambda (elt) (string= (gethash "type" elt "") "feat"))))
    ("Bugfixes" . (2 (lambda (elt) (string= (gethash "type" elt "") "fix"))))
    ("Other changes"
     . (2 (lambda (elt)
            (and
             (not (gethash "breaking-changes" elt))
             (not (string= (gethash "type" elt "") "feat"))
             (not (string= (gethash "type" elt "") "fix")))))))
  "Types of changes."
  :group 'ccl
  :type '(list))

(defun conventional-changelog-get-first-commit (working-directory)
  "Return last commit for conventional-changelog-file in WORKING-DIRECTORY."
  (shell-command-to-string
   (format "git -C %s log -n1 --pretty=format:%%H -- %s"
           (shell-quote-argument working-directory)
           (shell-quote-argument conventional-changelog-file))))

(defun conventional-changelog-get-changes-list-plain (first-commit working-directory)
  (let ((range
         (if (string= first-commit "") ""
           (format " %s..HEAD"
                   (shell-quote-argument first-commit)))))
    (shell-command-to-string
     (format "git -C %s log --pretty='format:%%h %%ae %%s'%s"
             (shell-quote-argument working-directory)
             range))))

(defun conventional-changelog-commit-parse-line (line)
  (save-match-data
    (and (string-match "^\\([^ ]+\\) \\([^ ]+\\) \\(.+\\)" line)
         (let ((tbl (make-hash-table :test 'equal)))
           (puthash "hash" (match-string 1 line) tbl)
           (puthash "email" (match-string 2 line) tbl)
           (puthash "subject" (match-string 3 line) tbl)
           tbl))))

(defun conventional-changelog-parse-subject (tbl)
  (let ((subject (gethash "subject" tbl)))
    (save-match-data
      (if (string-match "^\\([a-z]+\\)\\([!]*\\): +\\(.+\\)" subject)
          (progn
            (puthash "type" (match-string 1 subject) tbl)
            (puthash "breaking-changes" (string= "!" (match-string 2 subject)) tbl)
            (puthash "message" (match-string 3 subject) tbl))
        (if (string-match "^\\([a-z]+\\)\\([!]*\\)(\\([a-z]+\\)): +\\(.+\\)" subject)
            (progn
              (puthash "type" (match-string 1 subject) tbl)
              (puthash "breaking-changes" (string= "!" (match-string 2 subject)) tbl)
              (puthash "scope" (match-string 3 subject) tbl)
              (puthash "message" (match-string 4 subject) tbl))
          (progn
            (puthash "breaking-changes" 'nil tbl)
            (puthash "message" subject tbl)))))
    tbl))

(defun conventional-changelog-get-changes-list (first-commit &optional working-directory)
  (let* ((git-output (conventional-changelog-get-changes-list-plain first-commit working-directory))
         (lines (split-string git-output "[\n]+" t)))
    (mapcar
     'conventional-changelog-parse-subject
     (mapcar 'conventional-changelog-commit-parse-line lines))))

(defun conventional-changelog-format-entry (entry)
  (let ((scope (gethash "scope" entry))
        (message (gethash "message" entry))
        (hash (gethash "hash" entry)))
    (format " - %s%s (%s)\n" (if scope (format "*%s* : " scope) "") message hash)))

(defun conventional-changelog-filter-format-list (heading predicate list)
  (let ((filtered (seq-filter predicate list)))
    (if (not filtered) ()
      (let ((formatted-list
             (string-join
              (mapcar 'conventional-changelog-format-entry filtered) "")))
        (if (string= "" formatted-list) ""
          (format "%s\n\n%s\n" heading formatted-list))))))

(defun conventional-changelog-get-current-version (working-directory)
  (let ((content
         (with-temp-buffer
           (condition-case nil
               (insert-file-contents (concat working-directory "/" conventional-changelog-version-file))
             (error nil))
           (buffer-string))))
    (if (string= "" content) '(0 0 0)
      (mapcar 'string-to-number (split-string content "\\." t)))))

(defun conventional-changelog-save-current-version (working-directory version)
  (with-temp-file (concat working-directory "/" conventional-changelog-version-file)
    (insert (string-join (mapcar (lambda (elt) (format "%i" elt)) version) "."))))

(defun conventional-changelog-increase-version (current-version changes)
  (let ((severity-of-change (seq-reduce #'min (mapcar 'car changes) most-positive-fixnum))
        (i 0))
    (mapcar
     (lambda (elt)
       (let ((r
              (if (< i severity-of-change) elt
                (if (= i severity-of-change) (+ elt 1)
                  0))))
         (setq i (+ i 1))
         r))
     current-version)))

(defun conventional-changelog (&optional working-directory)
  (interactive)
  (or working-directory (setq working-directory "."))
  (let* ((current-version (conventional-changelog-get-current-version working-directory))
         (first-commit (conventional-changelog-get-first-commit working-directory))
         (changes (conventional-changelog-get-changes-list first-commit working-directory))
         (formatted
          (if (null changes) ()
            (seq-remove
             'null
             (mapcar
              (lambda (elt)
                (let* ((heading (car elt))
                       (conf (cdr elt))
                       (priority (car conf))
                       (filter (car (cdr conf)))
                       (changes (conventional-changelog-filter-format-list
                                 (format "*** %s" heading)
                                 filter changes)))
                  (if changes (list priority changes) ())))
              conventional-changelog-items)))))
    (if (null formatted) (message "No committed changes found")
      (let ((version (conventional-changelog-increase-version current-version formatted)))
        (find-file (concat working-directory "/" conventional-changelog-file))
        (org-mode)

        ;; Go to position right after top heading
        (let ((changelog-pos (org-find-exact-headline-in-buffer conventional-changelog-top-heading)))
          (if changelog-pos
              (progn
                (goto-char changelog-pos)
                (end-of-line)
                (insert "\n"))
            (insert (format "* %s\n\n" conventional-changelog-top-heading))))

        ;; Insert heading of new version
        (insert (format "** [%s] v%s\n\n"
                        (format-time-string "%Y-%m-%d")
                        (string-join (mapcar (lambda (elt) (format "%i" elt)) version) ".")))

        ;; Insert changes
        (mapcar (lambda (elt) (insert (car (cdr elt)))) formatted)

        ;; Save updated version into the file
        (conventional-changelog-save-current-version working-directory version)))))

(provide 'conventional-changelog)

;;; conventional-changelog.el ends here
