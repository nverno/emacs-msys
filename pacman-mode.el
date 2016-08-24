;;; pacman-mode --- Emacs major mode to interact with msys2 pacman

;;; Commentary:

;; See msys.el

;;; Code:

(eval-when-compile
  (require 'easymenu)
  (require 'cl-lib))
(require 'tree-widget)
(autoload 'occur "replace")

(defgroup pacman nil
  "Pacman output from msys2."
  :group 'msys :group 'external
  :prefix "pacman-")

(defface pacman-msys-face
  '((t (:background "pink" :foreground "#504945" :weight bold)))
  "Msys package face."
  :group 'pacman)

(defface pacman-mingw32-face
  '((t (:background "light blue" :foreground "#504945" :weight bold)))
  "Ming32 package face."
  :group 'pacman)

(defface pacman-mingw64-face
  '((t (:background "light green" :foreground "#504945" :weight bold)))
  "Ming64 package face."
  :group 'pacman)

(defvar pacman-package-buffer "*Pacman Packages*")


;; ------------------------------------------------------------
;;* Internal

;; structure to store packages
(cl-defstruct (pacman--pkg (:constructor pacman--make-pkg))
  type name version installed description)

(defvar-local pacman-packages nil)

(defun pacman-get-packages ()
  (let (pkgs)
    (goto-char (point-min))
    (while (re-search-forward
            (eval-when-compile
              (concat (regexp-opt '("msys" "mingw32" "mingw64") t)
                      "/\\([^ ]+\\) \\([^ ]+\\)\\(installed\\)?\n\\s-*"
                      "\\(.*\\)"))
            nil t)
      (push (pacman--make-pkg
             :type (match-string-no-properties 1)
             :name (match-string-no-properties 2)
             :version (match-string-no-properties 3)
             :installed (match-string-no-properties 4)
             :description (match-string-no-properties 5))
            pkgs))
    pkgs))


;;* Package widgets
;; based off recentf.el `recentf-dialog-mode'

(defvar-local pacman-selected-packages nil)

(defmacro pacman-mode-dialog (&rest forms)
  (declare (indent 1) (debug t))
  `(progn
     (pacman-mode)
     ,@forms
     (widget-setup)))

(defun pacman-package-select (widget &rest _ignore)
  "Select a package based on the checkbox WIDGET state."
  (let ((value (widget-get widget :tag))
        (check (widget-value widget)))
    (if check
        (add-to-list 'pacman-selected-packages value)
      (setq pacman-selected-packages (delq value pacman-selected-packages)))
    (message "%s %sselected" value (if check "" "un"))))

(defun pacman-package-list ()
  "Setup pacman package listing."
  (pacman-mode-dialog (current-buffer)
    (set (make-local-variable 'pacman-packages) (pacman-get-packages))
    (set (make-local-variable 'pacman-selected-packages) nil)
    (erase-buffer)
    (widget-insert
     "Click on OK to select package to install. 
Click on Cancel or type `q' to cancel.\n")
    ;; Insert packages as checkboxes
    (dolist (item pacman-packages)
       (widget-create 'checkbox
                     :value nil
                     :format "\n %[%v%]  %t"
                     :tag (format "(%s)%s %s %s"
                                  (pacman--pkg-type item)
                                  (pacman--pkg-name item)
                                  (pacman--pkg-version item)
                                  (pacman--pkg-installed item))
                     :notify 'pacman-package-select))
    (widget-insert "\n\n")
    (widget-create
     'push-button
     :notify 'pacman-install
     :help-echo "Install selected packages"
     "Install")
    (widget-insert " ")
    (widget-create
     'push-button
     :notify 'pacman-cancel
     "Cancel")))

(defun pacman-print-packages ()
  (interactive)
  (when pacman-selected-packages
   (dolist (item pacman-selected-packages)
     (prin1 item))))

;; (defun pacman-get-sections ()
;;   "Sections corresponding to 'msys', 'mingw32', and 'mingw64'."
;;   (widen)
;;   (setq pacman-sections
;;         (cl-loop for patt in '("mingw32/" "mingw64/" "msys/")
;;            collect (and (goto-char (point-min))
;;                         (re-search-forward (regexp-quote patt) nil t)
;;                         (cons patt (match-beginning 0))))))


;; ------------------------------------------------------------
;;* User Interface

(defun pacman-narrow-matching (arg)
  (interactive
   (list (ido-completing-read "Only show packages for: "
                              '("msys" "mingw32" "mingw64"))))
  (let* ((sections (or pacman-sections (pacman-get-sections)))
         (inds (mapcar 'cdr sections))
         (start (cdr (assoc-string (concat arg "/") sections)))
         (end (and start
                   (or (cadr (memq start inds))
                       (point-max)))))
    (narrow-to-region start end)))

(defun pacman-toggle-narrow ()
  "Toggle narrowing to region."
  (interactive)
  (if (buffer-narrowed-p)
      (progn
        (widen)
        (goto-char (point-min)))
    (call-interactively 'pacman-narrow-matching)))

(defun pacman-occur ()
  (interactive)
  (occur "\\(installed\\)"))

;; @@TODO: add marks, alist with line number to retrieve packages from
;; 'packman-packages' alist
(defun pacman-mark ()
  "Mark package."
  (interactive))

(defun pacman-installed ()
  "Show installed packages only."
  (interactive))

(defun pacman-install (&rest _ignore)
  "Install selected packages."
  (interactive)
  (if pacman-selected-packages
      (dolist (pkg pacman-selected-packages)
        (prin1 (pacman--pkg-name pkg)))
    (message "No packages selected for installation.")))

(defun pacman-cancel (&rest _ignore)
  "Kill the pacman buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Pacman output killed"))


;; ------------------------------------------------------------
;;* Mode

(defvar pacman-font-lock
  '(("\\(msys/\\)\\([^ ]+\\)"
     (1 'pacman-msys-face)
     (2 font-lock-constant-face))
    ("\\(mingw32/\\)\\([^ ]+\\)"
     (1 'pacman-mingw32-face)
     (2 font-lock-constant-face))
    ("\\(mingw64/\\)\\([^ ]+\\)"
     (1 'pacman-mingw64-face)
     (2 font-lock-constant-face))
    ("\\[\\(installed\\)\\]" . 'msys-installed-face)))

(defvar pacman-menu
  '("Pacman"
    ;; ["Mark package" pacman-mark :help "Mark package"]
    ["Install" pacman-install :help "Install selected packages"]
    ;; ["Narrow region" pacman-narrow :help "Show only packages for msys/w32/w64"]
    ["Show installed" pacman-installed :help "Show only installed packages"]
    ["Occur installed" pacman-occur :help "Occur installed packages"]))

(defvar pacman-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (easy-menu-define nil km nil pacman-menu)
    (define-key km "n" 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km (kbd "C-i") 'pacman-installed)
    (define-key km "i" 'pacman-install)
    (define-key km "o" 'pacman-occur)
    (define-key km "q" 'pacman-cancel)
    km)
  "Keymap for pacman mode.")

;;;###autoload
(define-derived-mode pacman-mode nil "Pacman"
  "Major mode for pacman output.\n
Commands:
\\{pacman-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  ;; (setq-local font-lock-defaults '(pacman-font-lock nil nil nil nil))
  )

(provide 'pacman-mode)

;;; pacman-mode.el ends here
