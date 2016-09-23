;;; pacman-mode --- Emacs major mode to interact with msys2 pacman

;;; Commentary:

;; See msys.el

;;; Code:

(eval-when-compile
  (require 'easymenu)
  (require 'cl-lib))
(require 'tree-widget)
(require 'msys)
(autoload 'occur "replace")

(defgroup pacman nil
  "Pacman output from msys2."
  :group 'msys :group 'external
  :prefix "pacman-")

(defcustom pacman-default-nodes-expanded '("msys" "mingw64")
  "Nodes listed here will be expanded by default in the 
packages buffer."
  :group 'pacman)

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


;; ------------------------------------------------------------
;;* Internal

;; structure to store packages
(cl-defstruct (pacman--pkg (:constructor pacman--make-pkg))
  type name version group installed description)

(defvar pacman-packages nil)
(defvar pacman-selected-packages nil)
(defun pacman-get-packages ()
  (let (pkgs)
    (goto-char (point-min))
    (while (re-search-forward
            (eval-when-compile
              (concat (regexp-opt '("msys" "mingw32" "mingw64") t)
                      "/\\([^ ]+\\) \\([^ ]+\\)\\s-*"
                      "\\(\(.*\)\\)?\\s-*"
                      "\\(\\\[installed.*\\\]\\)?\n\\s-*"
                      "\\(.*\\)"))
            nil t)
      (push (pacman--make-pkg
             :type (match-string-no-properties 1)
             :name (match-string-no-properties 2)
             :version (or (match-string-no-properties 3) "")
             :group (or (match-string-no-properties 4) "")
             :installed (or (match-string-no-properties 5) "")
             :description (match-string-no-properties 6))
            pkgs))
    pkgs))


;;* Package widgets

(defmacro pacman-mode-dialog (&rest forms)
  (declare (indent 1) (debug t))
  `(progn
     (pacman-mode)
     ,@forms
     (widget-setup)
     (goto-char (point-min))
     (condition-case nil
         (widget-forward 1)
       (error (goto-char (point-min))))
     ;; (pacman-package-goto-first 'tree-widget)
     ))

;; (defun pacman-package-tree-nodes ()
;;   "Get list of tree nodes."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let (res)
;;       (condition-case nil
;;           (progn
;;             (widget-move 1)
;;             (when (widget-type-match (widget-at (point))
;;                                      :tree-widget-open-icon)
;;               (push ))
;;             )
;;         ))))

(defun pacman-package-expand-all ()
  "Expand all tree nodes, exposing all children."
  )

(defun pacman-package-goto-first (widget-type)
  "Move cursor to the first WIDGET-TYPE."
  (goto-char (point-min))
  (condition-case nil
      (let (done)
        (widget-move 1)
        (while (not done)
          (if (eq widget-type (widget-type (widget-at (point))))
              (setq done t))))
    (error (goto-char (point-min)))))

(defun pacman-package-select (widget &rest _ignore)
  "Select a package based on the checkbox WIDGET state."
  (let ((value (widget-get widget :package))
        (check (widget-value widget)))
    (if check
        (add-to-list 'pacman-selected-packages value)
      (setq pacman-selected-packages
            (delq value pacman-selected-packages)))
    (message "%s %sselected" value (if check "" "un"))))

(defun pacman-package-list ()
  "Setup pacman package listing."
  (pacman-mode-dialog (current-buffer)
    ;; (setq-local inhibit-read-only t)
    (setq-local widget-field-add-space nil)
    (erase-buffer)
    (widget-insert
     "Click on Install or type `i' to install selected packages. 
Click on Cancel or type `q' to cancel.\n\n")
    
    ;; Insert packages as checkboxes
    (dolist (type '("msys" "mingw32" "mingw64"))
      (widget-create
       'tree-widget
       :open (member type pacman-default-nodes-expanded)
       :tag type
       :args
       (cl-loop for item in pacman-packages
          when (string= (pacman--pkg-type item) type)
          collect (widget-convert
                   'checkbox
                   :parent type
                   :value nil
                   :format " %{%v%} %t\n    %d"
                   :tag (format "%s %s %s %s"
                                (pacman--pkg-name item)
                                (pacman--pkg-version item)
                                (pacman--pkg-group item)
                                (pacman--pkg-installed item))
                   :doc (pacman--pkg-description item)
                   :package (pacman--pkg-name item)
                   :notify 'pacman-package-select))))
    
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


;; ------------------------------------------------------------
;;* User Interface

(defun pacman-installed ()
  "Occur the installed packages in buffer."
  (interactive)
  (occur "\\(installed\\)"))

(defun pacman-install (&rest _ignore)
  "Install selected packages."
  (interactive)
  (if pacman-selected-packages
      (progn
        (msys-pacman 4 (concat
                        msys-pacman-install-command " "
                        (mapconcat 'identity pacman-selected-packages " ")))
        (pop-to-buffer msys-pacman-install-buffer))
    (message "No packages selected for installation.")))

(defun pacman-upgrade (&rest _ignore)
  "Upgrade selected packages."
  (interactive)
  (if pacman-selected-packages
      (progn
        (msys-pacman 4 (concat
                        msys-pacman-install-command " "
                        (mapconcat 'identity pacman-selected-packages " ")))
        (pop-to-buffer msys-pacman-install-buffer))))

(defun pacman-cancel (&rest _ignore)
  "Kill the pacman buffer."
  (interactive)
  (setq pacman-selected-packages nil)
  (kill-buffer (current-buffer))
  (message "Pacman output killed"))


;; ------------------------------------------------------------
;;* Mode

(defvar pacman-font-lock-vars
  '(("\\(msys\\>\\)\)?\\s-*\\([^ ]+\\)"
     (1 'pacman-msys-face)
     (2 font-lock-constant-face))
    ("\\(mingw32\\>\\)\)?\\s-*\\([^ ]+\\)"
     (1 'pacman-mingw32-face)
     (2 font-lock-constant-face))
    ("\\(mingw64\\>\\)\)?\\s-*\\([^ ]+\\)"
     (1 'pacman-mingw64-face)
     (2 font-lock-constant-face))
    ("\\[\\(installed\\).*\\]" 1 'msys-installed-face)))

(defun pacman-font-lock (packages)
  (let ((pkgs (regexp-opt (mapcar 'pacman--pkg-name packages)))
        (ver (regexp-opt (mapcar 'pacman--pkg-version packages))))
    (append
     pacman-font-lock-vars
     `((,pkgs . font-lock-builtin-face)
       (,ver . font-lock-constant-face)))))

(defvar pacman-menu
  '("Pacman"
    ["Install" pacman-install :help "Install selected packages"]
    ["Upgrade" pacman-upgrade]
    ["Show installed (Occur)" pacman-installed :help "Occur installed packages"]
    ["Cancel" pacman-cancel]))

(defvar pacman-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (easy-menu-define nil km nil pacman-menu)
    (define-key km "n" 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km "i" 'pacman-install)
    (define-key km "o" 'pacman-installed)
    (define-key km "q" 'pacman-cancel)
    (define-key km "u" 'pacman-upgrade)
    km)
  "Keymap for pacman mode.")

;;;###autoload
(define-derived-mode pacman-mode nil "Pacman"
  "Major mode for pacman output.\n
Commands:
\\{pacman-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq-local pacman-packages (pacman-get-packages))
  (setq truncate-lines t)
  (setq-local font-lock-defaults
              `(,(pacman-font-lock pacman-packages) nil nil nil nil)))

(provide 'pacman-mode)

;;; pacman-mode.el ends here
