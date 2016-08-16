;;; pacman-mode --- Emacs major mode to interact with msys2 pacman

;;; Commentary:

;; See msys.el

;;; Code:

(eval-when-compile
  (require 'easymenu)
  (require 'cl-lib))
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

;; ------------------------------------------------------------
;;* Functions

(defvar-local pacman-packages nil)
(defvar-local pacman-sections nil)

(defun pacman-get-packages ()
  "Store alist of packages, (LINE . PACKAGE)."
  (let (pkgs)
    (goto-char (point-min))
    (while (re-search-forward
            (eval-when-compile
              (concat "\\(?:msys/\\)\\([^ ]+\\)\\|"
                      "\\(?:mingw32/\\)\\([^ ]+\\)\\|"
                      "\\(?:mingw64/\\)\\([^ ]+\\)"))
            nil t)
      (push (cons (line-number-at-pos)
                  (or
                   (match-string-no-properties 1)
                   (match-string-no-properties 2)
                   (match-string-no-properties 3)))
            pkgs))
    pkgs))

(defun pacman-get-sections ()
  "Sections corresponding to 'msys', 'mingw32', and 'mingw64'."
  (widen)
  (setq pacman-sections
        (cl-loop for patt in '("mingw32/" "mingw64/" "msys/")
           collect (and (goto-char (point-min))
                        (re-search-forward (regexp-quote patt) nil t)
                        (cons patt (match-beginning 0))))))

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
  (interactive))


;; ------------------------------------------------------------
;;* Mode

(defvar pacman-menu
  '("Pacman"
    ["Narrow region" pacman-narrow :help "Show only packages for msys/w32/w64"]
    ["Mark package" pacman-mark :help "Mark package"]
    ["Show installed" pacman-occur :help "Show installed packages in `occur'"]))

(defvar pacman-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil pacman-menu)
    (define-key map "o" #'pacman-occur)
    (define-key map "m" #'pacman-mark)
    (define-key map "n" #'pacman-toggle-narrow)
    map)
  "Keymap for pacman mode.")

;;;###autoload
(define-derived-mode pacman-mode special-mode "Pacman"
  "Major mode for pacman output.\n
Commands:
\\{pacman-mode-map}"
  (setq-local font-lock-defaults '(pacman-font-lock nil nil nil nil)))

(provide 'pacman-mode)

;;; pacman-mode.el ends here
