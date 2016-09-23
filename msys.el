;;; msys --- Open msys2 shells, install and interact with pacman from Emacs.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/emacs-msys
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 15 August 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some utilities to open msys2 shells and install packages/interact with
;; pacman from Emacs.
;;
;; The main entry point is `msys-pacman', which calls pacman to search or install
;; packages.  When searching, the results are displayed in `pacman-mode'.  
;; 
;;; Pacman mode:

;;  Provides emacs major mode to select and install packages from pacman
;;  package manager using a `tree-widget' format to have nodes for
;;  packages available under msys, mingw32, or mingw64 nodes.

;;  Some features:
;;  - Toggle msys/mingw32/mingw64 package results
;;  - Mark and install packages
;;  - Font-locking for msys/mingw32/mingw64 and installed packages
;;
;; ![example](example.png)
;;

;;; Code:
(autoload 'pacman-package-list "pacman-mode")

(defgroup msys nil
  "Interact with msys/pacman from emacs."
  :group 'external
  :prefix "msys-")

(defface msys-installed-face
  '((((class color) (background light))
     (:background "navy" :foreground "yellow" :weight bold :slant italic))
    (t (:background "yellow" :foreground "navy" :weight bold :slant italic)))
  "Face to highlight installed packages."
  :group 'msys)

(defcustom msys-directory "c:/msys/"
  "Root directory for msys2."
  :group 'msys
  :type 'file)

(defcustom msys-shell (expand-file-name "msys2_shell.cmd" msys-directory)
  "Name of msys2 shell (previously *.bat)."
  :group 'msys
  :type 'file)

(defcustom msys-pacman-install-command "pacman -Sy --noconfirm --needed"
  "Command to install packages with pacman."
  :group 'msys
  :type 'string)


;; ------------------------------------------------------------
;;* Msys2 terminals
;;@@FIXME: should this be inferior comint?

;;;###autoload
(defun msys-external-shell (&optional arg)
  "Launch msys2 shell. With single prefix, prompt for parameters:
 (mingw64, ming32, msys).  With prefix '(16), prompt for arguments to pass to 
shell, typing '?' in the minibuffer is temporarily bound to display options."
  (interactive "P")
  (let* ((version (if arg (ido-completing-read
                           "Shell(Default 64): "
                           '("mingw64" "mingw32" "msys") nil t)
                    "mingw64"))
         (args (if (and arg (equal arg '(16)))
                   (progn
                     (msys-temporary-keybinding "?" #'msys-shell-help)
                     (read-string "Arguments to shell (type \"?\" for options): "))
                 ""))
         (launcher (format "%s -%s %s" msys-shell version args)))
    (call-process-shell-command launcher nil 0)))

(defun msys-shell-help ()
  "Show available options to pass to msys2 terminal in temp buffer."
  (interactive)
  (with-output-to-temp-buffer "*Msys2 Shell Options*"
    (call-process msys-shell nil "*Msys2 Shell Options*" t "-help")))

(defun msys-temporary-keybinding (key cmd)
  "Temporarily bind KEY to CMD while minibuffer is active."
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   #'(lambda () (eq major-mode 'minibuffer-inactive-mode))))


;; ------------------------------------------------------------
;;* Pacman
(defvar msys-pacman-installing nil)
(defvar msys-pacman-install-buffer "*Msys2 Pacman Output*")
(defvar msys-pacman-package-buffer "*Pacman Packages*")

;;;###autoload
(defun msys-pacman (&optional arg command)
  "Interface to msys2 pacman.  With prefix install packages, otherwise search
'pacman -Ss'.  Results are shown in buffer when pacman finishes doing its stuff."
  (interactive "p")
  (let* ((installing (> arg 1))
         (default (if installing msys-pacman-install-command "pacman -Ss"))
         (buffer (if installing msys-pacman-install-buffer
                   msys-pacman-package-buffer))
         (cmd (or command
                  (concat default " " (read-shell-command (format "%s: " default)))))
         (buff (prog1 (get-buffer-create buffer)
                 (with-current-buffer buffer
                   (let ((inhibit-read-only t))
                     (erase-buffer)))))
         (proc (start-process
                "pacman"
                buffer
                (expand-file-name "usr/bin/sh.exe" msys-directory)
                "-l" "-c" cmd)))
    (message "Running: %s" cmd)
    (setq msys-pacman-installing installing)
    (set-process-sentinel proc #'msys-pacman-sentinel)))

(defun msys-pacman-sentinel (p s)
  (message "Process %s finished with status: '%s'"
           p (replace-regexp-in-string "\n" "" s))
  (pop-to-buffer (if msys-pacman-installing msys-pacman-install-buffer
                   msys-pacman-package-buffer))
  (if msys-pacman-installing
      (progn (view-mode) (goto-char (point-min)))
    (pacman-mode)
    (pacman-package-list)))

(provide 'msys)

;;; msys.el ends here
