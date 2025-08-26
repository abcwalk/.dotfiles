;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

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
;;

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

(setenv "PYENV_ROOT" (concat (getenv "HOME") "/.pyenv"))
(setenv "PYENV_SHELL" "zsh")

(let ((pyenv-bin (concat (getenv "HOME") "/.pyenv/bin")))
  (when (file-directory-p pyenv-bin)
    (setenv "PATH" (concat pyenv-bin ":" (getenv "PATH")))))

;; Python Mode
;; Install: pip install pyflakes autopep8
(use-package python
  :ensure nil
  :functions exec-path-from-shell-copy-env
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  (setq python-shell-completion-native-enable nil)

  :config
  (defun my-python-setup-project-env ()
    (when-let ((root (projectile-project-root)))
      (let ((venv-python (expand-file-name ".venv/bin/python3.12" root))
            (framework-path (expand-file-name "framework" root)))
        (when (file-exists-p venv-python)
          (setq-local python-shell-interpreter venv-python)
          (setenv "PYTHONPATH"
                  (concat (file-truename root) ":" (file-truename framework-path)))
          (message "PYTHON :: venv activated: %s" venv-python)))))

  (add-hook 'python-mode-hook 'my-python-setup-project-env)
  (add-hook 'python-ts-mode-hook 'my-python-setup-project-env))

(use-package pyvenv
  :config
  ;;  :diminish
  (setq pyvenv-mode-line-indicator '(pyenv-mode-version-name ("[pyenv:" pyenv-mode-version-name "] ")))

  (defun projectile-pyenv-mode-set ()
    (let ((project-name (projectile-project-name)))
      (when (member project-name (pyenv-mode-versions))
        (pyenv-mode-set project-name))))

  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
  (pyenv-mode t))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
