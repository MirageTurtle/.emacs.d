;;; init-straight.el --- straight package manager configuration -*- lexical-binding: t; -*-

;; [straight] Package manager config
;; put setq before installation
;; most are copied from https://github.com/roife/.emacs.d/blob/master/core/init-straight.el
(setq straight-check-for-modifications nil                   ; skip modification
      straight-vc-git-default-clone-depth '(1 single-branch) ; shadow clone
      comp-deferred-compilation-deny-list ()                 ; config native comp
      warning-suppress-log-types '((comp))                   ; Don't display comp warnings
      straight-disable-native-compile (not (and (fboundp 'native-comp-available-p)
                                                (native-comp-available-p)))
      straight-recipe-repositories '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                                     ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                                     ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                                     ("gnu-devel" . "https://mirrors.ustc.edu.cn/elpa/gnu-devel/")))

;; installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; [use-package] config
(eval-when-compile
  (require 'use-package))
(setq use-package-expand-minimally t)

(provide 'init-straight)

;;; init-straight.el ends here
