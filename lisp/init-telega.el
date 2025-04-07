;;; init-telega.el -*- lexical-binding: t; -*-

;;; Code:

(use-package telega
  :straight (:type git :host github :repo "zevlg/telega.el")
  :custom
  (telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/HEAD-b498497/"))

(provide 'init-telega)

;;; init-telega.el ends here.
