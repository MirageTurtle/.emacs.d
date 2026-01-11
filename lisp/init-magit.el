;;; init-magit.el --- Git Configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package transient
  :straight t)

(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-dispatch)
  ("C-c f" . magit-file-dispatch)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  ;; copied from https://github.com/LuciusChen/.emacs.d/blob/d6246d7abcf4a79e2765767214c7c1c15b03d281/lib/lib-magit.el
  (defconst gptel-commit-prompt
    "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`,
  `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.")

  (defun gptel-commit ()
    "Generate commit message with gptel and insert it into the buffer."
    (interactive)
    (require 'init-llm)
    (let* ((lines (magit-git-lines "diff" "--cached"))
           (changes (string-join lines "\n")))
      (gptel-request changes :system gptel-commit-prompt))))

;; [ssh agent]
(defvar mt/original-ssh-auth-sock nil
  "Holds the original SSH_AUTH_SOCK value.")
(defun mt/set-bitwarden-ssh-agent ()
  "Set up Bitwarden SSH agent."
  (interactive)
  (let ((current-sock (getenv "SSH_AUTH_SOCK"))
        (bitwarden-sock (expand-file-name "~/.bitwarden/ssh-agent.sock")))
    (unless (string= current-sock bitwarden-sock)
      (setenv "SSH_AUTH_SOCK" bitwarden-sock)
      (setq mt/original-ssh-auth-sock current-sock))
    (message "Switched to Bitwarden SSH agent.")))

(provide 'init-magit)

;;; init-magit.el ends here
