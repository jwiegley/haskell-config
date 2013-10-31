;;; haskell-config --- My personal configurations for haskell-mode

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 09 Aug 2012
;; Version: 1.0
;; Keywords: haskell programming awesomeness
;; X-URL: https://github.com/jwiegley/dot-emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; My personal configurations for haskell-mode.  Requires my `use-package'
;; macro from:
;;
;;     https://github.com/jwiegley/use-package
;;
;; Further, this code currently depends on my fork of haskell-mode:
;;
;;     https://github.com/jwiegley/haskell-mode

(require 'use-package)

(load "haskell-mode-autoloads")

(defcustom haskell-config-use-unicode-symbols nil
  "If non-nil, use Unicode symbols to represent mathematical operators."
  :type 'boolean
  :group 'haskell)

(defface haskell-subscript '((t :height 0.6))
 "Face used for subscripts."
 :group 'haskell)

(defcustom hoogle-binary-path (expand-file-name "~/.cabal/bin/hoogle")
  "Path to the local 'hoogle' binary."
  :type 'file
  :group 'haskell)

(defconst ghc-extensions
  '("-Wall"
    "-XBangPatterns"
    "-XCPP"
    "-XConstraintKinds"
    "-XDefaultSignatures"
    "-XDeriveDataTypeable"
    "-XDeriveFoldable"
    "-XDeriveFunctor"
    "-XDeriveGeneric"
    "-XDeriveTraversable"
    "-XEmptyDataDecls"
    "-XFlexibleContexts"
    "-XFlexibleInstances"
    "-XFunctionalDependencies"
    "-XGADTs"
    "-XGeneralizedNewtypeDeriving"
    "-XImplicitParams"
    "-XMonadComprehensions"
    "-XMultiParamTypeClasses"
    "-XNamedFieldPuns"
    ;; "-XNoImplicitPrelude"
    "-XNoMonomorphismRestriction"
    "-XOverloadedStrings"
    "-XPackageImports"
    "-XParallelListComp"
    "-XPatternGuards"
    "-XRankNTypes"
    "-XRecordWildCards"
    "-XScopedTypeVariables"
    "-XStandaloneDeriving"
    "-XTemplateHaskell"
    "-XTupleSections"
    "-XTypeFamilies"
    "-XViewPatterns"))

(defconst haskell-unicode-conversions
  '(("[ (]\\(->\\)[) \n]"     . ?→)
    ("[ (]\\(/=\\)[) ]"       . ?≠)
    ;;("[ (]\\(<=\\)[) ]"       . ?≤)
    ;;("[ (]\\(>=\\)[) ]"       . ?≥)
    ;;("[ (]\\(=\\)[) ]"        . ?≡)
    ("[ (]\\(\\.\\)[) ]"      . ?∘)
    ("[ (]\\(&&\\)[) ]"       . ?∧)
    ("[ (]\\(||\\)[) ]"       . ?∨)
    ("[ (]\\(\\*\\)[) ]"      . ?×)
    ("[ (]\\(\\\\\\)[(_a-z]"  . ?λ)
    (" \\(<-\\)[ \n]"         . ?←)
    (" \\(-<\\) "             . ?↢)
    (" \\(>-\\) "             . ?↣)
    (" \\(=>\\)[ \n]"         . ?⇒)
    ;;(" \\(>=>\\) "           . ?↣)
    ;;(" \\(<=<\\) "           . ?↢)
    ;;(" \\(>>=\\) "           . ?↦)
    ;;(" \\(=<<\\) "           . ?↤)
    ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)
    ;;("[ (]\\(<<<\\)[ )]"      . ?⋘)
    ;;("[ (]\\(>>>\\)[ )]"      . ?⋙)
    (" \\(::\\) "             . ?∷)
    ("\\(`union`\\)"          . ?⋃)
    ("\\(`intersect`\\)"      . ?⋂)
    ("\\(`elem`\\)"           . ?∈)
    ("\\(`notElem`\\)"        . ?∉)
    ;;("\\<\\(mempty\\)\\>"    . ??)
    ;; ("\\(`mappend`\\)"        . ?⨂)
    ;; ("\\(`msum`\\)"           . ?⨁)
    ;; ("\\(\\<True\\>\\)"       . "𝗧𝗿𝘂𝗲")
    ;; ("\\(\\<False\\>\\)"      . "𝗙𝗮𝗹𝘀𝗲")
    ("\\(\\<undefined\\>\\)"  . ?⊥)
    ("\\<\\(forall \\)\\>"   . ?∀)))

(defvar hoogle-server-process nil)

(defvar put-index 0)
(defvar put-prefix "step")

(defun haskell-setup-unicode-conversions ()
  (if (and nil (featurep 'proof-site))
      (use-package haskell-unicode-tokens
        :load-path "site-lisp/proofgeneral/generic/"
        :config
        (hook-into-modes #'(lambda ()
                             (ignore-errors
                               (unicode-tokens-mode 1))
                             (unicode-tokens-use-shortcuts 0))
                         '(haskell-mode-hook
                           literate-haskell-mode-hook)))
    (mapc (lambda (mode)
            (font-lock-add-keywords
             mode
             (append (mapcar (lambda (chars)
                               `(,(car chars)
                                 ,(if (characterp (cdr chars))
                                      `(0 (ignore
                                           (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ,(cdr chars))))
                                    `(0 ,(cdr chars)))))
                             haskell-unicode-conversions)
                     '(("(\\|)" . 'esk-paren-face)
                       ;; ("\\<[a-zA-Z]+\\([0-9]\\)\\>"
                       ;;  1 haskell-subscript)
                       ))))
          '(haskell-mode literate-haskell-mode))))

(defun my-haskell-load-and-run ()
  "Loads and runs the current Haskell file."
  (interactive)
  (inferior-haskell-load-and-run inferior-haskell-run-command)
  (sleep-for 0 100)
  (goto-char (point-max)))

(defun my-inferior-haskell-find-definition ()
  "Jump to the definition immediately, the way that SLIME does."
  (interactive)
  (inferior-haskell-find-definition (haskell-ident-at-point))
  (forward-char -1))

(defun my-inferior-haskell-find-haddock (sym)
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string
            (if (> (length sym) 0)
                (format "Find documentation of (default %s): " sym)
              "Find documentation of: ")
            nil nil sym))))
  (inferior-haskell-find-haddock sym)
  (goto-char (point-min))
  (search-forward (concat sym " ::") nil t)
  (search-forward (concat sym " ::") nil t)
  (goto-char (match-beginning 0)))

(defun my-inferior-haskell-type (expr &optional insert-value)
  "When used with C-u, don't do any prompting."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (if current-prefix-arg
               sym
             (read-string (if (> (length sym) 0)
                              (format "Show type of (default %s): " sym)
                            "Show type of: ")
                          nil nil sym))
           current-prefix-arg)))
  (message (inferior-haskell-type expr insert-value)))

(defun my-inferior-haskell-break (&optional arg)
  (interactive "P")
  (let ((line (line-number-at-pos))
        (col (if arg
                 ""
               (format " %d" (current-column))))
        (proc (inferior-haskell-process)))
    (inferior-haskell-send-command
     proc (format ":break %d%s" line col))
    (message "Breakpoint set at %s:%d%s"
             (file-name-nondirectory (buffer-file-name)) line col)))

(defun define-haskell-checkers ()
  (eval
   `(flycheck-define-checker haskell-hdevtools
      "A Haskell syntax and type checker using hdevtools.

See URL `https://github.com/bitc/hdevtools'."
      :command
      ("hdevtools" "check"
       ,@(apply
          #'append (mapcar (lambda (x) (list "-g" x))
                           ghc-extensions))
       ,@(let ((hdevtools-path (executable-find "hdevtools")))
           (if (and hdevtools-path
                    (string-match "\\`\\(.+/\\.hsenv/\\)"
                                  hdevtools-path))
               (let* ((path-prefix (match-string 1 hdevtools-path))
                      (ghc-version
                       (with-temp-buffer
                         (shell-command "ghc --version" t)
                         (goto-char (point-min))
                         (re-search-forward "version \\(.+\\)")
                         (match-string 1))))
                 (list "-g" (concat "-package-conf="
                                    (expand-file-name
                                     (concat "ghc/lib/ghc-"
                                             ghc-version
                                             "/package.conf.d")
                                     path-prefix))))))
       source-inplace):error-patterns
      ((warning line-start (file-name) ":" line ":" column ":"
                (or " " "\n    ") "Warning:" (optional "\n")
                (one-or-more " ")
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more " ")
                                       (one-or-more not-newline)))
                line-end)
       (error line-start (file-name) ":" line ":" column ":"
              (or (message (one-or-more not-newline))
                  (and "\n" (one-or-more " ")
                       (message (one-or-more not-newline)
                                (zero-or-more "\n"
                                              (one-or-more " ")
                                              (one-or-more not-newline)))))
              line-end))
      :modes haskell-mode
      :next-checkers ((warnings-only . haskell-hlint))))

  (eval
   `(flycheck-define-checker haskell-ghc
      "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
      :command ("ghc" "-i." "-i.." "-i../.." "-v0"
                ,@ghc-extensions source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ":"
                (or " " "\n    ") "Warning:" (optional "\n")
                (one-or-more " ")
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more " ")
                                       (one-or-more not-newline)))
                line-end)
       (error line-start (file-name) ":" line ":" column ":"
              (or (message (one-or-more not-newline))
                  (and "\n" (one-or-more " ")
                       (message (one-or-more not-newline)
                                (zero-or-more "\n"
                                              (one-or-more " ")
                                              (one-or-more not-newline)))))
              line-end))
      :modes haskell-mode
      :next-checkers ((warnings-only . haskell-hlint)))))

(defun hoogle-local (query)
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def))))
  (let ((buf (get-buffer "*hoogle*")))
    (if buf
        (kill-buffer buf))
    (setq buf (get-buffer-create "*hoogle*"))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (call-process hoogle-binary-path nil t t query)
      (goto-char (point-min))
      (highlight-lines-matching-regexp (regexp-quote query) 'helm-match)
      (display-buffer (current-buffer)))))

(defun insert-counting-putStrLn (arg)
  (interactive "P")
  (if arg
      (setq put-index 0
            put-prefix (read-string "Prefix: ")))
  ;; (insert (format "trace (\"%s %d..\") $ return ()\n"
  ;;                 put-prefix
  ;;                 (setq put-index (1+ put-index))))
  (insert (format "liftIO $ putStrLn $ \"%s %d..\"\n"
                  put-prefix
                  (setq put-index (1+ put-index))))
  (forward-line -1)
  (indent-according-to-mode)
  (forward-line))

(defun find-first-cabal-file (dir)
  (car (directory-files dir t "\\.cabal$")))

(defun find-project-cabal-file (&optional directory)
  (let ((dir (or directory default-directory))
        cabal-file)
    (while (and (null (setq cabal-file (find-first-cabal-file dir)))
                (progn
                  (setq dir
                        (file-name-directory
                         (directory-file-name
                          (file-name-directory dir))))
                  ;; Give up if we are already at the root dir.
                  (not (string= "/" dir)))))
    cabal-file))

(defun add-missing-package ()
  (save-excursion
    (goto-char (or compilation-filter-start (point-min)))
    (when (re-search-forward
           (concat "It is a member of the hidden package"
                   " `\\(.+?\\)-\\([0-9].+\\)'\\.") nil t)
      (let ((cabal-file (find-project-cabal-file))
            (package (match-string 1))
            (version (match-string 2)))
        (message "Found build depends: %s-%s" package version)
        (when cabal-file
          (message "Cabal file is %s" cabal-file)
          (with-current-buffer (find-file cabal-file)
            (goto-char (point-max))
            (when (re-search-backward "[Bb]uild-depends:" nil t)
              (message "Found build depends")
              (forward-paragraph)
              (indent-according-to-mode)
              (insert ", " package " >= " version))))))))

(defun inferior-haskell-find-haddock (sym &optional arg)
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if (> (length sym) 0)
                            (format "Find documentation of (default %s): "
                                    sym)
                          "Find documentation of: ")
                        nil nil sym)
           current-prefix-arg)))
  (setq sym (inferior-haskell-map-internal-ghc-ident sym))
  (let* ( ;; Find the module and look it up in the alist
         (module (let ((mod (condition-case err
                                (inferior-haskell-get-module sym)
                              (error sym))))
                   (if (string-match ":\\(.+?\\)\\.[^.]+$" mod)
                       (match-string 1 mod)
                     mod)))
         (alist-record (assoc module (inferior-haskell-module-alist))))
    (if (null alist-record)
        (my-haskell-hoogle sym arg)
      (let* ((package (nth 1 alist-record))
             (file-name (concat (subst-char-in-string ?. ?- module)
                                ".html"))
             (local-path (concat (nth 2 alist-record) "/" file-name))
             (url (if (or (eq inferior-haskell-use-web-docs 'always)
                          (and (not (file-exists-p local-path))
                               (eq inferior-haskell-use-web-docs
                                   'fallback)))
                      (concat inferior-haskell-web-docs-base
                              package "/" file-name
                              ;; Jump to the symbol anchor within Haddock.
                              "#v:" sym)
                    (and (file-exists-p local-path)
                         (concat "file://" local-path)))))
        (let ((browse-url-browser-function
               (if (not arg)
                   browse-url-browser-function
                 '((".*" . w3m-browse-url)))))
          (if url
              (browse-url url)
            (error "Local file doesn't exist")))))))

(defun my-haskell-pointfree (beg end)
  (interactive "r")
  (let ((str (buffer-substring beg end)))
    (delete-region beg end)
    (call-process "pointfree" nil t nil str)
    (delete-char -1)))

(defun haskell-chris ()
  (require 'haskell-session)
  (require 'haskell-move-nested)
  (require 'haskell-interactive-mode)

  ;; Load the current file (and make a session if not already made).
  (bind-key "C-. C-l" 'haskell-process-load-or-reload haskell-mode-map)

  ;; Switch to the REPL.
  (bind-key "C-. C-z" 'haskell-interactive-bring haskell-mode-map)
  (bind-key "C-. C-k" 'haskell-interactive-mode-clear haskell-mode-map)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (bind-key "C-. C-t" 'haskell-process-do-type haskell-mode-map)
  (bind-key "C-. C-i" 'haskell-process-do-info haskell-mode-map)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (bind-key "C-. SPC" 'haskell-mode-contextual-space haskell-mode-map)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (bind-key "C-. C-m" 'haskell-navigate-imports haskell-mode-map)

  ;; Jump to the definition of the current symbol.
  (bind-key "C-. M-." 'haskell-mode-tag-find haskell-mode-map)

  (bind-key "C-. C-M-k" 'haskell-kill-nested haskell-mode-map)
  ;; (bind-key "M-{" 'haskell-nested-return haskell-mode-map)

  ;; Visit the latest error
  (bind-key "<f9>" 'haskell-interactive-mode-visit-error haskell-mode-map)

  (bind-key "C-. M-," 'haskell-who-calls haskell-mode-map))

(defun define-haskell-checkers-no-implicit-prelude ()
  (interactive)
  (let ((ghc-extensions
         (append ghc-extensions
                 '("-XNoImplicitPrelude"
                   "-i/Users/johnw/fpco/async-actor"
                   "-i/Users/johnw/fpco/isolation-runner"
                   ))))
    (define-haskell-checkers)))

(defun define-haskell-checkers-with-implicit-prelude ()
  (interactive)
  (define-haskell-checkers))

(defun my-haskell-mode-hook ()
  (auto-complete-mode 1)
  (whitespace-mode 1)
  (bug-reference-prog-mode 1)
  ;; (flyparse-mode 1)
  (flycheck-mode 1)
  (define-haskell-checkers)

  (set (make-local-variable 'comint-prompt-regexp) "^>>> *")
  (setq compilation-first-column 1)

  ;; (add-hook 'after-save-hook 'check-parens nil t)

  (ghc-init)

  (turn-on-haskell-indentation)
  (turn-on-font-lock)
  (turn-on-haskell-decl-scan)

  (smartparens-mode 1)
  ;; (smartparens-strict-mode 1)
  (show-smartparens-mode 1)

  (setq sp-pair-list
        '(("\\\"" . "\\\"")
          ("{-" . "-}")
          ("\"" . "\"")
          ("(" . ")")
          ("[" . "]")
          ("{" . "}")
          ("`" . "`")))

  ;; (let ((this-directory default-directory))
  ;;   (while (not (string= this-directory ""))
  ;;     (let ((hsenv (expand-file-name ".hsenv" this-directory)))
  ;;       (if (file-exists-p hsenv)
  ;;           (set (make-local-variable 'exec-path)
  ;;                (cons (expand-file-name "cabal/bin" hsenv)
  ;;                      (cons (expand-file-name "bin" hsenv) exec-path)))))
  ;;     (setq this-directory
  ;;           (file-name-directory
  ;;            (file-name-nondirectory exec-path)))))

  (require 'align)
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))

  (bind-key "C-<left>" (lambda ()
                         (interactive)
                         (haskell-move-nested -4))
            haskell-mode-map)
  (bind-key "S-<left>" (lambda ()
                         (interactive)
                         (haskell-move-nested -1))
            haskell-mode-map)

  (bind-key "C-<right>" (lambda ()
                          (interactive)
                          (haskell-move-nested 4))
            haskell-mode-map)
  (bind-key "S-<right>" (lambda ()
                          (interactive)
                          (haskell-move-nested 1))
            haskell-mode-map)

  (bind-key "C-c C-u" (lambda ()
                        (interactive)
                        (insert "undefined"))
            haskell-mode-map)

  (defun killall-hdevtools (&optional arg)
    (interactive "P")
    (shell-command "killall hdevtools")
    ;; (flyparse-buffer)
    (when arg
      (define-haskell-checkers-no-implicit-prelude))
    (flycheck-buffer))

  (bind-key "C-x SPC" 'my-inferior-haskell-break haskell-mode-map)
  (bind-key "C-h C-i" 'my-inferior-haskell-find-haddock haskell-mode-map)
  (bind-key "C-c C-b" 'haskell-bot-show-bot-buffer haskell-mode-map)
  (bind-key "C-c C-d" 'ghc-browse-document haskell-mode-map)
  (bind-key "C-c C-k" 'inferior-haskell-kind haskell-mode-map)
  (bind-key "C-c C-r" 'inferior-haskell-load-and-run haskell-mode-map)
  ;; (bind-key "C-c C-c" 'flyparse-buffer haskell-mode-map)
  (bind-key "C-c C-c" 'flycheck-buffer haskell-mode-map)
  (bind-key "C-c C" 'killall-hdevtools haskell-mode-map)

  (use-package shm
    :disabled t
    :config
    (progn
      (bind-key "C-k" 'shm-kill-line haskell-mode-map)
      (bind-key "M-(" 'shm-wrap-parens haskell-mode-map)
      ;; (bind-key "(" 'shm-open-parens haskell-mode-map)
      (bind-key "C-M-u" 'shm-goto-parent haskell-mode-map)
      (bind-key "C-M-k" 'shm-kill-node haskell-mode-map)
      (bind-key "C-M-f" 'shm-forward-node haskell-mode-map)
      (bind-key "C-M-b" 'shm-backward-node haskell-mode-map)
      (bind-key "M-}" 'shm-forward-paragraph haskell-mode-map)
      (bind-key "M-{" 'shm-backward-paragraph haskell-mode-map)))

  (use-package haskell-edit)
  (bind-key "C-c M-q" 'haskell-edit-reformat haskell-mode-map)

  (when nil
    (unbind-key "C-c C-l" haskell-mode-map)
    (unbind-key "C-c C-z" haskell-mode-map)
    ;; (bind-key "SPC" 'haskell-mode-contextual-space haskell-mode-map)
    (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
    (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map))

  ;; Use C-u C-c C-t to auto-insert a function's type above it
  (if t
      (progn
        (bind-key "C-c C-t" 'ghc-show-type haskell-mode-map)
        (bind-key "C-c C-i" 'ghc-show-info haskell-mode-map))
    (bind-key "C-c C-t" 'my-inferior-haskell-type haskell-mode-map)
    (bind-key "C-c C-i" 'inferior-haskell-info haskell-mode-map))

  ;; (bind-key "M-." 'my-inferior-haskell-find-definition haskell-mode-map)
  (bind-key "M-." 'find-tag haskell-mode-map)
  (bind-key "M-n" 'flycheck-next-error haskell-mode-map)
  (bind-key "M-p" 'flycheck-previous-error haskell-mode-map)

  (bind-key "C-c C-s" 'ghc-insert-template haskell-mode-map)
  (bind-key "C-c C-p" 'my-haskell-pointfree haskell-mode-map)
  (bind-key "C-. y" 'insert-counting-putStrLn haskell-mode-map)
  (bind-key "C-. C-y" 'insert-counting-putStrLn haskell-mode-map)

  (ac-define-source ghc-mod
    '((depends ghc)
      (candidates . (ghc-select-completion-symbol))
      (symbol . "s")
      (document . haskell-doc-sym-doc)
      (cache)))

  (use-package auto-complete-etags)

  (setq ac-sources (list 'ac-source-etags
                         ;; 'ac-source-ghc-mod
                         'ac-source-words-in-same-mode-buffers))
  (set (make-local-variable 'yas-fallback-behavior)
       '(apply indent-according-to-mode . nil))
  (bind-key "<tab>" 'yas-expand-from-trigger-key haskell-mode-map)
  (bind-key "<A-tab>" 'ac-complete haskell-mode-map)

  (unbind-key "M-s" haskell-mode-map)
  (unbind-key "M-t" haskell-mode-map)

  (bind-key "C-c C-h" 'my-haskell-hoogle haskell-mode-map)
  (bind-key "A-M-h" 'my-haskell-hoogle haskell-mode-map)
  (bind-key "C-M-x" 'inferior-haskell-send-decl haskell-mode-map)
  (unbind-key "C-x C-d" haskell-mode-map)

  (haskell-chris))

(use-package haskell-cabal
  :mode ("\\.cabal\\'" . haskell-cabal-mode))

(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (progn
    (use-package ghc
      :load-path "site-lisp/ghc-mod/elisp/"
      :commands ghc-init
      :init
      (progn
        (setq ghc-module-command
              (expand-file-name "ghc-mod/cabal-dev/bin/ghc-mod"
                                user-site-lisp-directory))
        (add-hook 'haskell-mode-hook 'ghc-init)))

    (if haskell-config-use-unicode-symbols
        (haskell-setup-unicode-conversions)))

  :config
  (progn
    (setq ghc-hoogle-command hoogle-binary-path)

    (use-package inf-haskell
      :config
      (add-hook 'inferior-haskell-mode-hook
                (lambda ()
                  (set (make-local-variable 'comint-prompt-regexp) "^>>> *"))))

    (use-package haskell-bot :commands haskell-bot-show-bot-buffer)
    (use-package hpaste :commands (hpaste-paste-buffer hpaste-paste-region))
    (use-package helm-hoogle :commands helm-hoogle)
    (use-package hsenv :commands hsenv-activate-fpco)

    (defun my-haskell-hoogle (query &optional arg)
      "Do a Hoogle search for QUERY."
      (interactive
       (let ((def (haskell-ident-at-point)))
         (if (and def (symbolp def)) (setq def (symbol-name def)))
         (list (read-string (if def
                                (format "Hoogle query (default %s): " def)
                              "Hoogle query: ")
                            nil nil def)
               current-prefix-arg)))
      (let ((browse-url-browser-function
             (if (not arg)
                 browse-url-browser-function
               '((".*" . w3m-browse-url)))))
        (if (null haskell-hoogle-command)
            (progn
              (unless (and hoogle-server-process
                           (process-live-p hoogle-server-process))
                (message "Starting local Hoogle server on port 8687...")
                (with-current-buffer (get-buffer-create " *hoogle-web*")
                  (cd temporary-file-directory)
                  (setq hoogle-server-process
                        (start-process "hoogle-web" (current-buffer)
                                       (expand-file-name ghc-hoogle-command)
                                       "server" "--local" "--port=8687")))
                (sleep-for 0 500)
                (message "Starting local Hoogle server on port 8687...done"))
              (browse-url
               (format "http://localhost:8687/?hoogle=%s"
                       (replace-regexp-in-string
                        " " "+"
                        (replace-regexp-in-string "\\+" "%2B" query)))))
          (lexical-let ((temp-buffer (if (fboundp 'help-buffer)
                                         (help-buffer) "*Help*")))
            (with-output-to-temp-buffer temp-buffer
              (with-current-buffer standard-output
                (let ((hoogle-process
                       (start-process "hoogle" (current-buffer)
                                      haskell-hoogle-command query))
                      (scroll-to-top
                       (lambda (process event)
                         (set-window-start
                          (get-buffer-window temp-buffer t) 1))))
                  (set-process-sentinel hoogle-process scroll-to-top))))))))))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(provide 'haskell-config)

;;; haskell-config.el ends here
