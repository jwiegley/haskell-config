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

(defcustom haskell-config-use-unicode-symbols nil
  "If non-nil, use Unicode symbols to represent mathematical operators."
  :type 'boolean
  :group 'haskell)

(defface haskell-subscript '((t :height 0.6))
 "Face used for subscripts."
 :group 'haskell)

(use-package haskell-cabal
  :mode ("\\.cabal\\'" . haskell-cabal-mode))

(use-package haskell-mode
  :mode (("\\.hsc?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (when haskell-config-use-unicode-symbols
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
      (let ((conv-chars '(("[ (]\\(->\\)[) \n]"     . ?→)
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
                          ("\\<\\(forall \\)\\>"   . ?∀))))
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
                                 conv-chars)
                         '(("(\\|)" . 'esk-paren-face)
                           ;; ("\\<[a-zA-Z]+\\([0-9]\\)\\>"
                           ;;  1 haskell-subscript)
                           ))))
              '(haskell-mode literate-haskell-mode)))))

  :config
  (progn
    (load "haskell-site-file")

    (defcustom hoogle-binary-path (expand-file-name "~/.cabal/bin/hoogle")
      "Path to the local 'hoogle' binary."
      :type 'file
      :group 'haskell)

    (use-package inf-haskell
      :init
      (progn
        (defun my-haskell-load-and-run ()
          "Loads and runs the current Haskell file."
          (interactive)
          (inferior-haskell-load-and-run inferior-haskell-run-command)
          (sleep-for 0 100)
          (end-of-buffer))

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
                     (file-name-nondirectory (buffer-file-name)) line col)))))

    (use-package ghc
      :load-path "site-lisp/ghc-mod/elisp/"
      :commands ghc-init
      :init
      (progn
        (setq ghc-module-command
              (expand-file-name "ghc-mod/cabal-dev/bin/ghc-mod"
                                user-site-lisp-directory))
        (add-hook 'haskell-mode-hook 'ghc-init))

      :config
      (setq ghc-hoogle-command hoogle-binary-path))

    (use-package haskell-bot
      :commands haskell-bot-show-bot-buffer)

    (use-package hpaste
      :commands (hpaste-paste-buffer hpaste-paste-region))

    (use-package helm-hoogle
      :commands helm-hoogle)

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

    (defvar hoogle-server-process nil)

    (defun haskell-hoogle (query &optional arg)
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
               (format "http://localhost:8687/?hoogle=%s&exact=1"
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
                  (set-process-sentinel hoogle-process scroll-to-top))))))))

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
            (haskell-hoogle sym arg)
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

    (defun my-haskell-mode-hook ()
      (auto-complete-mode 1)
      (whitespace-mode 1)
      (bug-reference-prog-mode 1)

      (set (make-local-variable 'comint-prompt-regexp) "^>>> *")

      ;; (add-hook 'after-save-hook 'check-parens nil t)

      (turn-on-haskell-indentation)
      (turn-on-font-lock)
      (turn-on-haskell-decl-scan)

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

      (ghc-init)

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
                             (haskell-move-nested -1))
                haskell-mode-map)

      (bind-key "C-<right>" (lambda ()
                              (interactive)
                              (haskell-move-nested 1))
                haskell-mode-map)

      (bind-key "C-c C-u" (lambda ()
                            (interactive)
                            (insert "undefined"))
                haskell-mode-map)

      (defun killall-hdevtools ()
        (interactive)
        (shell-command "killall hdevtools")
        (flyparse-buffer))

      (bind-key "C-x SPC" 'my-inferior-haskell-break haskell-mode-map)
      (bind-key "C-h C-i" 'my-inferior-haskell-find-haddock haskell-mode-map)
      (bind-key "C-c C-b" 'haskell-bot-show-bot-buffer haskell-mode-map)
      (bind-key "C-c C-d" 'ghc-browse-document haskell-mode-map)
      (bind-key "C-c C-k" 'inferior-haskell-kind haskell-mode-map)
      (bind-key "C-c C-r" 'inferior-haskell-load-and-run haskell-mode-map)
      (bind-key "C-c C-c" 'flyparse-buffer haskell-mode-map)
      (bind-key "C-c C" 'killall-hdevtools haskell-mode-map)

      (use-package haskell-edit)
      (bind-key "M-q" 'haskell-edit-reformat haskell-mode-map)

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

      (bind-key "C-c C-s" 'ghc-insert-template haskell-mode-map)
      (bind-key "C-c C-p" 'my-haskell-pointfree haskell-mode-map)

      (ac-define-source ghc-mod
        '((depends ghc)
          (candidates . (ghc-select-completion-symbol))
          (symbol . "s")
          (document . haskell-doc-sym-doc)
          (cache)))

      (setq ac-sources (list 'ac-source-words-in-same-mode-buffers
                             'ac-source-ghc-mod))
      (set (make-local-variable 'yas/fallback-behavior)
           '(apply indent-according-to-mode . nil))
      (bind-key "<tab>" 'yas/expand-from-trigger-key haskell-mode-map)
      (bind-key "<A-tab>" 'ac-complete haskell-mode-map)

      (unbind-key "M-s" haskell-mode-map)
      (unbind-key "M-t" haskell-mode-map)

      (bind-key "A-M-h" 'hoogle-local haskell-mode-map)
      (bind-key "C-M-x" 'inferior-haskell-send-decl haskell-mode-map)
      (unbind-key "C-x C-d" haskell-mode-map))

    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

    (add-hook 'inferior-haskell-mode-hook
              (lambda ()
                (set (make-local-variable 'comint-prompt-regexp) "^>>> *")))))

(provide 'haskell-config)

;;; haskell-config.el ends here
