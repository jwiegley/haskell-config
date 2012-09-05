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

(use-package haskell-mode
  :mode (("\\.hsc?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (if haskell-config-use-unicode-symbols
      (let ((conv-chars '(("[ (]\\(->\\)[) \n]"     . ?→)
                          ("[ (]\\(/=\\)[) ]"       . ?≠)
                          ("[ (]\\(<=\\)[) ]"       . ?≤)
                          ("[ (]\\(>=\\)[) ]"       . ?≥)
                          ("[ (]\\(=\\)[) ]"        . ?≡)
                          ("[ (]\\(\\.\\)[) ]"      . ?∘)
                          ("[ (]\\(&&\\)[) ]"       . ?∧)
                          ("[ (]\\(||\\)[) ]"       . ?∨)
                          ("[ (]\\(\\*\\)[) ]"      . ?×)
                          ("[ (]\\(\\\\\\)[(_a-z]"  . ?λ)
                          (" \\(<-\\)[ \n]"         . ?←)
                          (" \\(-<\\) "             . ?⤙)
                          (" \\(=>\\)[ \n]"         . ?⇒)
                          ;;(" \\(>=>\\) "           . ?↣)
                          ;;(" \\(<=<\\) "           . ?↢)
                          ;;(" \\(>>=\\) "           . ?↦)
                          ;;(" \\(=<<\\) "           . ?↤)
                          ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)
                          ("[ (]\\(<<<\\)[ )]"      . ?⋘)
                          ("[ (]\\(>>>\\)[ )]"      . ?⋙)
                          (" \\(::\\) "             . ?∷)
                          ("\\(`union`\\)"          . ?⋃)
                          ("\\(`intersect`\\)"      . ?⋂)
                          ("\\(`elem`\\)"           . ?∈)
                          ("\\(`notElem`\\)"        . ?∉)
                          ;;("\\<\\(mempty\\)\\>"    . ??)
                          ("\\(`mappend`\\)"        . ?⨂)
                          ("\\(`msum`\\)"           . ?⨁)
                          ("\\(\\<alpha\\>\\)"      . ?α)
                          ("\\(\\<beta\\>\\)"       . ?β)
                          ("\\(\\<delta\\>\\)"      . ?δ)
                          ("\\(\\<theta\\>\\)"      . ?θ)
                          ("\\(\\<undefined\\>\\)"  . ?⊥)
                          ("\\<\\(forall\\)\\> "    . ?∀))))
        (mapc (lambda (mode)
                (font-lock-add-keywords
                 mode
                 (append (mapcar (lambda (chars)
                                   `(,(car chars)
                                     (0 (ignore
                                         (compose-region (match-beginning 1)
                                                         (match-end 1)
                                                         ,(cdr chars))))))
                                 conv-chars)
                         '(("(\\|)" . 'esk-paren-face)
                           ;; ("\\<[a-zA-Z]+\\([0-9]\\)\\>"
                           ;;  1 haskell-subscript)
                           ))))
              '(haskell-mode literate-haskell-mode))))

  :config
  (progn
    (use-package inf-haskell
      :config
      (progn
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
          (message (inferior-haskell-type expr insert-value)))))

    (use-package ghc
      :load-path "site-lisp/ghc-mod/elisp/"
      :commands ghc-init
      :init
      (progn
        (setq ghc-module-command (expand-file-name "~/.cabal/bin/ghc-mod")
              ghc-hoogle-command (expand-file-name "~/.cabal/bin/hoogle"))
        (add-hook 'haskell-mode-hook 'ghc-init)))

    (use-package scion
      :disabled t
      :load-path "site-lisp/scion/emacs/"
      :init
      (progn
        ;; if ./cabal/bin is not in your $PATH
        (setq scion-program (expand-file-name "~/.cabal/bin/scion-server"))

        ;; Use ido-mode completion (matches anywhere, not just beginning)
        ;;
        ;; WARNING: This causes some versions of Emacs to fail so badly that
        ;; Emacs needs to be restarted.
        (setq scion-completing-read-function 'ido-completing-read)))

    (defcustom hoogle-binary-path (expand-file-name "~/.cabal/bin/hoogle")
      "Path to the local 'hoogle' binary."
      :type 'file
      :group 'haskell)

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

    (defun haskell-hoogle (query)
      "Do a Hoogle search for QUERY."
      (interactive
       (let ((def (haskell-ident-at-point)))
         (if (and def (symbolp def)) (setq def (symbol-name def)))
         (list (read-string (if def
                                (format "Hoogle query (default %s): " def)
                              "Hoogle query: ")
                            nil nil def))))
      (if (null haskell-hoogle-command)
          (progn
            (unless (and hoogle-server-process
                         (process-live-p hoogle-server-process))
              (message "Starting local Hoogle server on port 8687...")
              (with-current-buffer (get-buffer-create " *hoogle-web*")
                (cd temporary-file-directory)
                (setq hoogle-server-process
                      (start-process "hoogle-web" (current-buffer)
                                     (expand-file-name "~/.cabal/bin/hoogle")
                                     "server" "--local" "--port=8687")))
              (sleep-for 0 500)
              (message "Starting local Hoogle server on port 8687...done"))
            (browse-url (format "http://localhost:8687/?hoogle=%s" query)))
        (lexical-let ((temp-buffer (if (fboundp 'help-buffer)
                                       (help-buffer) "*Help*")))
          (with-output-to-temp-buffer temp-buffer
            (with-current-buffer standard-output
              (let ((hoogle-process
                     (start-process "hoogle" (current-buffer)
                                    haskell-hoogle-command query))
                    (scroll-to-top
                     (lambda (process event)
                       (set-window-start (get-buffer-window temp-buffer t) 1))))
                (set-process-sentinel hoogle-process scroll-to-top)))))))

    (defun my-haskell-mode-hook ()
      (whitespace-mode 1)

      (require 'align)
      (add-to-list 'align-rules-list
                   '(haskell-types
                     (regexp . "\\(\\s-+\\)::\\s-+")
                     (modes quote (haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-assignment
                     (regexp . "\\(\\s-+\\)=\\s-+")
                     (modes quote (haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-arrows
                     (regexp . "\\(\\s-+\\)->\\s-+")
                     (modes quote (haskell-mode literate-haskell-mode))))
      (add-to-list 'align-rules-list
                   '(haskell-left-arrows
                     (regexp . "\\(\\s-+\\)<-\\s-+")
                     (modes quote (haskell-mode literate-haskell-mode))))

      (require 'haskell-align-imports)
      (require 'haskell-sort-imports)
      (require 'haskell-move-nested)

      (bind-key "C-<left>" (lambda ()
                             (interactive)
                             (haskell-move-nested -1))
                haskell-mode-map)

      (bind-key "C-<right>" (lambda ()
                              (interactive)
                              (haskell-move-nested 1))
                haskell-mode-map)

      (when (featurep 'inf-haskell)
        (bind-key "C-c C-d" 'my-inferior-haskell-find-haddock haskell-mode-map)
        (bind-key "C-c C-i" 'inferior-haskell-info haskell-mode-map)
        (bind-key "C-c C-k" 'inferior-haskell-kind haskell-mode-map)
        ;; Use C-u C-c C-t to auto-insert a function's type above it
        (bind-key "C-c C-t" 'my-inferior-haskell-type haskell-mode-map)

        (bind-key "M-." 'my-inferior-haskell-find-definition haskell-mode-map))

      (when (featurep 'ghc)
        (bind-key "C-c C-s" 'ghc-insert-template haskell-mode-map)
        (bind-key "A-<tab>" 'ghc-complete haskell-mode-map))

      (when (featurep 'scion)
        ;; Whenever we open a file in Haskell mode, also activate Scion
        (scion-mode 1)
        ;; Whenever a file is saved, immediately type check it and highlight
        ;; errors/warnings in the source.
        (scion-flycheck-on-save 1))

      (unbind-key "M-t" haskell-mode-map)

      (bind-key "A-M-h" 'hoogle-local haskell-mode-map)
      (bind-key "C-M-x" 'inferior-haskell-send-decl haskell-mode-map)
      (unbind-key "C-x C-d" haskell-mode-map)

      (setq haskell-saved-check-command haskell-check-command)
      (flymake-mode 1)

      (bind-key "C-c w" 'flymake-display-err-menu-for-current-line
                haskell-mode-map)
      (bind-key "C-c *" 'flymake-start-syntax-check haskell-mode-map)
      (bind-key "M-n" 'flymake-goto-next-error haskell-mode-map)
      (bind-key "M-p" 'flymake-goto-prev-error haskell-mode-map))

    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)))

(provide 'haskell-config)

;;; haskell-config.el ends here
