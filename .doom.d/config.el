(require 'auto-dark)
(setq auto-dark--allow-osascript t)
(setq auto-dark--dark-theme 'doom-zenburn)
(setq auto-dark--light-theme 'my-github-light)

(setq +modeline-height 25)

(setq confirm-kill-emacs nil)

(defun my/set-initial-frame-size ()
  "Set the initial frame size to something reasonable. Works on multiple monitors"
  (interactive)
  (let* ((base-factor 0.93)
         (monitor-w (nth 2 (frame-monitor-workarea (selected-frame))))
         (monitor-h (nth 3 (frame-monitor-workarea (selected-frame))))
         (a-width (* monitor-w base-factor))
         (a-height (* monitor-h base-factor)))
    (set-frame-size (selected-frame)
                    (truncate a-width)
                    (truncate a-height) t)))

(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                           frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(defun my/set-initial-frame ()
  (interactive)
  (my/set-initial-frame-size)
  (my/frame-recenter))

(setq frame-resize-pixelwise t)
(add-hook 'window-setup-hook #'my/set-initial-frame)

;; Convert the emacs-e.svg to a png: `rsvg-convert -h 256 emacs-e.svg > emacs-e.png`
;; (setq fancy-splash-image
;;       (concat doom-private-dir "splash/" "emacs-e.png"))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(bind-keys* ("C-h" . evil-window-left)
            ("C-j" . evil-window-down)
            ("C-k" . evil-window-up)
            ("C-l" . evil-window-right))

(map! :map general-override-mode-map
      :nvm "C-c SPC" #'doom/leader
      :ei "C-c SPC" #'doom/leader)

(setq-default left-margin-width 2 right-margin-width 0)

(setq mac-mouse-wheel-smooth-scroll t)

(add-hook 'after-init-hook 'global-visual-line-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq evil-escape-key-sequence nil)

;; (defun my/evil-delete-advice (orig-fn beg end &optional type _ &rest args)
;;     "Make d, c, x to not write to clipboard."
;;     (apply orig-fn beg end type ?_ args))
;; (advice-add 'evil-delete :around 'my/evil-delete-advice)
;; (advice-add 'evil-change :around 'my/evil-delete-advice)

(bind-keys* ("C-c l" . evil-window-vsplit))
(bind-keys* ("C-c j" . evil-window-split))

(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; (bind-key "C-c F" 'toggle-frame-maximized)

;; C-c c will contain all buffer-related bindings
;; (global-set-key (kbd "C-c b") 'my/buffer)

;; (defalias 'my/buffer
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "k") 'kill-buffer-and-window)
;;     map) "Buffer-related bindings")

;; "C-c f" for files/folders
;; (global-set-key (kbd "C-c f") 'my/files)

;; (defalias 'my/files
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "t") 'my/treemacs)
;;     map) "Files-related bindings")

;; "C-c f" for "files" and "folders"
;; (global-set-key (kbd "C-c f") 'my/treemacs)

;; (defalias 'my/treemacs
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "t") 'treemacs) ;; Toggle treemacs
;;     (define-key map (kbd "a") 'treemacs-add-project-to-workspace)
;;     (define-key map (kbd "r") 'treemacs-remove-project-from-workspace)
;;     map) "Treemacs-related bindings")

(bind-keys* ("C-c d" . +lookup/documentation))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(setq which-key-idle-delay 0.2)

(global-set-key (kbd "C-c m") 'my/<localleader>)

(defun my/bind-python-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; REPL
      (define-key map (kbd "R") #'run-python)
      ;; Restart python
      (define-key map (kbd "r") #'pyvenv-restart-python)
      ;; Virtual environment
      (define-key map (kbd "v") #'pyvenv-activate)
      ;; Format
      (define-key map (kbd "f") #'+format/buffer)
      ;; LSP-related bindings
      ;; (define-key map (kbd "l") #'my/lsp)
      ;; Jupyter-related bindings
      (define-key map (kbd "j") #'my/jupyter)
      map)))

(add-hook 'python-mode-hook 'my/bind-python-keys)

;; (defun my/bind-python-lsp-keys ()
;;   (defalias 'my/lsp
;;     (let ((map (make-sparse-keymap)))
;;       ;; Restart lsp server
;;       (define-key map (kbd "r") #'lsp-workspace-restart)
;;       ;; Find definition
;;       (define-key map (kbd "f") #'lsp-find-definition)
;;       ;; Look at documentation
;;       (define-key map (kbd "d") #'lsp-ui-doc-show)
;;       map)))

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (add-hook 'lsp-mode-hook
;;                        #'my/bind-python-lsp-keys)))

(defun my/bind-python-jupyter-keys ()
  (defalias 'my/jupyter
    (let ((map (make-sparse-keymap)))
      ;; Run jupyter REPL associated with current buffer
      (define-key map (kbd "R") #'jupyter-repl-associate-buffer)
      ;; Restart jupyter REPL
      (define-key map (kbd "r") #'jupyter-repl-restart-kernel)
      map)))

(add-hook 'python-mode-hook #'my/bind-python-jupyter-keys)

(defun my/bind-ess-r-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; REPL
      (define-key map (kbd "R") #'run-ess-r)
      map)))

(add-hook 'ess-r-mode-hook 'my/bind-ess-r-keys)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; TODO

(defun my/org-sort-todo-list ()
  "Sort an org-mode todo-list from the heading of the list by priority and todo order."
  (interactive)
  (org-sort-entries nil ?p nil nil nil nil)
  (org-sort-entries nil ?o nil nil nil nil))


(defun my/org-sort-todo-list-from-within ()
  "Sort an org-mode todo-list from within the list by priority and todo order."
  (interactive)
  ;; Move up a heading
  (outline-up-heading 1)
  ;; Sort once at top of heading
  (my/org-sort-todo-list))

(defun my/bind-org-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; Sort todo list by priority and by todo order
      (define-key map (kbd "s") #'my/org-sort-todo-list-from-within)
      map)))

(add-hook 'org-mode-hook 'my/bind-org-keys)

(defun my/bind-markdown-keys ()
  (defalias 'my/<localleader>
    (let ((map (make-sparse-keymap)))
      ;; Format markdown table
      (define-key map (kbd "f") #'markdown-table-align)
      ;; Refresh toc
      (define-key map (kbd "r") #'markdown-toc-refresh-toc)
      map)))

(add-hook 'markdown-mode-hook 'my/bind-markdown-keys)

(setq lsp-ui-doc-max-height 30)
(setq lsp-ui-doc-max-width 90)

(setq lsp-signature-auto-activate nil)
(setq lsp-eldoc-render-all nil)

(defun my/python-lsp-ignore-venv ()
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv_*"))
(add-hook 'python-mode-hook
          '(lambda () (add-hook 'lsp-mode-hook 'my/python-lsp-ignore-venv)))

(defun my/venv_pattern ()
  "Virtual environment pattern"
  "venv*")

(defun my/venv_directories_to_search ()
  "List of directories in which to search for `my/venv_pattern`"
  ;; Remove all nil elements
  (seq-filter (lambda (element) element)
              (list
               (file-name-directory (buffer-file-name))
               (projectile-project-root))))

(defun my/get-matching-directory-files (directory regexp)
  "Find all files in DIRECTORY that begin with REGEXP"
  ;; Concatenate the directory to the filename to recover the full path
  (seq-map (lambda ( file ) (file-name-concat directory file))
           ;; Filter out all files that do not begin with REGEXP
           (seq-filter
            (lambda (x) (equal 0 (string-match-p regexp x)))
            (directory-files directory))))

(defun my/get-matching-directories-files (directories regexp)
  "Find all files in all directories within DIRECTORIES that begin with REGEXP"
  ;; remove duplicates
  (seq-uniq
   ;; flatten the list of lists
   (flatten-tree
    ;; Apply my/get-matching-directory-files to all the directories with regexp.
    (seq-map '(lambda ( dir ) (my/get-matching-directory-files dir regexp)) directories))))

(defun my/get-matching-project-root-files (regexp)
  "Find all root directories/files that begin with REGEXP"
  (my/get-matching-directory-files (projectile-project-root) regexp))

(defun my/python-venv-auto-activate ()
  "Activate the virtual environment satisfying the pattern given by the function, my/venv_pattern if it's a unique match, otherwise do nothing"
  (interactive)
  (setq matching-venvs
        (my/get-matching-directories-files
         (my/venv_directories_to_search) (my/venv_pattern)))
  ;; If we have found a uniquely matching virtual environment, activate it.
  (if (equal (length matching-venvs) 1) ;; if
      (pyvenv-activate (car matching-venvs))
    ;; If there is no matching virtual environment, warn the user.
    (if (equal (length matching-venvs) 0)
        (display-warning :warning "No virtual environment found.")
      ;; If there is more than one matching virtual environment, warn the user.
      (display-warning :warning (concat "Found multiple venvs. Please select one manually using `pyvenv-activate`.")))))

(add-hook 'find-file-hook
          #'(lambda () (when (string= (file-name-extension buffer-file-name) "py")
                        (my/python-venv-auto-activate))))
;; (add-hook 'python-mode-hook #'my/python-venv-auto-activate)

(add-hook 'jupyter-repl-mode-hook (lambda () (setq jupyter-repl-echo-eval-p t)))

;; (add-hook 'polymode-minor-mode-hook #'doom-mark-buffer-as-real-h)
;; (add-to-list 'auto-mode-alist
;;              '("\\.Rmd\\'" . (lambda ()
;;                                ;; add major mode setting here, if needed, for example:
;;                                ;; (text-mode)
;;                                ;; (insert "OK")
;;                                (doom-mark-buffer-as-real-h))))

(define-innermode poly-text-R-innermode
  :indent-offset 2
  :head-matcher (cons "^[ \t]*\\(```[ \t]*{?[[:alpha:]].*\n\\)" 1)
  :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
  :mode 'ess-r-mode
  :head-mode 'host
  :tail-mode 'host)
(define-polymode poly-text-R-mode
  :hostmode 'pm-host/text
  :innermodes '(poly-text-R-innermode))

(defun my/latexmk ()
  (interactive)
  (TeX-command "LatexMk" #'TeX-master-file nil))

;; (defun my/bibtex ()
;;   (interactive)
;;   (TeX-command "BibTeX" #'TeX-master-file nil))

;; (defun my/latex-view ()
;;     (interactive)
;;   (TeX-command "View" #'TeX-master-file nil))

;; (defun my/bind-latex-keys ()
;;   (defalias 'my/<localleader>
;;     (let ((map (make-sparse-keymap)))
;;       ;; Compile
;;       (define-key map (kbd "c") #'my/latexmk)
;;       ;; Recompile BibTeX
;;       (define-key map (kbd "b") #'my/bibtex)
;;       ;; Word count
;;       (define-key map (kbd "w") #'tex-count-words)
;;       map)))

;; (add-hook 'LaTeX-mode-hook 'my/bind-latex-keys)

;; (defun my/latex-format-environment-on-save ()
;;   (add-hook 'after-save-hook #'LaTeX-fill-environment))

;; Run LatexMk after saving .tex files
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-hook 'after-save-hook 'my/latexmk)))

(setq +latex-viewers '(skim))

(setq-default TeX-master nil)

(setq TeX-electric-sub-and-superscript nil)

(after! git-gutter
  (setq git-gutter:disabled-modes '(latex-mode)))

(after! tex
  (remove-hook 'TeX-update-style-hook #'rainbow-delimiters-mode))

(setq
 org-directory
 "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

(custom-set-faces '(org-level-1 ((t (:inherit outline-1 :height 1.2)))))

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(after! org
  (setq org-todo-keywords
        '((sequence "IN-PROGRESS(p)" "TODO(t)" "WAITING(w)"
                    "IDEA(i)" "|" "DONE" "CANCELLED(c)"))))

;; Set other todo colors according to the nord theme (https://www.nordtheme.com/)
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "#88C0D0")
        ("WAITING" . "#5E81AC")
        ("IDEA" . "#EBCB8B")
        ("CANCELED" . "#BF616A"))
      )

(setq org-log-done 'time)

(remove-hook 'org-mode-hook #'org-superstar-mode)

(setq
 org-roam-directory
 "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")

(setq org-roam-db-update-method 'immediate)

(setq org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "${slug}" :head "#+title: ${title}\n" :unnarrowed t)))

(global-set-key (kbd "C-c n") 'my/notes)

(defalias 'my/notes
  (let ((map (make-sparse-keymap)))
    ;; Open org-roam buffer
    (define-key map (kbd "r") #'org-roam)
    ;; Find file
    (define-key map (kbd "f") #'org-roam-find-file)
    ;; Capture
    (define-key map (kbd "c") #'org-roam-capture)
    ;; Insert link
    (define-key map (kbd "i") #'org-roam-insert)
    ;; Insert link immediate
    (define-key map (kbd "i") #'org-roam-insert-immediate)
    map))

(global-set-key (kbd "C-c s") 'my/spelling)

(defun my/bind-spell-fu-bindings ()
  (defalias 'my/spelling
    (let ((map (make-sparse-keymap)))
      ;; Add word to dictionary
      (define-key map (kbd "a") #'spell-fu-word-add)
      map)))

(add-hook 'spell-fu-mode-hook 'my/bind-spell-fu-bindings)

(global-set-key (kbd "C-c y") 'my/yasnippet)

(defalias 'my/yasnippet
    (let ((map (make-sparse-keymap)))
      ;; Add word to dictionary
      (define-key map (kbd "i") #'yas-insert-snippet)
      (define-key map (kbd "e") #'+snippets/find)
      ;; Disabling the yas-new-snippet shortcut for now since it's broken (bug in Doom Emacs (https://github.com/hlissner/doom-emacs/issues/4330))
      ;; (define-key map (kbd "n") #'yas-new-snippet)
      map))

(setq documents-directory "~/Documents")
(setq cloud-directory "~/Box Sync")

(defun my/goto-private-config-org-file ()
  "Open your private config.org file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-private-dir)))

(defun my/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.el" doom-private-dir)))

(defun my/goto-private-init-file ()
  "Open your private init.el file."
  (interactive)
  (find-file (expand-file-name "init.el" doom-private-dir)))

(defun my/goto-private-packages-file ()
  "Open your private packages.el file."
  (interactive)
  (find-file (expand-file-name "packages.el" doom-private-dir)))

(defun my/open-private-themes-directory ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "themes" doom-private-dir)))

(defun my/open-documents-directory ()
  (interactive)
  (ido-find-file-in-dir documents-directory))

(defun my/open-gitprojects-directory ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "GitProjects" documents-directory)))

(defun my/open-papers-directory ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "GitProjects/papers" documents-directory)))

(defun my/open-cloud-unencrypted-directory ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "unencrypted" cloud-directory)))

(defun my/open-org-directory ()
  (interactive) (ido-find-file-in-dir org-directory))

;; C-c o will be reserved for opening files/directories
(global-set-key (kbd "C-c o") 'my/open)
;; (global-set-key (kbd "C-c e") 'my/emacs-config)

(defalias 'my/open
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'my/emacs-config)
    (define-key map (kbd "t") #'my/open-private-themes-directory)
    (define-key map (kbd "o") #'my/open-org-directory)
    (define-key map (kbd "d") #'my/open-documents-directory)
    (define-key map (kbd "g") #'my/open-gitprojects-directory)
    (define-key map (kbd "p") #'my/open-papers-directory)
    (define-key map (kbd "c") #'my/open-cloud-unencrypted-directory)
    map) "Config-related bindings")

(defalias 'my/emacs-config
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'my/goto-private-config-org-file)
    (define-key map (kbd "C") #'my/goto-private-config-file)
    (define-key map (kbd "i") #'my/goto-private-init-file)
    (define-key map (kbd "p") #'my/goto-private-packages-file)
    map) "Config-related bindings")

(setq documents-directory "~/Documents")
(setq cloud-directory "~/Box Sync")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Roboto Mono" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Roboto Mono" :size 14))

(after! company
  (bind-keys* ("C-SPC" . company-complete)))

(after! company
  (setq company-minimum-prefix-length 2))

(after! company
  (setq company-frontends (nconc '(company-pseudo-tooltip-frontend)
                                 (remove 'company-pseudo-tooltip-unless-just-one-frontend
                                         (remove 'company-preview-if-just-one-frontend company-frontends)))))

(after! company
  (setq company-idle-delay 0.05))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (company-mode)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (company-mode)
            (make-local-variable 'company-backends)
            (setq company-backends
                  '(company-files
                    company-reftex-labels
                    company-reftex-citations
                    company-bibtex
                    company-auctex-macros
                    company-auctex-symbols
                    company-auctex-environments
                    ;; company-keywords
                    company-latex-commands
                    company-math-symbols-latex
                    ;; :with
                    company-yasnippet))))

(add-hook 'org-mode-hook
          (lambda ()
            (company-mode)
            (make-local-variable 'company-backends)
            (setq company-backends
                  '(company-capf
                    company-files
                    company-yasnippet
                    company-dabbrev))))

(add-hook 'python-mode-hook
          (lambda ()
            (company-mode)
            (make-local-variable 'company-backends)
            (setq company-backends
                  '(company-files
                    company-capf
                    :with
                    '(company-yasnippet company-dabbrev-code)))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Waudby-Smith"
      user-mail-address "iwaudbysmith@gmail.com")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
