;;; my-github-light-theme.el -- port of tomorrow theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; This file is part of emacs-doom-themes, which provides license
;; information.
;;; Code:

(require 'doom-themes)

(defgroup my-github-light-theme nil
  "Options for the `my-github-light' theme."
  :group 'doom-themes)

(defcustom my-github-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'my-github-light-theme
  :type '(choice integer boolean))

(def-doom-theme my-github-light
  "A light theme based off of GitHub's code viewer."

  ((bg         '("#fcfcfc" "white"   "white"        ))
   (fg         '("#24292f" "#424242" "black"        ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#fafafa" "white"   "white"        ))
   (fg-alt     '("#2e353d" "#2e353d" "brightblack"  ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#f2f2f2" "white"   "white" ))
   (base1      '("#e4e4e4" "#e4e4e4"         ))
   (base2      '("#dedede" "#cccccc"         ))
   (base3      '("#d6d4d4" "#cccccc" "silver"))
   (base4      '("#C0bfbf" "#c0c0c0" "silver"))
   (base5      '("#a3a1a1" "#adadad" "silver"))
   (base6      '("#8a8787" "#949494" "silver"))
   (base7      '("#696769" "#6b6b6b" "silver"))
   (base8      '("#000000" "#000000" "black" ))

   (grey       '("#6f7781" "#6f7781" "silver"))
   (red        '("#cf212e" "#cf212e" "red"))
   (orange     '("#e66401" "#e66401" "brightred"))
   (gold     '("#986801" "#986801" "yellow"))
   (yellow     '("#eab700" "#ffcc00" "yellow"))
   (green      '("#206329" "#206329" "green"))
   (light-green '("#3ba44e" "#3ba44e" "green"))
   (blue       '("#0650ae" "#0650ae" "brightblue"))
   (light-blue '("#3d69da" "#3d69da" "brightblue"))
   (dark-blue  '("#0a3069" "#0a3069" "blue"))
   (teal       '("#44ab91" "#44ab91" "brightblue"))
   (magenta    '("#c678dd" "#c9b4cf" "magenta"))
   (violet     '("#824fdf" "#824fdf" "purple"))
   (brown      '("#963800" "#963800" "brown"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-lighten cyan 0.4))

   ;; face categories
   (highlight      grey)
   (vertical-bar   base3)
   (selection      base1)
   (builtin        blue)
   (comments       grey)
   (doc-comments   dark-blue)
   (constants      red)
   (functions      blue)
   (keywords       red)
   (methods        blue)
   (operators      fg)
   (type           brown)
   (strings        dark-blue)
   (variables      violet)
   (numbers        blue)
   (region         selection)
   (error          red)
   (warning        gold)
   (success        light-green)
   (vc-modified    yellow)
   (vc-added       light-green)
   (vc-deleted     red)

   ;; custom categories
   (org-block-bg             (doom-darken bg 0.03))
   (modeline-bg              `(,(doom-lighten (car bg-alt) 0.4) ,@(cdr base3)))
   (modeline-bg-alt          bg)
   (modeline-bg-inactive     `(,(doom-darken (car bg) 0.04) ,@(cdr base1)))
   (modeline-bg-alt-inactive bg)
   (modeline-fg              fg)
   (modeline-fg-inactive     comments)
   (modeline-fg-alt-inactive comments)

   (-modeline-pad
    (when my-github-light-padded-modeline
      (if (integerp my-github-light-padded-modeline)
          my-github-light-padded-modeline
        4))))

  ;;;; Base theme face overrides
  (((font-lock-doc-face &override) :slant 'italic)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base8)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-highlight :inherit 'bold :background highlight :foreground base0)

   ;;;; doom-modeline
   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-path       :foreground violet :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; ivy
   (ivy-current-match :background region :distant-foreground grey :weight 'ultra-bold)
   (ivy-minibuffer-match-face-1 :foreground base5 :weight 'light)
   (ivy-minibuffer-match-face-2 :inherit 'ivy-minibuffer-match-face-1 :foreground violet :weight 'ultra-bold)
   (ivy-minibuffer-match-face-3 :inherit 'ivy-minibuffer-match-face-2 :foreground blue)
   (ivy-minibuffer-match-face-4 :inherit 'ivy-minibuffer-match-face-2 :foreground red)
   ;;;; org <built-in>
   ((org-block &override)            :background org-block-bg)
   ((org-block-background &override) :background org-block-bg)
   ((org-block-begin-line &override) :background org-block-bg)
   ((org-quote &override)            :background org-block-bg)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground fg)
   ((outline-2 &override) :foreground (doom-lighten fg 0.1))
   ((outline-3 &override) :foreground (doom-lighten fg 0.2))
   ((outline-4 &override) :foreground (doom-lighten fg 0.2))
   ((outline-5 &override) :foreground (doom-lighten fg 0.2))
   ((outline-6 &override) :foreground (doom-lighten fg 0.2))
   ((outline-7 &override) :foreground (doom-lighten fg 0.2))
   ((outline-8 &override) :foreground (doom-lighten fg 0.2))
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground light-blue)
   (rainbow-delimiters-depth-2-face :foreground light-green)
   (rainbow-delimiters-depth-3-face :foreground magenta)
   (rainbow-delimiters-depth-4-face :foreground blue)
   (rainbow-delimiters-depth-5-face :foreground green)
   (rainbow-delimiters-depth-6-face :foreground violet)
   (rainbow-delimiters-depth-7-face :foreground yellow)
   ;;;; solaire-mode
   (solaire-mode-line-face :inherit 'mode-line :background modeline-bg-alt)
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-alt-inactive
    :foreground modeline-fg-alt-inactive)
   ;;;; treemacs
   (treemacs-git-untracked-face :foreground yellow)
   ;;;; whitespace <built-in>
   (whitespace-tab :background (doom-lighten base0 0.6)
                   :foreground comments))

  ;; --- variables --------------------------
  ;; ()
  )

;;; my-github-light-theme.el ends here
