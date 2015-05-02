;;
;; My version of the Solarized color theme for Emacs.
;;
;; Basically mix of sellout's and bbatsov's Solarized themes, and of
;; wasamasa's fork of bbatsov's.
;;
;; Why fork?
;;
;;     - I never use Emacs in the terminal, so don't need support for that.
;;     - I want only the original Solarized accent colors, not the additional ones in bbatsov's.
;;     - I want to learn how to change/add colors.
;; 
;; First I was using the official Solarized theme for Emacs by Greg
;; Pfeil. I liked how it uses the colors. Then he changed the setting
;; of light/dark to be based on the background mode of the frame, which
;; I found confusing.
;;
;; So then I tried bbatsov's version. But I liked the way it used the
;; colors much less. And it changed Org mode to a non-fixed width
;; font, which I didn't like at all.
;;
;; Then I installed the IRC client Circe, for which neither sellout
;; nor bbatsov had colors defined. Asking for help installing Circe, I
;; stumbled upon wasamasa's literate Emacs configuration on GitHub. He
;; used a heavily modified fork of bbatsov's theme. I looked at the
;; code, and it seemed cleaner and easier to understand than both
;; sellout's and the original bbatsov's version.
;;
;; So I decided to bite the bullet and make my own theme by mixing sellout's
;; bbatsov's and wasamasa's.
;;
;; Doing this makes a lot of sense, actually. Because getting the
;; color setup right is important. As a programmer, I'm spending *a
;; lot* of time looking at the screen. So I better learn how to
;; customize the colors myself if I want to get them the way I want.
;;
;; See:
;;
;;     * http://ethanschoonover.com/solarized
;;     * https://github.com/sellout/emacs-color-theme-solarized
;;     * https://github.com/bbatsov/solarized-emacs
;;     * https://github.com/wasamasa/dotemacs/blob/master/theme/my-solarized-theme.el
;;

;;
;; Some more info on the Solarized theme and how to use the colors
;; follow.
;;
;; Solarized defines 16 colors -- 8 monotones and 8 accent colors.
;;
;; There are two variants of the theme: light background and dark
;; background. The accent colors are the same for both the light and
;; dark variants. Only five of the eight monotones are used in each
;; variant, respectively.
;;
;; For the dark version, this is the mapping of monotones to
;; variables:
;;
;;   base1   fg-1  Emphasized content
;;   base0   fg-0  Primary content
;;   base01  bg-1  Secondary content
;;   base02  bg-2  Background highlights
;;   base03  bg-3  Background
;;

;;
;; To do:
;;
;; TODO Make a README.org.
;; TODO Comment functions.
;; TODO Add completion mode colors.
;; TODO Add Org-mode colors from sellout (maybe).
;; TODO Add info colors from bbatsov.
;; TODO Add markdown-mode colors from bbatsov.
;; TODO Add web-mode colors from bbatsov.
;; TODO Add note about how to choose accent colors to README.
;;

(deftheme my-solarized "Mix of sellout's, bbatsov's and
wasamasa's fork of bbatsov's Solarized theme.")

(defun my-solarized-toggle ()
  (interactive)
  (if my-solarized-dark?
      (setq my-solarized-dark? nil)
    (setq my-solarized-dark? t))
  (load-theme 'my-solarized t))

(defvar my-solarized-dark? t
  "t for dark, nil for light.")

(defun my-solarized-color (color)
  (cond
   ((eq color 'bg-3) (if my-solarized-dark? "#002b36" "#fdf6e3"))
   ((eq color 'bg-2) (if my-solarized-dark? "#073642" "#eee8d5"))
   ((eq color 'bg-1) (if my-solarized-dark? "#586e75" "#93a1a1"))
   ((eq color 'bg-0) (if my-solarized-dark? "#657b83" "#839496"))
   ((eq color 'fg-0) (if my-solarized-dark? "#839496" "#657b83"))
   ((eq color 'fg-1) (if my-solarized-dark? "#93a1a1" "#586e75"))
   ((eq color 'fg-2) (if my-solarized-dark? "#eee8d5" "#073642"))
   ((eq color 'fg-3) (if my-solarized-dark? "#fdf6e3" "#002b36"))
   ((eq color 'yellow) "#b58900")
   ((eq color 'orange) "#cb4b16")
   ((eq color 'red) "#dc322f")
   ((eq color 'magenta) "#d33682")
   ((eq color 'violet) "#6c71c4")
   ((eq color 'blue) "#268bd2")
   ((eq color 'cyan) "#2aa198")
   ((eq color 'green) "#859900")
   )
  )

(defun my-apply-faces (faces)
  "Helper function that sets the faces of the theme to a list of FACES.
See `my-transform-face' for the transformation, see
`my-transform-spec' for the rules."
  (apply #'custom-theme-set-faces 'my-solarized
         (mapcar #'my-transform-face faces)))

(defun my-transform-face (face)
  "Helper function that transforms a FACE to all variants.
FACE is a list where the first element is the name of the
affected face and the remaining elements specify the face
attributes which are transformed into face attributes for both
graphical and terminal displays. See `my-transform-spec' for the
rules that are applied to the face attributes."
  (let* ((name (car face))
         (spec (cdr face))
	 (graphic-spec (my-transform-spec spec))
	 )
    `(,name ((((type graphic)) ,@graphic-spec)))))

(defun my-transform-spec (spec)
  "Helper function that transforms SPEC. SPEC is a property list
where the values are substituted with colors from
`my-solarized-colors' for keys which are either :foreground
or :background. All other key-value combinations remain
unchanged."
  (let (output)
    (while spec
      (let* ((key (car spec))
             (value (cadr spec))
             (color (my-solarized-color value)))
        (cond
         ((memq key '(:foreground :background :overline :color))
          (setq output (append output (list key color))))
	 ((and (memq key '(:box :underline)) (listp value))
	  (setq output (append output (list key (my-transform-spec value)))))
         (t (setq output (append output (list key value))))))
      (setq spec (cddr spec)))
    output))

;;
;; Here goes the actual face definitions
;;
(my-apply-faces
 '(
   ;; 
   ;; First some basic ones.
   
   (default :foreground fg-0 :background bg-3)
   (cursor :foreground bg-3 :background bg-0)
   (shadow :foreground bg-1)
   (link :foreground violet :underline t)
   (link-visited :foreground magenta :underline t)
   (match :foreground yellow :inverse-video t) ; search
   (error :foreground red :inverse-video t)
   (warning :foreground red :weight bold)
   (success :foreground blue)
   (escape-glyph :foreground red)
   (lazy-highlight :inherit match)
   (fringe :foreground bg-1 :background bg-2)
   (highlight :background bg-2)
   (widget-field :foreground fg-1 :background bg-2 :box (:line-width 1 :color fg-2) :inherit default)
   (widget-single-line-field :inherit widget-field)
   (button :underline t)
   
   (header-line :foreground fg-0 :background bg-2 :weight bold :inverse-video t)
   (menu :foreground fg-0 :background bg-2)
   (minibuffer-prompt :foreground cyan :weight bold)
   (mode-line :foreground fg-1 :background bg-2 :weight bold :inverse-video t :box nil)
   (mode-line-inactive :foreground fg-0 :background bg-2 :weight bold :inverse-video t :box nil) ; FIXME
   (region :foreground bg-1 :background bg-3 :weight bold :inverse-video t)
   (secondary-selection :background bg-2)
   (trailing-whitespace :background red :weight bold :inverse-video t)
   (vertical-border :foreground fg-0)

   (isearch :foreground orange :background bg-3 :inverse-video t)
   (isearch-fail :inherit error)

   ;;
   ;; Then organized by mode, in alphanumeric order.
   
   ;; makefile
   (makefile-space :background magenta)

   ;; circe
   (lui-button-face :foreground blue :underline t)
   (lui-highlight-face :foreground fg-2)
   (lui-time-stamp-face :foreground violet-d :weight bold)

   (circe-originator-face :foreground fg-2)
   (circe-prompt-face :foreground blue :background bg-3)
   (circe-server-face :foreground blue)
   (circe-highlight-nick-face :foreground fg-2 :weight bold)
   (circe-topic-diff-new-face :background green-d)
   (circe-topic-diff-removed-face :background red-d)
   (circe-fool-face :foreground bg-1)

   ;; custom
   (custom-button
    :foreground fg-1 :background bg-2 :box (:line-width 2 :style released-button))
   (custom-button-mouse :inverse-video t :foreground fg-1 :background bg-2 :inherit custom-button)
   (custom-button-pressed :inverse-video t :foreground fg-1 :background bg-2 :box (:line-width 2 :style pressed-button) :inherit custom-button-mouse)
   (custom-changed :inverse-video t :foreground blue :background fg-3)
   (custom-comment :foreground fg-1 :background bg-2)
   (custom-comment-tag :foreground fg-1 :background bg-2)
   (custom-documentation :inherit default)
   (custom-group-tag :foreground fg-1)
   (custom-group-tag-1 :weight bold :foreground fg-1)
   (custom-invalid :inverse-video t :foreground red :background bg-3)
   (custom-link :foreground violet)
   (custom-state :foreground green)
   (custom-variable-tag :foreground fg-1)

   ;; diff
   (diff-added :foreground green :background bg-3)
   (diff-changed :foreground blue :background bg-3)
   (diff-removed :foreground red :background bg-3)
   (diff-header :background bg-3)
   (diff-file-header :background bg-3 :foreground fg-0 :weight bold)
   (diff-refine-added :foreground bg-3 :background green)
   (diff-refine-change :foreground bg-3 :background blue)
   (diff-refine-removed :foreground bg-3 :background red)

   ;; dired
   (dired-directory :foreground blue :weight normal)
   (dired-flagged :foreground red)
   (dired-header :foreground bg-3 :background blue)
   (dired-ignored :inherit shadow)
   (dired-mark :foreground yellow :weight bold)
   (dired-marked :foreground magenta :weight bold)
   (dired-perm-write :foreground fg-0 :underline t)
   (dired-symlink :foreground cyan :weight normal :slant italic)
   (dired-warning :foreground orange :underline t)

   ;; font-lock
   (font-lock-builtin-face :foreground green) ; Statement
   (font-lock-comment-face :slant italic :foreground bg-1)
   (font-lock-constant-face :foreground cyan)
   (font-lock-function-name-face :foreground blue) ; Identifier
   (font-lock-keyword-face :foreground green) ; Statement
   (font-lock-string-face :foreground cyan) ; Constant
   (font-lock-type-face :foreground yellow)
   (font-lock-variable-name-face :foreground blue) ; Identifier
   (font-lock-warning-face :weight bold :foreground red) ; Error
   (font-lock-doc-face :slant italic :foreground bg-1) ; Comment
   (font-lock-doc-string-face :slant italic :foreground bg-1) ; Comment (XEmacs)
   (font-lock-color-constant-face :foreground green)
   (font-lock-comment-delimiter-face italic :foreground bg-1) ; Comment
   (font-lock-preprocessor-face :foreground orange) ; PreProc
   (font-lock-reference-face :foreground cyan)
   (font-lock-negation-char-face :foreground red)
   (font-lock-other-type-face :slant italic :foreground blue)
   (font-lock-regexp-grouping-construct :foreground orange)
   (font-lock-special-keyword-face :foreground red) ; Special
   (font-lock-exit-face :foreground red)
   (font-lock-other-emphasized-face :weight bold :slant italic :foreground violet)
   (font-lock-regexp-grouping-backslash :foreground yellow)
   
   ;; ;; flx
   ;; (flx-highlight-face :foreground blue :weight normal :underline nil)

   ;; grep
   (grep-context-face :foreground fg-0)
   (grep-error-face :foreground red :weight bold :underline t)
   (grep-hit-face :foreground blue)
   (grep-match-face :foreground orange :weight bold)

   ;; hl-line-mode
   (hl-line :background bg-2 :underline nil)

   ;; ;; highlight-indentation
   (highlight-indentation-face :background bg-2)
   (highlight-indentation-current-column-face :background bg-2)

   ;; ido
   (ido-first-match :weight bold :foreground green)
   (ido-only-match :foreground green)
   (ido-subdir :foreground blue)

   ;; linum
    (linum :foreground bg-1 :background bg-2)
   
   ;; magit
   (magit-section-title :foreground yellow :weight bold)
   (magit-branch :foreground orange :weight bold)
   (magit-diff-add :background unspecified :foreground green)
   (magit-diff-del :background unspecified :foreground red)
   (magit-item-highlight :background bg-2 :weight unspecified)
   (magit-log-author :foreground cyan)
   (magit-log-graph :foreground bg-1)
;   (magit-log-head-label-bisect-bad :background red-l :foreground red-d :box (:line-width -1))
;   (magit-log-head-label-bisect-good :background green-l :foreground green-d :box (:line-width -1))
   (magit-log-head-label-default :background bg-2 :box (:line-width -1))
;   (magit-log-head-label-local :background blue-d :foreground blue-l :box (:line-width -1))
;   (magit-log-head-label-patches :background red-d :foreground red-l :box (:line-width -1))
;   (magit-log-head-label-remote :background green-d :foreground green-l :box (:line-width -1))
;   (magit-log-head-label-tags :background yellow-d :foreground yellow-l :box (:line-width -1))
   (magit-log-sha1 :foreground yellow)

   ;; markdown-mode
   (markdown-header-face :foreground green)
   (markdown-header-face-1 :inherit markdown-header-face)
   (markdown-header-face-2 :inherit markdown-header-face)
   (markdown-header-face-3 :inherit markdown-header-face)
   (markdown-header-face-4 :inherit markdown-header-face)
   (markdown-header-face-5 :inherit markdown-header-face)
   (markdown-header-face-6 :inherit markdown-header-face)

   ;; ;; org-mode
   (org-agenda-structure :foreground fg-1 :background bg-2 :weight bold :slant normal :inverse-video nil :underline nil :box (:line-width -1 :color bg-3))
   (org-agenda-calendar-event :foreground fg-1)
   (org-agenda-calendar-sexp :foreground fg-0 :slant italic)
   (org-agenda-date :foreground bg-1 :background bg-3 :weight normal :inverse-video nil :overline nil :slant normal :height 1.0 :box (:line-width -1 :color bg-3) t)
   (org-agenda-date-weekend :inherit org-agenda-date :inverse-video nil :background unspecified :foreground bg-1 :weight unspecified :underline t :overline nil :box unspecified t)
   (org-agenda-date-today :inherit org-agenda-date :inverse-video t :weight bold :underline unspecified :overline nil :box unspecified :foreground blue :background bg-3 t)
   (org-agenda-done :foreground bg-1 :slant italic t)
   (org-archived :foreground bg-1 :weight normal)
   (org-block :foreground bg-1)
   (org-block-begin-line :foreground bg-1 :slant italic)
   (org-checkbox :background bg-3 :foreground fg-0 )
   (org-code :foreground bg-1)
   (org-date :foreground blue :underline t)
   (org-done :weight bold :foreground green)
   (org-ellipsis :foreground bg-1)
   (org-formula :foreground yellow)
   (org-headline-done :foreground green)
   (org-hide :foreground bg-3)
   (org-level-1 :inherit fixed-pitch :foreground orange)
   (org-level-2 :inherit fixed-pitch :foreground green)
   (org-level-3 :inherit fixed-pitch :foreground blue)
   (org-level-4 :inherit fixed-pitch :foreground yellow)
   (org-level-5 :inherit fixed-pitch :foreground cyan)
   (org-level-6 :inherit fixed-pitch :foreground green)
   (org-level-7 :inherit fixed-pitch :foreground red)
   (org-level-8 :inherit fixed-pitch :foreground blue)
   (org-link :foreground yellow :underline t)
   (org-sexp-date :foreground violet)
   (org-scheduled :foreground green)
   (org-scheduled-previously :foreground cyan)
   (org-scheduled-today :foreground blue :weight normal)
   (org-special-keyword :foreground bg-1 :weight bold)
   (org-table :foreground green)
   (org-tag :weight bold)
   (org-time-grid :foreground bg-1)
   (org-todo :foreground red :weight bold)
   (org-upcoming-deadline :foreground yellow  :weight normal :underline nil)
   (org-warning :foreground orange :weight normal :underline nil)
   ;; org-habit (clear=blue ready=green alert=yellow overdue=red. future=lower contrast)
   (org-habit-clear-face :background blue-d :foreground blue-l)
   (org-habit-clear-future-face :background blue-d)
   (org-habit-ready-face :background green-d :foreground green)
   (org-habit-ready-future-face :background green-d)
   (org-habit-alert-face :background yellow :foreground yellow-d)
   (org-habit-alert-future-face :background yellow-d)
   (org-habit-overdue-face :background red :foreground red-d)
   (org-habit-overdue-future-face :background red-d)
   ;; latest additions
   (org-agenda-dimmed-todo-face :foreground bg-1)
   (org-agenda-restriction-lock :background yellow)
   (org-clock-overlay :background yellow)
   (org-column :background bg-2 :strike-through nil :underline nil :slant normal :weight normal :inherit default)
   (org-column-title :background bg-2 :underline t :weight bold)
   (org-date-selected :foreground red :inverse-video t)
   (org-document-info :foreground fg-0)
   (org-document-title :foreground fg-1  :weight bold)
   (org-drawer :foreground cyan)
   (org-footnote :foreground magenta :underline t)
   (org-latex-and-export-specials :foreground orange)
   (org-mode-line-clock-overrun :inherit mode-line :background red)

   ;; outline
   (outline-1 :inherit org-level-1)
   (outline-2 :inherit org-level-2)
   (outline-3 :inherit org-level-3)
   (outline-4 :inherit org-level-4)
   (outline-5 :inherit org-level-5)
   (outline-6 :inherit org-level-6)
   (outline-7 :inherit org-level-7)
   (outline-8 :inherit org-level-8)

   ;; sh-mode
   (sh-quoted-exec :foreground violet :weight bold)
   (sh-escaped-newline :foreground yellow :weight bold)
   (sh-heredoc :foreground yellow :weight bold)

   ;; table
   (table-cell :foreground fg-0 :background bg-3)

   ;; term
   (term-color-black :foreground bg-3 :background bg-2)
   (term-color-red :foreground red :background red-d)
   (term-color-green :foreground green :background green-d)
   (term-color-yellow :foreground yellow :background yellow-d)
   (term-color-blue :foreground blue :background blue-d)
   (term-color-magenta :foreground magenta :background magenta-d)
   (term-color-cyan :foreground cyan :background cyan-d)
   (term-color-white :foreground bg-0 :background fg-0)
   (term-default-fg-color :inherit term-color-white)
   (term-default-bg-color :inherit term-color-black)

   ;; web-mode
   (web-mode-builtin-face :foreground red)
   (web-mode-comment-face :foreground bg-1)
   (web-mode-constant-face :foreground blue :weight bold)
   (web-mode-current-element-highlight-face :underline unspecified :weight unspecified :background bg-2)
   (web-mode-css-at-rule-face :foreground violet :slant italic)
   (web-mode-css-pseudo-class-face :foreground green :slant italic)
   (web-mode-doctype-face :foreground bg-1 :slant italic :weight bold)
   (web-mode-folded-face :underline t)
   (web-mode-function-name-face :foreground blue)
   (web-mode-html-attr-name-face :foreground blue :slant normal)
   (web-mode-html-attr-value-face :foreground cyan :slant italic)
   (web-mode-html-tag-face :foreground green)
   (web-mode-keyword-face :foreground yellow :weight normal)
   (web-mode-preprocessor-face :foreground yellow :slant normal :weight unspecified)
   (web-mode-string-face :foreground cyan)
   (web-mode-type-face :foreground yellow)
   (web-mode-variable-name-face :foreground blue)
   (web-mode-warning-face :inherit font-lock-warning-face)
   (web-mode-block-attr-name-face :inherit web-mode-html-attr-name-face)
   (web-mode-block-attr-value-face :inherit web-mode-html-attr-value-face)
   (web-mode-block-comment-face :inherit web-mode-comment-face)
   (web-mode-block-control-face :inherit font-lock-preprocessor-face)
   (web-mode-block-face :background unspecified)
   (web-mode-block-string-face :inherit web-mode-string-face)
   (web-mode-comment-keyword-face :box (:line-width -1) :weight bold)
   (web-mode-css-color-face :inherit font-lock-builtin-face)
   (web-mode-css-function-face :inherit font-lock-builtin-face)
   (web-mode-css-priority-face :inherit font-lock-builtin-face)
   (web-mode-css-property-name-face :inherit font-lock-variable-name-face)
   (web-mode-css-selector-face :inherit font-lock-keyword-face)
   (web-mode-css-string-face :inherit web-mode-string-face)
   (web-mode-javascript-string-face :inherit web-mode-string-face)
   (web-mode-json-context-face :foreground violet)
   (web-mode-json-key-face :foreground violet)
   (web-mode-json-string-face :inherit web-mode-string-face)
   (web-mode-param-name-face :foreground fg-0)
   (web-mode-part-comment-face :inherit web-mode-comment-face)
   (web-mode-part-face :inherit web-mode-block-face)
   (web-mode-part-string-face :inherit web-mode-string-face)
   (web-mode-symbol-face :foreground yellow)
   (web-mode-whitespace-face :background red)

   ;; whitespace-mode
   (whitespace-space :background unspecified :foreground bg-1 :inverse-video unspecified :slant italic)
   (whitespace-hspace :background unspecified :foreground fg-1 :inverse-video unspecified)
   (whitespace-tab :background unspecified :foreground red :inverse-video unspecified :weight bold)
   (whitespace-newline :background unspecified :foreground bg-1 :inverse-video unspecified)
   (whitespace-trailing :background unspecified :foreground orange :inverse-video t)
   (whitespace-line :background unspecified :foreground magenta :inverse-video unspecified)
   (whitespace-space-before-tab :background red :foreground unspecified :inverse-video unspecified)
   (whitespace-indentation :background unspecified :foreground yellow :inverse-video unspecified :weight bold)
   (whitespace-empty :background unspecified :foreground red :inverse-video t)
   (whitespace-space-after-tab :background unspecified :foreground orange :inverse-video t :weight bold)
   ))

(custom-theme-set-variables
 'my-solarized
 ;; ansi-term
 `(ansi-color-names-vector [,(my-solarized-color 'bg-3)
                            ,(my-solarized-color 'red )
                            ,(my-solarized-color 'green)
                            ,(my-solarized-color 'yellow)
                            ,(my-solarized-color 'blue)
                            ,(my-solarized-color 'magenta)
                            ,(my-solarized-color 'cyan)
                            ,(my-solarized-color 'fg-0)])

 ;; compilation
 '(compilation-message-face 'default)

 ;; magit
 '(magit-use-overlays nil)

 ;; org-mode
 `(org-todo-keyword-faces
   '(("PDNG" . (:background bg-3 :foreground orange :weight bold :slant italic))
     ("TODO" . (:background bg-3 :foreground red :weight bold)))))

(provide-theme 'my-solarized)
