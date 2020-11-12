;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     csv
     html
     yaml
     ; mu4e
     ;; twitter
     racket
     scheme ;; sometimes useful. usually prefer racket.
     ;; javascript
     clojure ;; good
     haskell ;; good, though idris prob. better
     ;; smex ;; how useful is this?
     markdown ;; for when it comes-up
     asciidoc ;; good
     ;; python
     ivy
     auto-completion
     ;; better-defaults ;; when did i install this?
     ;; emacs-lisp ;; is this not included?
     git
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     version-control) ;; how does the version-control config layer differ from the git config layer?
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#disabling-layer-services-in-other-layers
   ;; for blacklisting or whitelisting layer interactions [use of optional deps]
   dotspacemacs-additional-packages '(;; themes
                                      base16-theme
                                      gruvbox-theme ;; red orange green blue on gray. solarized-gruvbox-dark is gray & yellow-orange
                                      gotham-theme
                                      jazz-theme ;; like trim-yer-beard. orange, red, light blue and green, on gray
                                      twilight-anti-bright-theme
                                      moe-theme ;; raindow on gray
                                      ;; leuven-theme ;; supposedly amazing for org mode
                                      color-theme-sanityinc-tomorrow
                                      flatland-theme ;; projekt202 colors

                                      ;; themes like win98 high contrast
                                      borland-blue-theme
                                      cyberpunk-2019-theme
                                      dark-mint-theme

                                      cherry-blossom-theme ;; black bg w/poppin' violets, roses, & sakura
                                      constant-theme ;; very cool war machine colors (grays, white & deep cyans)
                                      green-phosphor-theme ;; bright greens on dark green bg
                                      green-screen-theme ;; greens on black bg
                                      minsk-theme ;; very cool! gunmetal-green with good line highlight

                                      display-theme ;; on mode line

                                      ;; general tools. commented b/c i never use these.
                                      ;; tldr
                                      ;; ripgrep

                                      ;; python tools. all commented b/c not currently using python.
                                      ; ipdb ;; not found
                                      ;; elpy ; main tool. provides error checking via flymake, but use flycheck instead.
                                      ;; epc
                                      ;; importmagic ;; what's this?
                                      ; jupyter-console ;; not found
                                      ;; lisp tools
                                      scheme-complete ;; is this used by company? is it used for racket, ... or which schemes?
                                      ;; misc
                                      nyan-mode
                                      vterm
                                      (centaur-tabs :demand) ;; what's :demand do?
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists nil ;; TODO: remove the startup buffer entirely
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list
   dotspacemacs-themes '(base16-solarflare
                         base16-framer ;; bright text on graphite bg
                         base16-helios
                         moe-dark
                         twilight-anti-bright
                         base16-seti ;; blue trim. dark gray bg. red, yellow, green, orange, cyan, purple
                         base16-hopscotch
                         base16-atelier-cave ;; pale purple bg
                         base16-unikitty-dark ;; cave w/lighter bg
                         base16-material-darker ;; more colorful
                         base16-brogrammer

                         ;; like win98 high contrast
                         borland-blue
                         cyberpunk-2019
                         dark-mint

                         ;; earthy
                         base16-monokai
                         gruvbox-dark-hard
                         base16-atelier-estuary
                         jazz
                         base16-atelier-savanna
                         base16-mocha
                         minsk ;; gunmetal-green with good line highlight

                         ;; blue
                         gotham ;; w/burnt orange
                         base16-harmonic-dark ;; blue bg
                         constant ;; very cool war machine colors (grays, white & deep cyans)

                         ;; purpleish
                         base16-material-palenight ;; purple

                         ;; bright
                         sanityinc-tomorrow-day
                         base16-brushtrees ;; icy white
                         base16-nova ;; light gray. not too bright.

                         ;; solarized variants
                         base16-solarized-dark
                         base16-flat
                         base16-apathy ;; cyan
                         base16-atlas ;; deep colors. has yellow text

                         ;;; trim yr beard variants
                         base16-darktooth ;; warm colors
                         base16-sandcastle ;; blue ash. light bg.
                         base16-embers ;; darker variant

                         ;; neon
                         base16-rebecca ;; uv indigo
                         green-screen ;; greens on black bg
                         green-phosphor ;; bright greens on dark green bg

                         ;; dull
                         base16-ashes ;; cloudy sky
                         base16-onedark ;; but lighter gray bg and more colorful text
                         base16-black-metal-bathory ;; charcoal & amber
                         base16-chalk ;; pastels on dark bg
                         base16-ia-dark ;; more subdued of framer or chalk
                         base16-horizon-dark ;; cool colors helios
                         base16-tomorrow-night
                         cherry-blossom ;; black bg w/poppin' violets, roses, & sakura

                         ;; dark & crisp
                         base16-bright ;; has red text
                         sanityinc-tomorrow-bright)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Mononoki Nerd Font Mono"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key "'" ;; TODO: doesn't work. apparently it's being overridden by some evil map, but it (codepoint 39) is not in evil-state-normal-map...?!
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '()
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "called immediately after `dotspacemacs/init', before layer configuration.
use for exprs to eval before any packages are loaded; else put in `dotspacemacs/user-config'."
  ; (setq spacemacs-configuration-layer-path "~/programming/spacemacs-layers/")
  )

(defun %chg (a b) (* 100 (/ (- b a) a)))

(defun dotspacemacs/user-config ()
  "config func for user code. called last in spacemacs' init, after layers."
  (server-mode 1)
  ;; see https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/ about optimizing spacemacs' startup

  ;; TODO: change haskell indenting: don't start new lines with leading space (e.g. when adding a new line above a comment in a top-level statement)
  ;; (remove-hook 'haskell-mode company-mode) ;; doesn't work...?

  ;;; requires. execute requires after emacs loads
  (require 'org)

  ;;; definitions

  (defmacro cmd (&rest c) "transform a function into a command by prefixing its body with `interactive'. note that this command will not have a name; it'll just say \"lambda\"" `(lambda () (interactive) ,@c))
  (define-prefix-command 'ng) ;; prefix key map, "nic's 'g'"

  (defun timer-bell () (call-process
                        (car (file-expand-wildcards "/nix/store/*-user-environment/bin/cvlc"))
                        nil nil nil "--play-and-exit" "/home/nic/programming/op_finished.wav"))

  (defun toggle-timer-bell nil
    (interactive)
    (if (boundp 'org-timer-done-hook)
        (if (memql 'timer-bell org-timer-done-hook)
          (progn (message "removed \"timer done\" bell") (remove-hook 'org-timer-done-hook 'timer-bell))
          (progn (message "added \"timer done\" bell") (add-hook 'org-timer-done-hook 'timer-bell)))
        (error "org-timer-done-hook does not exist. you need to start a timer first")))

  (defun toggle-cursor-blink nil
    (interactive)
    (setq blink-cursor-mode (if (equal 0 (blink-cursor-mode)) t 0)))

  ;; better version of counsel-load-theme: shows only themes from dotspacemacs-themes
  (defun nics-counsel-load-theme ()
    "variant of `counsel-load-theme'. uses dotspacemacs-themes instead of (custom-available-themes)"
    (interactive)
    (ivy-read "Load one of nic's favorite themes: "
              (mapcar 'symbol-name dotspacemacs-themes)
              :action #'counsel-load-theme-action
              :caller 'nics-counsel-load-theme))

  ;; TODO: make version of nics-counsel-load-theme for fonts. use ivy-read to select a font. select from (x-list-fonts _regex nil (selected-frame)) constrained to names containing normal-normal-normal, mono, and nerd. use set-frame-font to set the font.

  (defun try-theme () ;; nb see http://ergoemacs.org/emacs/elisp_buffer_string.html for similar functions
    (interactive)
    (counsel-load-theme-action (current-word)))

  (defun save-modified-and-close-buffer ()
    (interactive)
    (when (buffer-modified-p) (save-buffer))
    (funcall 'spacemacs/kill-this-buffer))

  ;; NB: word boundary regex may need some tweaking
  (evil-define-motion vile-goto-word-by-first-letter (count char)
    "Move to the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil.
Slightly modified version of evil-find-char."
    :type inclusive
    (interactive "<c><C>")
    (setq count (or count 1))
    (let ((fwd (> count 0))
          (visual (and evil-respect-visual-line-mode
                       visual-line-mode)))
      (setq evil-last-find (list #'vile-goto-word-by-first-letter char fwd))
      (when fwd (forward-char))
      (let ((case-fold-search nil))
        (unless (prog1
                    (re-search-forward (concat "[^[:alnum:]]" (char-to-string char)) ; modified this line
                                       (cond (evil-cross-lines
                                              nil)
                                             ((and fwd visual)
                                              (save-excursion
                                                (end-of-visual-line)
                                                (point)))
                                             (fwd
                                              (line-end-position))
                                             (visual
                                              (save-excursion
                                                (beginning-of-visual-line)
                                                (point)))
                                             (t
                                              (line-beginning-position)))
                                       t count)
                  (when fwd (backward-char)))
          (user-error "Can't find %c" char)))))

  ;; backwards version
  (evil-define-motion vile-goto-word-by-first-letter-backward (count char)
    "Move to the previous COUNT'th occurrence of CHAR."
    :type exclusive
    (interactive "<c><C>")
    (vile-goto-word-by-first-letter (- (or count 1)) char)
    (forward-char 1))

  (defun line-numbers-on () (interactive) (display-line-numbers-mode))

  (when (fboundp 'eww)
    (defun xah-rename-eww-buffer ()
      "Rename `eww-mode' buffer so sites open in new page.
URL `http://ergoemacs.org/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
      (let (($title (plist-get eww-data :title)))
        (when (eq major-mode 'eww-mode )
          (if $title
              (rename-buffer (concat "eww " $title ) t)
            (rename-buffer "eww" t)))))

    (add-hook 'eww-after-render-hook 'xah-rename-eww-buffer))

  ;;; feature enable or disable
  ;; TODO: these don't work. why.

  (spacemacs/toggle-smartparens-globally-off)
  (setq flycheck-global-modes nil)  ;; disable flycheck; i'll enable it when i want it
  (add-hook 'racket-mode-hook 'parinfer-mode)
  (add-hook 'racket-mode-hook 'add-lang-specific-repl-bind)
  ;; (add-hook 'clojure-mode-hook 'parinfer-mode) ;; parinfer fucks-up a lot
  (smartparens-mode -1)

  ;; (eww-toggle-colors -1) ;; TODO: make this run when eww starts
  (nyan-mode 1) (nyan-start-animation)
  (spacemacs/toggle-display-time-on) ; display wall clock in mode line
  (global-git-commit-mode t) ; allow emacs to be my $EDITOR for git commits
  (ido-mode 1)
  (centaur-tabs-mode 1)
  (display-theme-mode 1)

  ;; ensure that dotspacemacs-line-numbers is not mentioned above; it'll screw-up things
  ;; linum mode SUCKS. it's inefficient and makes display unreadable after scaling text (font size)
  ;; to set relative line numbers using the native emacs (since v26.1) numbering system, Customize the display-line-numbers variable.
  (global-linum-mode -1)
  (display-line-numbers-mode) ;; don't use SPC t n

  ;;; variables / preferences
  (defalias 'yes-or-no-p 'y-or-n-p) ; "y" or "n" for "yes" or "no"

  ;; Don't ask about killing process buffers on shutdown
  ;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; shut-up compile saves
  (setq
   compilation-ask-about-save nil
   ;; save *nothing*
   compilation-save-buffers-predicate '(lambda () nil)
   powerline-default-separator 'roundstub)        ;; or 'wave, 'slant

  (defadvice ido-find-file (after find-file-sudo activate) ;; doesn't work for counsel-find-file somewhy
    ;; so use C-x C-f for ido-find-file
    "open file as root as needed."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  ;; ido everywhere. this (or at least a subset of these commands) is needed in order to do the auto-tramp thing above
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere 1)

  (setq ido-use-filename-at-point 'guess) ; parse URIs from text, allowing them to be opened
  (setq ido-create-new-buffer 'always) ; create new buffers w/o anticipating writing (considering filesystem or permissions)
                                        ; (setq ido-file-extensions-order '(".org" ".txt" ...)) ;; shows these exts first in minibuffer
                                        ; (setq ido-ignore-extensions '(...)) ;; hide files when using ido-find-file

  ;; ignore certain filesystem objects when finding files, or buffers when searching for them
                                        ; (setq ido-ignore-buffers) (setq ido-ignore-directories) (setq ido-ignore-files)

  ;; uses tz timezone identifiers
  ;; you can infer whether you've given an incorrect timezones identifier
  ;; by whether the timezone in (display-time-world) buffer is odd
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "West Coast")
          ("America/New_York" "East Coast")
          ("America/Chicago" "Central U.S.")
          ("America/Argentina/Buenos_Aires" "Buenos Aires, Argentina")
          ("Europe/London" "London, England")
          ("Europe/Prague" "Rijeka, Croatia | Prague")
          ("Europe/Helsinki" "Helsinki, Finland")
          ("Asia/Kolkata" "India")
          ("Asia/Tokyo" "Japan")))

  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t) ;; or change t to a singleton string

  (setq nyan-wavy-trail t)

  ;; to define abbrev table for a specific major mode, simply create a table
  ;; with name <major-mode>-abbrev-table
  ;; NOTE: abbrevs cannot contain special symbols!
  (clear-abbrev-table global-abbrev-table)
  (define-abbrev-table 'global-abbrev-table
    '(("B" "Boolean")
      ("HT" "HashTable")
      ("L" "Listof")
      ("N" "Natural")
      ("O" "Option")
      ("P" "Pairof")
      ("PS" "Path-String")
      ("R" "Real")
      ("S" "String")
      ("Sym" "Symbol")
      ("V" "Vectorof")
      ("Q" "Exact-Rational")
      ("Z" "Integer")
      ("hr" "hash-ref")))
  (set-default 'abbrev-mode t)
  (setq save-abbrevs nil)
  ;; (abbrev-mode 1)

  ;; TODO: when i get a programmable keyboard, change this to a single keycode.
  (setq-default evil-escape-key-sequence "+;") ;; key sequence to go from insert to normal mode
  (setq-default evil-escape-delay 0.2)

  (defun open-typed-racket-docs () ;; NOTE: this URL is incorrect on nixos
    (commandp)
    (eww-browse-url "file:///usr/share/doc/racket/ts-reference/index.html"))

  ;; TODO: bind some key to swiper-thing-at-point
  ;;; keybinds
  (defun keymap+ (&rest bindings)
    (if (stringp (car bindings))
        (progn (setq k (pop bindings) f (pop bindings))
               (while k
                 (global-set-key (kbd k) f)
                 (setq k (pop bindings) f (pop bindings))))
      (let ((m (pop bindings)))
        (setq k (pop bindings) f (pop bindings))
        (while k
          (define-key m (kbd k) f)
          (setq k (pop bindings) f (pop bindings))))))

  (keymap+ "M-t"          'toggle-timer-bell
           "C-x C-e"      'eval-print-last-sexp
           "C-x C-u"      'eval-last-sexp
           "M-+"          'text-scale-increase
           "M--"          'text-scale-decrease
           "M-="          (cmd (text-scale-set 0))
           "<C-return>"   'shell
           "<C-S-return>" 'eshell)
  (keymap+ evil-normal-state-local-map  ;; TODO: cf spacemacs/set-leader-keys
           "SPC t n" 'line-numbers-on ;; overrides spacemacs/toggle-line-numbers
           "SPC T z" 'nics-counsel-load-theme)
  (keymap+ evil-normal-state-map
           "e" (cmd (evil-forward-WORD-end) (evil-append 1))
           "SPC T c" 'try-theme ;; mnemonic: "try colors"
           "ZZ"  'save-modified-and-close-buffer ;; overrides evil-save-modified-and-close. i want to close only buffer
           "M-f" 'vile-goto-word-by-first-letter
           "M-o" 'find-file-at-point
           "M-F" 'vile-goto-word-by-first-letter-backward
           "g"    ng)
  (seq-do (lambda (m)
            (define-key m (kbd "M-j") (cmd (evil-next-line 10)))
            (define-key m (kbd "M-k") (cmd (evil-previous-line 10)))) ;; there's no evil-visual-line-state-map. boo. this means that i can't use ^K or ^J in visual line mode, though i can in visual and visual block modes.
          (list evil-normal-state-map evil-visual-state-map))
  (keymap+ ng
           "r" 'save-buffer
           "c" 'spacemacs/kill-this-buffer
           "T" 'centaur-tabs-backward
           "t" 'centaur-tabs-forward
           "b" 'toggle-cursor-blink
           "d" 'open-typed-racket-docs)

  ;; iedit hack until fix is mainstream
  (defalias 'iedit-cleanup 'iedit-lib-cleanup)

  ;;; hooks (to learn)

  ;; try to make saving auto-reload buffer into cider. FAILS:
  ;; 1) installs correctly on loading a clj(s) file
  ;; 2) says that cider-load-buffer isn't found (which shouldn't be a problem assuming dynamic scoping)
  ;; 3) all this going wrong makes all but two of the clojure major mode commands (SPC m ...) unfound in counsel
  ;; 4) this save/reload hook persists when editing even non-clojure files; once the hook is installed, it works everywhere!
  ;; (defun auto-load-cider-file-hook nil (cider-load-buffer) nil)
  ;; (defun auto-load-cider-clj-hook nil
  ;;   (interactive)
  ;;   (unless (member 'auto-load-cider-file-hook write-file-functions)
  ;;     (setq write-file-functions (append write-file-functions '(auto-load-cider-file-hook)))))
  ;; (add-hook 'clojure-mode-hook 'auto-load-cider-clj-hook)
  ;; (delq 'auto-load-cider-file-hook write-file-functions) ;; used to make saving possible again!

  ;; (add-hook 'write-file-functions (cmd (timer-bell))) ; do something when saving a file. remember to return ni!

  ;; modeline. TODO: remove percent-position from end of modeline; nyancat already displays that.
  ;; (setq mode-line-format (list "%e" 'spacemacs--cur-theme (list :eval (spaceline-ml-main))))
  ;; (setq spacemacs-spaceline-additional-segments '((new-version :when active) (spaceline-define-segment "current theme" spacemacs--cur-theme)))

  ;; append the theme to the modeline only once (isn't executed on SPC-f-e-R)
  ;; (unless (boundp 'global-mode-string-set)
  ;;   (spacemacs/toggle-mode-line-point-position) ;; wtf this shit still doesn't work!? WHY??
  ;;   (setq global-mode-string
  ;;       (cons
  ;;        ;; (concat (symbol-name spacemacs--cur-theme) " ") ;; assigns a string literal; mode line doesn't update when setting a new theme interactively
  ;;        spacemacs--cur-theme ;; if it's the symbol, it doesn't display, even though the cons succeeds
  ;; still doesn't update the variable even after setting a new theme interactively
  ;;        global-mode-string))
  ;;   (defconst global-mode-string-set t))

  ;; try seeing about var mode-line-position

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"])
 '(beacon-color "#d54e53")
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("db7f422324a763cfdea47abf0f931461d1493f2ecf8b42be87bbbbbabf287bfe" default)))
 '(display-line-numbers-type (quote relative))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#192028" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(highlight-symbol-colors
   (quote
    ("#FFF68F" "#B7EB8F" "#76DDBA" "#91D5FF" "#ADC6FF" "#D3ADF7" "#FFADD2" "#FFA39E" "#FFD591")))
 '(magit-diff-use-overlays nil)
 '(org-src-block-faces
   (quote
    (("emacs-lisp"
      (:background "#F0FFF0"))
     ("dot"
      (:foreground "gray50")))))
 '(package-selected-packages
   (quote
    (overcast-theme monokai-pro-theme minsk-theme metalheart-theme melancholy-theme lush-theme laguna-theme lab-themes green-screen-theme green-phosphor-theme green-is-the-new-black-theme github-theme github-modern-theme foggy-night-theme exotica-theme distinguished-theme display-theme dark-mint-theme danneskjold-theme cyberpunk-theme cyberpunk-2019-theme constant-theme cherry-blossom-theme challenger-deep-theme borland-blue-theme autumn-light-theme atom-one-dark-theme atom-dark-theme arc-dark-theme organic-green-theme subatomic256-theme naquadah-theme flatland-theme pine-script-mode base16-helios-theme graphviz-dot-mode faceup base16-theme json-reformat pyvenv org-category-capture alert log4e gntp simple-httpd json-snatcher parent-mode highlight-indentation haml-mode autothemer fringe-helper git-gutter+ pos-tip flx highlight web-completion-data ghc inflections multiple-cursors paredit lv eval-sexp-fu sesman spinner queue pkg-info parseclj a epl powerline bind-map markup-faces auto-complete js2-mode hydra f s dash bind-key magit-popup magit git-commit with-editor transient async projectile org-plus-contrib gotham-theme request markdown-mode git-gutter anzu counsel swiper ivy cider parseedn clojure-mode anaconda-mode pythonic avy popup dash-functional iedit smartparens evil goto-chg undo-tree haskell-mode company flycheck yasnippet skewer-mode csv-mode yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme twittering-mode twilight-bright-theme twilight-anti-bright-theme toc-org tldr tagedit subatomic-theme spaceline smex smeargle slim-mode shell-pop scss-mode scheme-complete sass-mode ripgrep restart-emacs rainbow-delimiters racket-mode pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el parinfer paradox orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file nyan-mode neotree mwim multi-term move-text monokai-theme moe-theme mmm-mode material-theme markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint leuven-theme json-mode js2-refactor js-doc jazz-theme ivy-hydra intero indent-guide importmagic hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers helm-make haskell-snippets gruvbox-theme google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser fuzzy flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emmet-mode elpy elisp-slime-nav dumb-jump diminish diff-hl define-word cython-mode counsel-projectile company-web company-statistics company-ghci company-ghc company-cabal company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow coffee-mode cmm-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu centaur-tabs auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent afternoon-theme adoc-mode adaptive-wrap ace-window ace-link ac-ispell)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(window-divider-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hasklig" :foundry "ADBO" :slant normal :weight normal :height 96 :width normal)))))
