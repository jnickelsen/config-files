;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     git
     helm
     ;; lsp
     imenu-list
     markdown
     multiple-cursors
     olivetti
     (org :variables
          org-directory "~/Documents/org"
          org-projectile-file "~/Documents/org/projects.org"
          org-enable-org-journal-support t
          org-enable-hugo-support t
          )
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; version-control
     treemacs

     (treemacs :variables treemacs-use-all-the-icons-theme t)
     (treemacs :variables treemacs-use-follow-mode t)
     (treemacs :variables treemacs-use-filewatch-mode t)
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      ;; navigation
                                      ranger
                                      ;; Aesthetics
                                      all-the-icons
                                      doom-modeline
                                      doom-themes
                                      nerd-icons
                                      nyan-mode
                                      org-bullets
                                      toxi-theme
                                      zenburn-theme
                                      ef-themes
                                      ;; For writing
                                      writegood-mode
                                      flyspell
                                      langtool
                                      wc-mode
                                      org-pomodoro
                                      org-roam
                                      org-side-tree
                                      palimpsest
                                      writeroom-mode
                                      ;; For annotating PDFs
                                      pdf-tools
                                      ;;org-noter
                                      ;; For making a kanban from TODO entries
                                      org-kanban
                                      )


   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    light-blue
                                    powerline
                                    vim-powerline
                                    )


   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only)


  ;; Optional: prioritize official packages if needed
  ;; (setq package-archive-priorities '(("gnu" . 10) ("org" . 8) ("melpa" . 5)))

  ;; Optional: custom load-path additions
  (add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

  ;; If using Spacemacs, `dotspacemacs/init` goes in `.spacemacs`, not here

  )


(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 7)
                                (projects . 10)
                                (bookmarks . 5)
                                (agenda . 5)
                                (todos . 5)
                                (recents-by-project . (5 . 5)))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.

   dotspacemacs-themes '((doom-flatwhite :package doom-themes)
                         (doom-horizon :package doom-themes)
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;;dotspacemacs-mode-line-theme '(vanilla :separator wave :separator-scale 1.5)
   dotspacemacs-mode-line-theme 'doom

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq evil-want-keybinding nil)
  (setq package-quickstart t)
  ;; (require 'package)
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("org"   . "https://orgmode.org/elpa/")
          ("gnu"   . "https://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

;;;;;;;;;; setting load path for private packages
  (let ((default-directory "~/.emacs.d/private/"))
    (normal-top-level-add-subdirs-to-load-path))
  (load "~/.emacs.d/private/jess-config/jess-theming.el")
  (load "~/.emacs.d/private/jess-config/jess-org.el")
  (require 'jess-theming)
  (require 'jess-org)
  (my/apply-all-palettes "Jess theme")
  (message "Theme loaded successfully.")

  ;; making sure markdown mode works right
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;;;;;;;; enable transient mark mode, which enables using org mode with different file types
  (transient-mark-mode 1)

;;;;;;;;;; trying to sort window dividers when I split panes
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode 1)

;;;;;;;;;; enabling nyan mode
  (require 'nyan-mode)
  (nyan-mode 1)

;;;;;;;;;; changing bell alarm
  ;; see M-x customize group / org-pomodoro for this

;;;;;;;;;; doom modeline setup
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-nyan-bar t)

  (require 'doom-modeline)
  (doom-modeline-mode 1)
  (nyan-mode 1)

  ;; Custom Pomodoro countdown segment (just the timer, no task name)


  (with-eval-after-load 'org-pomodoro
    (doom-modeline-def-segment my-pomodoro-countdown
      "Show org-pomodoro countdown timer if active."
      (when (and (featurep 'org-pomodoro) (org-pomodoro-active-p))
        (let* ((time-left (org-pomodoro-format-seconds)))
          (format "[%s]" time-left))))
    ;; Define your custom modeline layout
    (doom-modeline-def-modeline 'my-clean-line
      '(modals buffer-info)                           ;; Left
      '(word-count my-pomodoro-countdown major-mode))      ;; Right

    ;; Apply the custom modeline
    (defun my/use-clean-doom-modeline ()
      (doom-modeline-set-modeline 'my-clean-line 'default))

    (my/use-clean-doom-modeline)
    )
;;;;;;;;;; OLIVETTI - markdown
;;;;;;;;;; I am going to try and use writeroom mode for a bit and see if that is very different
  ;; (add-hook 'text-mode-hook (lambda ()
  ;;                             (interactive)
  ;;                             (message "Olivetti text-mode-hook")
  ;;                             (olivetti-set-width 81)
  ;;                             ;;(hidden-mode-line-mode)
  ;;                             (spacemacs/toggle-vi-tilde-fringe-off)
  ;;                             (olivetti-mode 1)
  ;;                             (palimpsest-mode)
  ;;                             ))


;;;;;;;;;; WRITEROOM MODE INSTEAD
  ;; I have customised other things via customize-group RET writeroom RET
  ;; for example not to maximize screen in fullscreen
  (add-hook 'text-mode-hook (lambda ()
                              (interactive)
                              (message "Writeroom text-mode-hook")
                              ;;(hidden-mode-line-mode)
                              (spacemacs/toggle-vi-tilde-fringe-off)
                              (writeroom-mode 1)
                              (palimpsest-mode)
                              (visual-line-mode t)
                              ))

;;;;;;;;;; MARKDOWN STUFF
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; Apply your custom color palette
              (my/apply-markdown-palette-by-name "Your Palette Name")

              ;; Adjust heading sizes and weights
              (set-face-attribute 'markdown-header-face-1 nil :height 1.1 :weight 'bold)
              (dolist (face '(markdown-header-face-2 markdown-header-face-3
                                                     markdown-header-face-4 markdown-header-face-5
                                                     markdown-header-face-6))
                (set-face-attribute face nil :height 1.0 :weight 'bold))))

;;;;;;;;;; RANGER
  (define-key evil-normal-state-map (kbd ", r") 'ranger)

;;;;;;;;;;ORG NOVELIST
;;;;;;;;;; see here for more: https://github.com/sympodius/org-novelist
;;;;;;;;;; including how to export
  ;; (load "~/.emacs.d/private/org-novelist/org-novelist.el")
  ;; (setq org-novelist-author "Jessica Nickelsen")  ; The default author name to use when exporting a story. Each story can also override this setting
  ;; (setq org-novelist-author-email "jessica.nickelsen@gmail.com")  ; The default author contact email to use when exporting a story. Each story can also override this setting
  ;; (setq org-novelist-automatic-referencing-p t)  ; Set this variable to 't' if you want Org Novelist to always keep note links up to date. This may slow down some systems when operating on complex stories. It defaults to 'nil' when not set

;;;;;;;;;; MAGIT
;;;;;;;;;; testing 'stage all and commit' with magit:
  (defun my/magit-stage-all-and-commit(message)
    (interactive "sCommit Message: ")
    (magit-stage-modified)
    (magit-commit (list "-m" message)))

;;;;;;;;;;;SAVE LAYOUT
;;;;;;;;;; save and load layouts cleanly (and cleanup old buffers)
  (defun jess/save-layout-cleanly ()
    "Prompt for a layout name and save the current layout with cleanup."
    (interactive)
    (require 'persp-mode)
    (persp-mode 1)
    (when (fboundp 'persp-remove-unused-buffers)
      (persp-remove-unused-buffers))
    (when (and (boundp 'persp-save-dir)
               (fboundp 'persp-save-state-to-file))
      (let* ((existing (directory-files persp-save-dir nil "\\.persp\\'"))
             (names (mapcar (lambda (f) (file-name-sans-extension f)) existing))
             (name (completing-read "Save layout as: " names nil nil "jess-")))
        (persp-save-state-to-file
         (expand-file-name (concat name ".persp") persp-save-dir))
        (message "🧼 Layout saved as '%s'." name))))

  (spacemacs/set-leader-keys "o w" #'jess/save-layout-cleanly)

;;;;;;;;;;;;LOAD LAYOUT
  (defun jess/load-layout ()
    "Prompt for a saved layout and load it."
    (interactive)
    (require 'persp-mode)
    (persp-mode 1)
    (when (and (boundp 'persp-save-dir)
               (fboundp 'persp-load-state-from-file))
      (let* ((existing (directory-files persp-save-dir nil "\\.persp\\'"))
             (names (mapcar (lambda (f) (file-name-sans-extension f)) existing))
             (name (completing-read "Load layout: " names nil t)))
        (persp-load-state-from-file
         (expand-file-name (concat name ".persp") persp-save-dir))
        (message "✨ Loaded layout: '%s'" name))))

  (spacemacs/set-leader-keys "o l" #'jess/load-layout)


;;;;;;;;;;;;;;;;POMODORO
;;;;;;;;;tweak times
  (setq org-pomodoro-length 25       ;; Sprint time in minutes
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15
        org-pomodoro-long-break-frequency 4) ;; Every 4 pomodoros

;;;;; or custom set some quick times for testing and whatnot
  (defun my/pomodoro-quick-1 ()
    (interactive)
    (setq org-pomodoro-length 1
          org-pomodoro-short-break-length 1
          org-pomodoro-long-break-length 1)
    (org-pomodoro))

  (defun my/pomodoro-quick-15 ()
    (interactive)
    (setq org-pomodoro-length 15
          org-pomodoro-short-break-length 3
          org-pomodoro-long-break-length 10)
    (org-pomodoro))

  (defun my/pomodoro-quick-25 ()
    (interactive)
    (setq org-pomodoro-length 25
          org-pomodoro-short-break-length 5
          org-pomodoro-long-break-length 15
          org-pomodoro-long-break-frequency 4)
    (org-pomodoro))

;;;;;;;;;; turn off the bell
  ;; (setq visible-bell t)

;;;;;;;;;; configure the rest

  (defvar my/pomodoro-buffer nil
    "The buffer being tracked during the current Pomodoro.")

  (defvar my/pomodoro-start-wordcount 0
    "Word count at the beginning of the Pomodoro.")

  (defvar my/pomodoro-log-file
    (expand-file-name "~/Documents/org/pomodoro-log.org")
    "File where Pomodoro word count logs are saved.")


  (defun my/pomodoro-start-tracking ()
    "Prompt for buffer to track, store initial word count."
    (let* ((buf (get-buffer (read-buffer "Track word count in buffer: " (buffer-name) t))))
      (setq my/pomodoro-buffer buf)
      (setq my/pomodoro-start-wordcount
            (with-current-buffer buf
              (count-words (point-min) (point-max))))
      (message "Tracking Pomodoro in buffer: %s (%d words)"
               (buffer-name buf) my/pomodoro-start-wordcount)))

  (defun my/pomodoro-log-results ()
    "Log the word count delta at end of Pomodoro."
    (let ((tracked-buf (get-buffer my/pomodoro-buffer)))
      (when (and tracked-buf (buffer-live-p tracked-buf))
        (let* ((end-count (with-current-buffer tracked-buf
                            (count-words (point-min) (point-max))))
               (delta (- end-count my/pomodoro-start-wordcount))
               (timestamp (format-time-string "%Y-%m-%d %H:%M"))
               (bufname (buffer-name tracked-buf)))
          (with-temp-buffer
            (insert (format "* %s\n  - %s: %d words in %s\n"
                            (format-time-string "%Y-%m-%d")
                            timestamp delta bufname))
            (append-to-file (point-min) (point-max) my/pomodoro-log-file))
          (message "Pomodoro complete: %d words in %s" delta bufname))))
    ;; Clean up

    (defvar my/pomodoro-timer-refresh nil)

    (defun my/start-pomodoro-modeline-refresh ()
      (setq my/pomodoro-timer-refresh
            (run-with-timer 1 1 (lambda () (force-mode-line-update t)))))

    (defun my/stop-pomodoro-modeline-refresh ()
      (when (timerp my/pomodoro-timer-refresh)
        (cancel-timer my/pomodoro-timer-refresh)
        (setq my/pomodoro-timer-refresh nil)))

    (add-hook 'org-pomodoro-started-hook #'my/start-pomodoro-modeline-refresh)
    (add-hook 'org-pomodoro-finished-hook #'my/stop-pomodoro-modeline-refresh)
    (add-hook 'org-pomodoro-killed-hook  #'my/stop-pomodoro-modeline-refresh)


    (setq my/pomodoro-buffer nil)
    (setq my/pomodoro-start-wordcount 0))

  (add-hook 'org-pomodoro-started-hook #'my/pomodoro-start-tracking)
  (add-hook 'org-pomodoro-finished-hook #'my/pomodoro-log-results)
  (add-hook 'org-pomodoro-killed-hook #'my/pomodoro-log-results)



  ;; end of user-config
  )




;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(doom-modeline-buffer-encoding nil)
   '(doom-modeline-buffer-file-name-style 'buffer-name)
   '(doom-modeline-buffer-file-true-name t)
   '(doom-modeline-buffer-state-icon nil)
   '(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
   '(doom-modeline-env-version nil)
   '(doom-modeline-major-mode-color-icon nil)
   '(doom-modeline-modal-icon nil)
   '(doom-modeline-persp-icon nil)
   '(doom-modeline-persp-name nil)
   '(doom-modeline-time t)
   '(helm-minibuffer-history-key "M-p")
   '(org-agenda-files
     '("~/Documents/org/today.org" "~/Documents/org/inbox.org"
       "~/Documents/GitHub/fiction/90 Projects/009 Writing Group Anthology 2/90 Manuscript/rewilding/1.0 drafts/rewilding-edits.org"))
   '(org-pomodoro-finished-sound "~/.emacs.d/private/sounds/kitchen-timer.wav")
   '(org-pomodoro-short-break-sound "~/.emacs.d/private/sounds/wood-block.wav")
   '(org-side-tree-narrow-on-jump nil)
   '(org-side-tree-recenter-position 0.2)
   '(package-selected-packages
     '(a ace-jump-helm-line ace-link add-node-modules-path aggressive-indent alert
         all-the-icons auto-compile auto-highlight-symbol catppuccin-theme
         centered-cursor-mode clean-aindent-mode closql column-enforce-mode
         company company-web counsel counsel-css dash-functional deferred
         define-word devdocs diminish dired-quick-sort disable-mouse doom-modeline
         doom-themes dotenv-mode drag-stuff dumb-jump edit-indirect ef-themes
         elisp-def elisp-demos elisp-slime-nav emacsql emmet-mode emojify emr
         eval-sexp-fu evil-anzu evil-args evil-cleverparens evil-collection
         evil-easymotion evil-escape evil-evilified-state evil-exchange
         evil-goggles evil-iedit-state evil-indent-plus evil-lion evil-lisp-state
         evil-matchit evil-mc evil-nerd-commenter evil-numbers evil-org
         evil-surround evil-textobj-line evil-tutor evil-unimpaired
         evil-visual-mark-mode evil-visualstar expand-region eyebrowse
         fancy-battery flatui-theme flx-ido flycheck flycheck-elsa
         flycheck-package flycheck-pos-tip forge gandalf-theme gh-md ghub git-link
         git-messenger git-modes git-timemachine gitignore-templates gntp gnuplot
         golden-ratio google-translate haml-mode helm-ag helm-comint helm-css-scss
         helm-descbinds helm-git-grep helm-ls-git helm-make helm-mode-manager
         helm-org helm-org-rifle helm-projectile helm-purpose helm-swoop
         helm-themes helm-xref hide-comnt highlight-indentation highlight-numbers
         highlight-parentheses hl-todo holy-mode htmlize hungry-delete hybrid-mode
         impatient-mode indent-guide info+ inspector ivy langtool link-hint llama
         log4e lorem-ipsum macrostep magit magit-section markdown-mode
         markdown-toc memoize multi-line nameless nerd-icons nyan-mode olivetti
         open-junk-file org org-bullets org-category-capture org-cliplink
         org-contrib org-download org-journal org-kanban org-mime org-noter
         org-plus-contrib org-pomodoro org-present org-project-capture
         org-projectile org-ql org-reverse-datetree org-rich-yank org-roam
         org-side-tree org-sidebar org-starter org-super-agenda org-superstar
         organic-green-theme orgit orgit-forge ov overseer ox-hugo package-lint
         palimpsest paradox password-generator pcre2el pdf-tools pomodoro popwin
         pos-tip prettier-js pug-mode quickrun rainbow-delimiters ranger request
         restart-emacs sass-mode scss-mode shrink-path sidebar sidebar-narrow
         simple-httpd slim-mode smeargle space-doc spaceline
         spaceline-all-the-icons spacemacs-purpose-popwin
         spacemacs-whitespace-cleanup string-edit-at-point string-inflection
         swiper symbol-overlay symon tablist tagedit term-cursor toc-org tomelr
         toxi-theme transient treemacs-all-the-icons treemacs-evil
         treemacs-icons-dired treemacs-magit treemacs-persp treemacs-projectile
         treepy ts undo-fu undo-fu-session uuidgen vi-tilde-fringe
         volatile-highlights vundo wc-mode web-beautify web-completion-data
         web-mode wgrep winum with-editor writegood-mode writeroom-mode ws-butler
         yaml yasnippet zenburn-theme))
   '(writeroom-fullscreen-effect 'maximized)
   '(writeroom-global-effects
     '(writeroom-set-alpha writeroom-set-tool-bar-lines
                           writeroom-set-vertical-scroll-bars
                           writeroom-set-bottom-divider-width))
   '(writeroom-maximize-window nil)
   '(writeroom-mode-line t))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(highlight-changes ((t (:foreground "#AE0073" :background "#FCDAE9"))))
   '(highlight-changes-delete ((t (:underline t :foreground "#AE0073" :background "#FCDAE9")))))
  )
