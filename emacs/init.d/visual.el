;; Start with the *scratch* buffer only.
(setq inhibit-startup-message t)
(setq inhibit-default-init t)

;; Disable bars
(scroll-bar-mode -1) 
(tool-bar-mode -1) 
(menu-bar-mode -1) 
(scroll-bar-mode -1) 

;; Display line-numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

(set-fringe-mode 10) ;; Margins
(setq visible-bell t)
(set-face-attribute 'default nil :font "Envy Code R")

;; Scroll one line at a time (less "jumpy" than defaults)
(setq scroll-margin 1
  scroll-step 1
  mouse-wheel-scroll-amount '(1 ((shift) . 1)) 
  mouse-wheel-progressive-speed nil 
  mouse-wheel-follow-mouse 't
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(use-package gruvbox-theme
   :ensure t
   :config
   (load-theme 'gruvbox-dark-hard t)
   )

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

(use-package dired-rainbow
	:ensure t
  :after dired
  :config
  (defconst dired-audio-files-extensions
    '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
    "Dired Audio files extensions")
  (dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)

  (defconst dired-video-files-extensions
    '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
      "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
    "Dired Video files extensions")
  (dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)

  (defconst dired-doc-files-extensions
    '("txt" "TXT" "pdf" "PDF" "doc" "DOC" "xls" "XLS" "org" "ORG" "docx" "DOCX" "csv" "CSV"
      "json" "JSON" "epub" "EPUB" "mobi" "MOBI"
      )
    "Dired Document files extensions")
  (dired-rainbow-define document "#D8BFD8" dired-doc-files-extensions)

  (defconst dired-exe-files-extensions
    '("exe" "EXE" "sh" "SH" "bat" "BAT" "msi" "MSI"
      )
    "Dired Executable files extensions")
  (dired-rainbow-define exe "#98FB98" dired-exe-files-extensions)

  (defconst dired-code-files-extensions
    '("js" "JS" "py" "PY" "c" "C" "h" "H" "php" "PHP" "xml" "XML"
      )
    "Dired Code files extensions")
  (dired-rainbow-define code "#CF9FFF" dired-code-files-extensions)

  (defconst dired-web-files-extensions
    '("css" "CSS" "html" "HTML" "scss" "SCSS" "htm" "HTM"
      )
    "Dired Web files extensions")
  (dired-rainbow-define web "#DA70D6" dired-web-files-extensions)

  (defconst dired-image-files-extensions
    '("jpg" "JPG" "jpeg" "JPEG" "bmp" "BMP" "tiff" "TIFF" "gif" "GIF" "webp" "WEBP" "png" "PNG" "svg" "SVG"
      )
    "Dired Image files extensions")
  (dired-rainbow-define image "#4169E1" dired-image-files-extensions)

  (defconst dired-arhive-files-extensions
    '("7z" "7Z" "zip" "ZIP" "rar" "RAR" "tar" "TAR" "iso" "ISO" "bz2" "BZ2" "gz" "GZ" "lz" "LZ" "lz4" "LZ4" "lzma" "LZMA" "lzo" "LZO"
      "z" "Z" "xz" "XZ" "zst" "ZST" "ace" "ACE" "arj" "ARJ" "cab" "CAB"
      )
    "Dired Arhive files extensions")
  (dired-rainbow-define arhive "#E97451" dired-arhive-files-extensions)

  (defconst dired-danger-files-extensions
    '("pem" "PEM" "crt" "CRT" "service" "SERVICE" "conf" "CONF" "cfg" "CFG" "efi" "EFI" "rc" "RC"
      )
    "Dired Danger files extensions")
  (dired-rainbow-define danger "#FF2400" dired-danger-files-extensions)
  )

(set-face-foreground 'font-lock-comment-face "spring green")

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-height 10)
  (setq doom-modeline-hud t)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode 1)
  )
