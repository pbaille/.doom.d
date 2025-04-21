;;; ef-dream-theme.el --- Legible dark theme with gold and nuanced colors -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/ef-themes
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `ef-themes' are a collection of light and dark themes for GNU
;; Emacs whose goal is to provide colorful ("pretty") yet legible
;; options for users who want something with a bit more flair than the
;; `modus-themes' (also designed by me).

;;; Code:



(require 'ef-themes)

(deftheme pb-ef-dream
  "Legible dark theme with gold and nuanced colors."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'ef)


(require 'pb-color)

(defun decline-theme-color (name c)
  "Emits color variation bindings for C named NAME."
  `((,name ,c)
    (,(intern (concat (symbol-name name) "-warmer"))
     ,(pb-color/warm c 0.1))
    (,(intern (concat (symbol-name name) "-cooler"))
     ,(pb-color/cool c 0.1))
    (,(intern (concat (symbol-name name) "-faint"))
     ,(pb-color/desaturate c 0.4))))

(defconst pb-ef-dream-palette
  (let ((red "#ff6f6f")
        (green "#51b04f")
        (yellow "#ffbc36")
        (blue "#57b0ff")
        (magenta "#ffaacf")
        (cyan "#6fb3c0")

        (bg "#232025")
        (fg (pb-color "#efd5c5"
                      (darken .2)))
        )

    `(
;;; Basic values

      (bg-main     ,bg)
      (bg-dim      "#322f34")
      (bg-alt      "#3b393e")
      (bg-active   "#5b595e")
      (bg-inactive "#2a272c")

      (fg-main     ,fg)
      (fg-dim      "#8f8886")
      (fg-alt      "#b0a0cf")

;;; Basic hues for foreground values

      ,@(decline-theme-color 'red red)
      ,@(decline-theme-color 'green green)
      ,@(decline-theme-color 'yellow yellow)
      ,@(decline-theme-color 'blue blue)
      ,@(decline-theme-color 'magenta magenta)
      ,@(decline-theme-color 'cyan cyan)

;;; Basic hues for background values

      (bg-red-intense     "#a02f50")
      (bg-green-intense   "#30682f")
      (bg-yellow-intense  "#8f665f")
      (bg-blue-intense    "#4f509f")
      (bg-magenta-intense "#885997")
      (bg-cyan-intense    "#0280b9")

      (bg-red-subtle      "#682a33")
      (bg-green-subtle    "#2f502f")
      (bg-yellow-subtle   "#5a4020")
      (bg-blue-subtle     "#3a3980")
      (bg-magenta-subtle  "#553977")
      (bg-cyan-subtle     "#22526a")

;;; Diffs

      (bg-added          "#304a4f")
      (bg-added-faint    "#16383f")
      (bg-added-refine   "#2f6767")
      (fg-added          "#a0d0f0")

      (bg-changed        "#51512f")
      (bg-changed-faint  "#40332f")
      (bg-changed-refine "#64651f")
      (fg-changed        "#dada90")

      (bg-removed        "#5a3142")
      (bg-removed-faint  "#4a2034")
      (bg-removed-refine "#782a4a")
      (fg-removed        "#f0bfcf")

;;; Graphs

      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#0fed00")
      (bg-graph-green-1   "#007800")
      (bg-graph-yellow-0  "#f1e00a")
      (bg-graph-yellow-1  "#b08940")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#bf94fe")
      (bg-graph-magenta-1 "#5f509f")
      (bg-graph-cyan-0    "#47dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special hues

      (bg-mode-line       "#675072")
      (fg-mode-line       "#fedeff")
      (bg-completion      "#503240")
      (bg-hover           "#4a3c2b")
      (bg-hover-secondary "#665f7a")
      (bg-hl-line         "#412f4f")
      (bg-paren           "#885566")
      (bg-err             "#501a2d")     ; check with err
      (bg-warning         "#4e3930")     ; check with warning
      (bg-info            "#0f3f4f")     ; check with info

      (border        "#635850")
      (cursor        "#f3c09a")
      (fg-intense    "#ffffff")

      (modeline-err     "#ffbfbf")
      (modeline-warning "#dfd443")
      (modeline-info    "#7fefff")

      (underline-err     "#c04f9f")
      (underline-warning "#c0b000")
      (underline-info    "#229fb2")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#6f6600")

;;; Mappings

;;;; General mappings

      (bg-fringe unspecified)
      (fg-fringe unspecified)

      (err magenta-warmer)
      (warning yellow-warmer)
      (info cyan)

      (link yellow-cooler)
      (link-alt cyan-warmer)
      (name cyan-warmer)
      (keybind cyan)
      (identifier yellow-cooler)
      (prompt magenta)

      (bg-region bg-dim)
      (fg-region unspecified)

;;;; Code mappings

      (builtin ,(pb-color red
                          (blend yellow .7)
                          (desaturate .4)
                          (darken .1)))
      (comment ,(pb-color fg
                          (darken 0.3)))
      (constant blue-warmer)
      (fnname ,(pb-color cyan
                         (saturate .3)))
      (keyword ,(pb-color yellow
                          (desaturate .3)))
      (preprocessor cyan-cooler)
      (docstring ,(pb-color fg
                            (darken 0.3)))
      (string red-faint)
      (type cyan)
      (variable ,(pb-color cyan
                           (warm .2)
                           (saturate .3)))
      (rx-escape cyan-cooler)            ; compare with `string'
      (rx-construct red-cooler)

;;;; Accent mappings

      (accent-0 yellow-cooler)
      (accent-1 red-cooler)
      (accent-2 magenta-warmer)
      (accent-3 blue-warmer)

;;;; Date mappings

      (date-common magenta-faint)
      (date-deadline red-cooler)
      (date-event fg-alt)
      (date-holiday red-warmer)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow-cooler)
      (date-weekday magenta)
      (date-weekend blue-faint)

;;;; Prose mappings

      (prose-code blue-warmer)
      (prose-done cyan-warmer)
      (prose-macro green-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-table-formula info)
      (prose-tag yellow-faint)
      (prose-todo yellow-warmer)
      (prose-verbatim magenta-warmer)

;;;; Mail mappings

      (mail-cite-0 yellow-cooler)
      (mail-cite-1 red-cooler)
      (mail-cite-2 cyan-faint)
      (mail-cite-3 green-faint)
      (mail-part red-faint)
      (mail-recipient cyan-faint)
      (mail-subject magenta-warmer)
      (mail-other magenta-faint)

;;;; Search mappings

      (bg-search-current bg-yellow-intense)
      (bg-search-lazy bg-blue-intense)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 bg-magenta-intense)
      (bg-search-rx-group-1 bg-green-intense)
      (bg-search-rx-group-2 bg-red-subtle)
      (bg-search-rx-group-3 bg-cyan-subtle)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Terminal mappings

      (bg-term-black           "black")
      (fg-term-black           "black")
      (bg-term-black-bright    "gray35")
      (fg-term-black-bright    "gray35")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green-warmer)
      (fg-term-green           green-warmer)
      (bg-term-green-bright    green-cooler)
      (fg-term-green-bright    green-cooler)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-warmer)
      (fg-term-yellow-bright   yellow-warmer)

      (bg-term-blue            blue-warmer)
      (fg-term-blue            blue-warmer)
      (bg-term-blue-bright     blue-cooler)
      (fg-term-blue-bright     blue-cooler)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "gray65")
      (fg-term-white           "gray65")
      (bg-term-white-bright    "white")
      (fg-term-white-bright    "white")

;;;; Rainbow mappings

      (rainbow-0 fg-dim)
      (rainbow-1 fg-dim)
      (rainbow-2 fg-dim)
      (rainbow-3 fg-dim)
      (rainbow-4 fg-dim)
      (rainbow-5 fg-dim)
      (rainbow-6 fg-dim)
      (rainbow-7 fg-dim)
      (rainbow-8 fg-dim)))
  "The `pb-ef-dream' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

(defcustom pb-ef-dream-palette-overrides nil
  "Overrides for `pb-ef-dream-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Ef themes,
refer to `pb-ef-themes-common-palette-overrides'.

To preview the palette entries, use `pb-ef-themes-preview-colors' or
`pb-ef-themes-preview-colors-current' (read the documentation for
further details)."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(ef-themes-theme pb-ef-dream pb-ef-dream-palette pb-ef-dream-palette-overrides)

(provide-theme 'pb-ef-dream)

;;; pb-ef-dream-theme.el ends here
