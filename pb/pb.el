;;; pb.el --- Multipurpose personal package -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Pierre Baille

;; Author: Pierre Baille <pierrebaille@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/pbaille/pb.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; personal utility belt

;;; Code:

(load "~/.doom.d/pb/km.el")
(load "~/.doom.d/pb/pb-sexpr.el")

(load "~/.doom.d/pb/pb-cider.el")
(load "~/.doom.d/pb/pb-symex.el")
(load "~/.doom.d/pb/pb-dired.el")
(load "~/.doom.d/pb/pb-elisp.el")
(load "~/.doom.d/pb/pb-fennel.el")
(load "~/.doom.d/pb/pb-fold.el")
(load "~/.doom.d/pb/pb-gptel.el")
(load "~/.doom.d/pb/pb-ibuffer.el")
(load "~/.doom.d/pb/pb-misc.el")
(load "~/.doom.d/pb/pb-reapl.el")
(load "~/.doom.d/pb/pb-sidebars.el")
(load "~/.doom.d/pb/pb-templates.el")
(load "~/.doom.d/pb/pb-udp.el")
(load "~/.doom.d/pb/pb-walk.el")

(provide 'pb)
;;; pb.el ends here
