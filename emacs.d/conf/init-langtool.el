;;; init-langtool.el --- My configuration for Language Tool
;;
;; Copyright (C) 2016 Brunno dos Santos <emacs at brunno dot me>
;;
;; Author: Brunno dos Santos @squiter
;; URL: http://github.com/squiter/emacs-dotfiles
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.
;;
;;; Commentary:

;;; Code:
(use-package langtool
  :init

  (setq langtool-language-tool-jar "/opt/LanguageTool-3.4/languagetool-commandline.jar")

  (setq langtool-disabled-rules
        '(
          "WHITESPACE_RULE"
          "EN_UNPAIRED_BRACKETS"
          "COMMA_PARENTHESIS_WHITESPACE"
          "EN_QUOTES")))

(provide 'init-langtool)
;;; init-langtool.el ends here
