(TeX-add-style-hook
 "StandardTemplate"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "lmodern"
    "inputenc"
    "fontenc"
    "float"
    "graphicx"
    "courier"
    "amsmath"
    "amssymb"
    "amsthm"
    "subfloat"
    "parskip"
    "fancyhdr"
    "hyperref")
   (LaTeX-add-amsthm-newtheorems
    "thm"
    "prop"
    "lem"
    "cor"
    "definition")
   (LaTeX-add-amsthm-newtheoremstyles
    "mystyle"
    "definition"))
 :latex)

