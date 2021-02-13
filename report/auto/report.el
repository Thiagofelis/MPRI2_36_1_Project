(TeX-add-style-hook
 "report"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "StandardTemplate"
    "StandardTemplate10")
   (LaTeX-add-labels
    "sec:intro"
    "sec:arrays"
    "subsec:zeros"
    "subsec:samenum"
    "subsec:identical"
    "sec:takuzu"
    "subsec:main"
    "subsec:first"
    "subsec:second"
    "subsec:labelw"
    "subsec:gc"
    "sec:exq"
    "sec:conc"))
 :latex)

