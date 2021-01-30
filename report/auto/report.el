(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "StandardTemplate"
    "StandardTemplate10")
   (LaTeX-add-labels
    "sec:intro"
    "sec:arrays"
    "subsec:zeros"))
 :latex)

