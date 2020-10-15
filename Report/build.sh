#!/bin/bash
# Don't forget to adjust the permissions with:
#chmod +x ~/somecrazyfolder/script1

## Program


### Description
# If the makefile becomes a pain in the ass

### Code
emacs -l ~/.emacs.d/init.el Report.org  --batch -f org-latex-export-to-latex --kill
latexmk -pdf -xelatex -f -interaction=nonstopmode
latexmk -c
# rm  Report.run.xml Report.dvi Report.run.xml Report.xdv # Report.bbl


## This is what the makefile does:
    # emacs -l ~/.emacs.d/init.el Report.org  --batch -f org-latex-export-to-latex --kill
    # biber Report.bcf
    # xelatex -interaction=nonstopmode -shell-escape Report.tex
    # latexmk -c
    # zathura Report.pdf & disown




## vim:fdm=expr:fdl=0
## vim:fde=getline(v\:lnum)=~'^##'?'>'.(matchend(getline(v\:lnum),'##*')-2)\:'='



# Wrapping the code blocks in tcolorobx will require working in TeX, you simply won't be to make it work from org-mode.
# This will atleast mean the fix merely involves reaarranging lines:

# sd \
#     '(\\\begin.*\{.*minted.*\}.*)' '$1 \n \\\begin{tcolorbox}' |\
#     sd '(\\\end.*\{.*minted.*\}.*)' '\\\end{tcolorbox}\n$1' \
#     Report.tex
