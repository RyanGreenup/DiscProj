# Final Report
#
# Why not ~latexmk -pdf~ ?
  # Does not export the org file.
  # Sometimes it just doesn't work so ~watch make~ is needed not ~latexmk -pvc~
    # To replace ~-pvc~ try ~watch make~ or ~while true; do make --silent; sleep 1; done~
  # Practice
# Probably better to just have org mode auto export the latex

# @file
# @version 0.1
#
# Use Tabs not Spaces
# https://opensource.com/article/18/8/what-how-makefile
# https://www.gnu.org/software/make/manual/html_node/How-Make-Works.html
# all: Report.tex Report.pdf Report.bbl Report.html
all: Report.pdf Report.bbl Report.html
.PHONY: all


clean :
	@ latexmk -c
	@ rm Report.run.xml
clear:
	latexmk -c
	@ rm Report.tex Report.pdf Report.pdf Report.run.xml Report.dvi Report.pdf Report.run.xml Report.xdv # Report.bbl

# * Rules

# Report.tex : Report.org
# It seems to only work if it exports from emacs as well.
# I wonder if there is a way to make the init file more portable?
	@ emacs -l ~/.emacs.d/init.el Report.org --batch -f org-latex-export-to-latex --kill

Report.pdf : Report.tex
# there was a reason I didn't use latexmk, I just can't remember
	@ xelatex -interaction=nonstopmode -shell-escape Report.tex

Report.bcf :
	@ xelatex -interaction=nonstopmode -shell-escape Report.tex

Report.bbl : Report.bcf
	@ biber Report.bcf
	@ xelatex -interaction=nonstopmode -shell-escape Report.tex


Report.html : Report.tex
# Make the HTML
#
# Doesn't tolerate missing images
#	@ pandoc Report.tex -s -t html                                \
#	-c resources/style.css                                      \
#	--csl resources/nature.csl --bibliography ../Resources/references.bib    \
#	--mathjax                                                   \
#	-o Report.html
#	@ pandoc -s --self-contained -B resources/mathjax.js        \
#	-c resources/style.css                                      \
#	Report.html -o Report.html
