##
# Final Report
#
# @file
# @version 0.1
#
# Use Tabs not Spaces
# https://opensource.com/article/18/8/what-how-makefile
# https://www.gnu.org/software/make/manual/html_node/How-Make-Works.html
build_report: ./Report.org
	make_latex make_refs make_latex clean


make_latex:
# there was a reason I didn't use latexmk, I just can't remember
	@ xelatex -interaction=nonstopmode -shell-escape Report.tex
make_refs:
	@ biber Report.bcf
clean:
	@ latexmk -c

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
