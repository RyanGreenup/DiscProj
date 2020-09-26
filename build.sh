#!/usr/bin/env bash

FILE="Data-Sci-Discover-Project"
FILEOUT="Data-Sci-Discover-Project_pandoc" # Org HTML Export looks better but no references

############################################################
## HTML File from Org ######################################
############################################################

# Run Pandoc Twice, once to tell pandoc to leave math alone by using mathax,
# again to swap the mathjax out with the JavaScript
# The CSS needs to be in the last one.
# The references become footnotes and depend on the tex, so that needs to be the first one
#
pandoc "${FILE}".tex --bibliography references.bib --csl resources/nature.csl --mathjax -s  -o "${FILEOUT}.html"
# Piping doesn't work right, so just save twice
# The html+tex_math ... extension is necessary to stop pandoc from mangling the math.
pandoc "${FILEOUT}" -f html+tex_math_dollars+tex_math_single_backslash \
    -c resources/github-pandoc.css -B resources/mathjax.js -s --self-contained -o "${FILEOUT}.html"

############################################################
### PDF File from LaTeX ####################################
############################################################

# I could also get the .org file exported as .tex with:
#
#emacs **/*.org --batch -f org-html-export-as-html --kill
#
# Make lstlistings float becuase didn't do it for me:
# Would a better alternative be tcolourbox?
   sed -i "s/begin{verbatim}.*/begin{lstlisting}[language={}]/" "${FILE}".tex # * Have no misbehaved verbatims
   sed -i "s/end{verbatim}.*/end{lstlisting}/" "${FILE}".tex # * Have no misbehaved verbatims
# make lstlisting float over double column
 sed -i "s/begin{lstlisting}.*/begin{lstlisting}[float=*tbph]/" "${FILE}".tex # * means to go over twocolumn
# 
#But there is a reason this didn't work
xelatex -interaction=nonstopmode "${FILE}.tex"
biber "${FILE}.tex"
xelatex -interaction=nonstopmode "${FILE}.tex"
latexmk -c
