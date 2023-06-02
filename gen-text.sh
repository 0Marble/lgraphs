#! /bin/bash

cd text

[ ! -d images ] && mkdir images

for GRAPH_FILE in $(find graphs/ -name *.dot -printf '%f\n')
do
    echo $GRAPH_FILE
    dot graphs/$GRAPH_FILE -Tsvg -o images/$GRAPH_FILE.svg
done

latexmk -synctex=1 -interaction=nonstopmode -file-line-error -xelatex -outdir=out main.tex --shell-escape

cd ../presentation

[ ! -d images ] && mkdir images

latexmk -synctex=1 -interaction=nonstopmode -file-line-error -xelatex -outdir=out main.tex --shell-escape

