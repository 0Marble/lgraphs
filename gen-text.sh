#! /bin/bash

cd text
for GRAPH_FILE in $(find graphs/ -name *.dot -printf '%f\n')
do
    echo $GRAPH_FILE
    dot graphs/$GRAPH_FILE -Tsvg -o images/$GRAPH_FILE.svg
done

pdflatex --shell-escape main.tex
