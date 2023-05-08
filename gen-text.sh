#! /bin/bash

cd text
for GRAPH_FILE in $(find graphs/ -name *.dot -printf '%f\n')
do
    echo $GRAPH_FILE
    dot graphs/$GRAPH_FILE -Tpng -o images/$GRAPH_FILE.png
done

pdflatex main.tex
