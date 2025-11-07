#!/bin/bash -x
#. ~/.bashrc
set -euxo pipefail

TEXFILE="ALPHA_doc"

pdflatex -interaction=nonstopmode -halt-on-error "${TEXFILE}.tex"
biber "${TEXFILE}"
pdflatex -interaction=nonstopmode -halt-on-error "${TEXFILE}.tex"
pdflatex -interaction=nonstopmode -halt-on-error "${TEXFILE}.tex"