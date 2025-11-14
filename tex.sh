#!/bin/bash -x
#. ~/.bashrc
set -euxo pipefail

TEXFILE="AIMALPHA_documentation"

pdflatex -interaction=nonstopmode -halt-on-error "${TEXFILE}.tex"
biber "${TEXFILE}"
pdflatex -interaction=nonstopmode -halt-on-error "${TEXFILE}.tex"
pdflatex -interaction=nonstopmode -halt-on-error "${TEXFILE}.tex"