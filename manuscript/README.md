This folder contains the manuscript and appendix source files needed to compile the submission package locally.

Requirements:
- `latexmk`
- `pdflatex`
- `bibtex`

Compile commands:

```bash
latexmk -pdf -interaction=nonstopmode -halt-on-error Paper_UPF_Argentina_manuscript.tex
latexmk -pdf -interaction=nonstopmode -halt-on-error Online_Appendix_UPF_Argentina.tex
```
