R=GT
PDF=$(R).pdf
.SUFFIXES: .pdf .tex
.PHONY: $(PDF) clean

all: $(PDF)

FILES_TEX= $(R).tex intro.tex related.tex

$(R).pdf: $(FILES_TEX)
	pdflatex $<
	bibtex $(R).aux
	pdflatex $<
	pdflatex $<

clean:
	$(RM) *.bak *.out $(PDF)

