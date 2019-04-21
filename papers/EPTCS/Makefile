.PHONY: main.pdf clean

all: main.pdf

OCANREN_TEX=main.tex intro.tex related.tex

main.pdf: $(OCANREN_TEX)
	pdflatex $<
	bibtex main.aux
	pdflatex $<
	pdflatex $<

clean:
	$(RM) *.bak *.out

