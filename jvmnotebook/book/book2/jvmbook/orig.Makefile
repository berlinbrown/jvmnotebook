##################################################
# Makefile
# Berlin Brown
##################################################

MAIN    = botlist-net_langs_032008
FIGDIR  = figures
FIGURES =	$(FIGDIR)/mainmunlogo.eps

.SUFFIXES: 	.tex .dvi .eps .ps .pdf
all: $(MAIN).dvi

FILES = botlist-net_langs_032008.tex \
		overview.tex \
		java.tex     \
		jruby.tex    \
		python.tex   \
		scala.tex    \
		haskell.tex  \
		erlang.tex   \
		testdriven.tex \
		factor.tex   \
		future.tex   \
		ref.bib  \

$(MAIN).aux: ref.bib $(MAIN).tex
	latex $(MAIN)
	bibtex $(MAIN)
	latex $(MAIN)
	latex $(MAIN)


$(MAIN).dvi: $(MAIN).tex $(FILES)
# Run latex on the main and then bibtex and then latex again.
	latex $*.tex; 
	bibtex $*;
	latex $*.tex;
	while grep -s 'Rerun' $*.log 2> /dev/null; do  \
		latex $*.tex;  \
	done

clean:
	rm -vf *.aux *.dvi *.log *.toc

## End of File