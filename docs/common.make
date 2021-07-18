PANDOC=pandoc
MD_DOCS = $(wildcard *.md)

info: $(DOCNAME).info
$(DOCNAME).texi: $(MD_DOCS)
	$(PANDOC) -o $@ $^

clean:
	rm -f $(DOCNAME).texi $(DOCNAME).info
