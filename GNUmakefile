# Good for tags.
DARCS_FILES = $(wildcard src/[A-Z]*.hs src/*/[A-Z]*.hs src/*/*/[A-Z]*.hs) \
	      $(wildcard src/*/*/*/[A-Z]*.hs src/*/*/*/*/[A-Z]*.hs) \
	      $(wildcard darcs/[A-Z]*.hs) \
	      $(wildcard harness/[A-Z]*.hs harness/*/[A-Z]*.hs) \
	      $(wildcard harness/*/*/[A-Z]*.hs harness/*/*/*/[A-Z]*.hs) \
	      $(wildcard harness/*/*/*/*/[A-Z]*.hs)

tags: $(DARCS_FILES) src/*.c
	hasktags -c $(filter %.lhs %.hs,$^)
	ctags -a $(filter %.c,$^)

# TAGS is for etags, whereas tags is for ctags
TAGS: $(DARCS_FILES) src/*.c
	hasktags -e $(filter %.lhs %.hs,$^)
	etags -a $(filter %.c,$^)

clean:
	rm -f TAGS tags
