NAME = pfds

OCAMLC     = ocamlfind ocamlc
OCAMLOPT   = ocamlfind ocamlopt
OCAMLDEP   = ocamlfind ocamldep
INCS       =
override OCAMLOPTFLAGS += $(INCS) -w Ae -g -rectypes
override OCAMLFLAGS    += $(INCS) -w Ae -g -rectypes

SOURCES  = \
	pfds_intf.ml stack_impl.ml stack_ops_impl.ml \
	btree_impl.ml finite_map_impl.ml leftist_heap_impl.ml \
	weight_leftist_heap_impl.ml heap_ops_impl.ml \
	binomial_heap_impl.ml pfds_test.ml
OBJECTS  = $(SOURCES:.ml=.cmo)
XOBJECTS = $(OBJECTS:.cmo=.cmx)

ARCHIVE  = $(NAME).cma
XARCHIVE = $(ARCHIVE:.cma=.cmxa)

REQUIRES   =

.PHONY: clean all opt

all: $(ARCHIVE)
opt: $(XARCHIVE)

$(ARCHIVE): $(OBJECTS) $(CLIB)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $(OBJECTS)

$(XARCHIVE): $(XOBJECTS) $(CLIB)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(XOBJECTS)

install: all
	if test -f $(XARCHIVE) ; then extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a ; fi ; \
	ocamlfind install $(NAME) *.cmi $(ARCHIVE) META geom.ml cnt.ml $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(ARCHIVE) $(XARCHIVE)
	todo

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.o *.s .depend

# Dependencies
.depend: $(SOURCES)
	$(OCAMLDEP) -package "$(REQUIRES)" $^ > $@

include make.common
include .depend
