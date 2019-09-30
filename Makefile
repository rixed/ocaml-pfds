# vim:ft=make

# Generic rules:

OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep
OCAMLDOC = ocamlfind ocamldoc -keep-code -html -all-params -colorize-code
override OCAMLOPTFLAGS += -I . -w Ael -g -annot
override OCAMLFLAGS    += -I . -w Ael -g -annot

.PHONY: all opt clean install uninstall reinstall doc

PFDS_SOURCES = \
	pfds_misc.ml \
	pfds_intf.ml iterable_impl.ml stack_ops_impl.ml stack_impl.ml \
	set_ops_impl.ml btree_impl.ml finite_map_impl.ml leftist_heap_impl.ml \
	weight_leftist_heap_impl.ml heap_ops_impl.ml \
	binomial_heap_impl.ml red_black_tree_impl.ml \
	lStream.mli lStream.ml \
	batched_queue_impl.ml dequeue_impl.ml \
	ring_impl.ml rope_impl.ml sortlist_impl.ml \
	iterable_test.ml prefix_tree.ml

INSTALLED = \
	pfds.cmxa \
	pfds.cma \
	pfds.a \
	$(PFDS_SOURCE:.ml=.cmi) \
	$(PFDS_SOURCE:.ml=.cmx) \
	META

all: $(INSTALLED)

pfds.a: pfds.cmxa

pfds.cma: \
		$(patsubst %.mli,%.cmi,$(filter %.mli, $(PFDS_SOURCES))) \
		$(patsubst %.ml,%.cmo,$(filter %.ml, $(PFDS_SOURCES)))
	$(OCAMLC) -a -o $@ -linkpkg $(OCAMLFLAGS) $(patsubst %.ml,%.cmo,$(filter %.ml, $(PFDS_SOURCES)))

pfds.cmxa: \
		$(patsubst %.mli,%.cmi,$(filter %.mli, $(PFDS_SOURCES))) \
		$(patsubst %.ml,%.cmx,$(filter %.ml, $(PFDS_SOURCES)))
	$(OCAMLOPT) -a -o $@ $(OCAMLOPTFLAGS) $(patsubst %.ml,%.cmx,$(filter %.ml, $(PFDS_SOURCES)))

install: $(INSTALLED)
	ocamlfind install pfds $(INSTALLED)

uninstall:
	ocamlfind remove pfds

reinstall: uninstall install

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .opt .byte

lStream.cmx: lStream.cmi
lStream.cmo: lStream.cmi

%.cmi: %.mli
	@echo 'Compiling $@ (interface)'
	@$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmx %.cmt: %.ml
	@echo 'Compiling $@'
	@$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.opt: %.ml  pfds.cmxa
	$(OCAMLOPT) -o $@ -package benchmark pfds.cmxa -linkpkg $(OCAMLOPTFLAGS) $^

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.byte: %.ml pfds.cma
	$(OCAMLC) -o $@ -package benchmark pfds.cma -linkpkg $(OCAMLFLAGS) $^

# Clean up

clean:
	$(RM) *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt .depend *.annot
	$(RM) tests/*.cm[ioxa] tests/*.cmxa tests/*.a tests/*.s tests/*.o tests/*.byte tests/*.opt tests/*.annot

# Dependencies

.depend: $(SOURCES)
	$(OCAMLDEP) -I .. $^ > $@

# Doc

doc: $(wildcard *.ml)
	mkdir -p $@
	$(OCAMLDOC) -d doc $^

# Tests

TEST_PROGRAMS = \
	tests/stack_test.byte \
	tests/heap_test.byte \
	tests/set_test.byte \
	tests/stream_test.byte \
	tests/queue_test.byte \
	tests/dequeue_test.byte \
	tests/ring_test.byte \
	tests/rope_test.byte \
	tests/insertion_bench.byte \
	tests/sortlist_test.byte \
	tests/finite_map_test.byte \
	tests/inserts.byte \
	tests/prefix_tree_test.byte

check: $(TEST_PROGRAMS) $(TEST_PROGRAMS:.byte=.opt)
	@for t in $^ ; do \
		$$t || echo "FAILED: $$t" ; \
	done ; \
	echo "OK"
