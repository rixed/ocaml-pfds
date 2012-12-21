top_srcdir = .
PKG_NAME = pfds
SOURCES  = \
	pfds_intf.ml iterable_impl.ml stack_ops_impl.ml stack_impl.ml \
	set_ops_impl.ml btree_impl.ml finite_map_impl.ml leftist_heap_impl.ml \
	weight_leftist_heap_impl.ml heap_ops_impl.ml \
	binomial_heap_impl.ml red_black_tree_impl.ml \
	lStream.mli lStream.ml \
	batched_queue_impl.ml dequeue_impl.ml \
	ring_impl.ml rope_impl.ml sortlist_impl.ml \
	iterable_test.ml

REQUIRES = batteries

.PHONY: all
all: byte opt

include make.common

byte: $(ARCHIVE)
opt: $(XARCHIVE)

check: $(ARCHIVE) $(XARCHIVE)
	@cd tests && $(MAKE) $(MAKEFLAGS) all opt
	@for t in tests/*_test.byte tests/*_test.opt ; do \
		$$t || echo "FAILED" ; \
	done ; \
	echo "OK"

clean-spec:

