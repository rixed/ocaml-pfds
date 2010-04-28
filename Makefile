top_srcdir = .
PKG_NAME = pfds
SOURCES  = \
	pfds_intf.ml stack_impl.ml stack_ops_impl.ml \
	btree_impl.ml finite_map_impl.ml leftist_heap_impl.ml \
	weight_leftist_heap_impl.ml heap_ops_impl.ml \
	binomial_heap_impl.ml red_black_tree_impl.ml \
	stream_intf.ml stream_impl.ml \
	batched_queue_impl.ml dequeue_impl.ml

REQUIRES =

include make.common

all: $(ARCHIVE)

opt: $(XARCHIVE)

check: $(ARCHIVE) $(XARCHIVE)
	@cd tests && $(MAKE) $(MAKEFLAGS) all opt
	@for t in stack heap set stream queue ; do \
		tests/"$$t"_test.byte ; \
		tests/"$$t"_test.opt ; \
	done

clean-spec:

