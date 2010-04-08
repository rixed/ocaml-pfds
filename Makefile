top_srcdir = .
PKG_NAME = pfds
SOURCES  = \
	pfds_intf.ml stack_impl.ml stack_ops_impl.ml \
	btree_impl.ml finite_map_impl.ml leftist_heap_impl.ml \
	weight_leftist_heap_impl.ml heap_ops_impl.ml \
	binomial_heap_impl.ml

REQUIRES =

include make.common

all: $(ARCHIVE)

opt: $(XARCHIVE)

check: $(ARCHIVE) $(XARCHIVE)
	todo

clean-spec:

