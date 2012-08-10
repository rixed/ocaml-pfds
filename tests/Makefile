top_srcdir = ..
REQUIRES = pfds benchmark
PROGRAMS = \
	stack_test.byte \
	heap_test.byte \
	set_test.byte \
	stream_test.byte \
	queue_test.byte \
	dequeue_test.byte \
	ring_test.byte \
	rope_test.byte \
	insertion_bench.byte \
	sortlist_test.byte \
	finite_map_test.byte

include ../make.common

all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

clean-spec:

