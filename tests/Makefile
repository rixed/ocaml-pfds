top_srcdir = ..
REQUIRES = pfds benchmark
PROGRAMS = \
	stack_test.byte \
	heap_test.byte \
	set_test.byte \
	stream_test.byte \
	queue_test.byte \
	insertion_bench.byte

include ../make.common

all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

clean-spec:

