FPC=fpc
FPCFLAGS=-MObjFPC -Fu../core -Fu../brokers

all: test

run_tests: run_tests.lpr test_insertbuilder.pas
	$(FPC) $(FPCFLAGS) $<

test: run_tests
	./run_tests

clean:
	rm -f run_tests *.o *.ppu

.PHONY: all test clean

