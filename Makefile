.PHONY: all run-examples test clean

GUILE = guile
SOURCES = src/heavybool.scm src/heavybool-quantifiers.scm src/heavybool-monad.scm
EXAMPLES = examples/associativity-test.scm examples/strange-loops.scm
TESTS = tests/run-tests.scm

all: run-examples

run-examples:
	@echo "Running examples..."
	@for example in $(EXAMPLES); do \
		echo "=== Running $$example ==="; \
		GUILE_AUTO_COMPILE=0 $(GUILE) $$example; \
		echo; \
	done

test:
	@echo "Running tests..."
	GUILE_AUTO_COMPILE=0 $(GUILE) $(TESTS)

clean:
	rm -f *~
	rm -f src/*~
	rm -f examples/*~
	rm -f tests/*~
	rm -f images/diagrams/*

setup:
	mkdir -p src examples tests images/diagrams
