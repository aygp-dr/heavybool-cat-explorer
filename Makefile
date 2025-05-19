.PHONY: all run-examples test clean

GUILE = guile
SOURCES = src/heavybool.scm src/heavybool-quantifiers.scm
EXAMPLES = examples/associativity-test.scm
TESTS = tests/run-tests.scm

all: run-examples

run-examples:
	@echo "Running examples..."
	$(GUILE) $(EXAMPLES)

test:
	@echo "Running tests..."
	$(GUILE) $(TESTS)

clean:
	rm -f *~
	rm -f src/*~
	rm -f examples/*~
	rm -f tests/*~
	rm -f images/diagrams/*

setup:
	mkdir -p src examples tests images/diagrams
