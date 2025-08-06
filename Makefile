.PHONY: all run-examples test clean emacs tmux-start tmux-attach tmux-stop tmux-tty

# Project configuration
PROJECT_NAME ?= heavybool-cat-explorer
PROJECT_ROOT ?= $(shell pwd)

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

# Tmux and Emacs development environment
tmux-start: emacs
	@if tmux has-session -t $(PROJECT_NAME) 2>/dev/null; then \
		echo "Tmux session '$(PROJECT_NAME)' already exists. Use 'make tmux-attach' to connect."; \
	else \
		echo "Starting tmux session '$(PROJECT_NAME)' with Emacs..."; \
		tmux new-session -d -s $(PROJECT_NAME) "emacs -nw -Q -l $(PROJECT_NAME).el"; \
		echo "Session started. Use 'make tmux-attach' to connect."; \
		$(MAKE) tmux-tty; \
	fi

tmux-attach:
	@if tmux has-session -t $(PROJECT_NAME) 2>/dev/null; then \
		tmux attach-session -t $(PROJECT_NAME); \
	else \
		echo "No tmux session '$(PROJECT_NAME)' found. Use 'make tmux-start' to create one."; \
		exit 1; \
	fi

tmux-stop:
	@if tmux has-session -t $(PROJECT_NAME) 2>/dev/null; then \
		echo "Stopping tmux session '$(PROJECT_NAME)'..."; \
		tmux kill-session -t $(PROJECT_NAME); \
		echo "Session stopped."; \
	else \
		echo "No tmux session '$(PROJECT_NAME)' found."; \
	fi

tmux-tty:
	@if tmux has-session -t $(PROJECT_NAME) 2>/dev/null; then \
		echo "TTY for tmux session '$(PROJECT_NAME)':"; \
		tmux list-panes -t $(PROJECT_NAME) -F "#{pane_tty}"; \
	else \
		echo "No tmux session '$(PROJECT_NAME)' found."; \
		exit 1; \
	fi

emacs:
	@if [ ! -f $(PROJECT_NAME).el ]; then \
		echo "Error: $(PROJECT_NAME).el configuration file not found."; \
		echo "Please ensure the Emacs configuration file exists."; \
		exit 1; \
	fi
