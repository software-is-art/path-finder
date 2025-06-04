# PathFinder LISP Development Makefile
# Provides convenient shortcuts for common development tasks

.PHONY: help setup build test fmt lint clean ci install-hooks docs

help: ## Show this help message
	@echo "PathFinder LISP Development Commands"
	@echo "===================================="
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

setup: ## Install dependencies and setup development environment
	devbox run setup

build: ## Check syntax and compile
	devbox run build

test: ## Run comprehensive test suite
	devbox run test

fmt: ## Format all Racket code
	devbox run fmt

check-fmt: ## Check code formatting without modifying files
	devbox run check-fmt

lint: ## Run static analysis
	devbox run lint

clean: ## Clean compiled files
	devbox run clean

ci: ## Run full CI pipeline (format, lint, build, test)
	devbox run ci

install-hooks: ## Install git pre-commit hooks
	@echo "Installing pre-commit hooks..."
	@cp scripts/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "✅ Pre-commit hooks installed"

docs: ## Generate documentation
	devbox run docs

repl: ## Start interactive REPL
	devbox run repl

run: ## Run PathFinder LISP interpreter
	devbox run run

version: ## Show version information
	devbox run version

# Development workflow shortcuts
dev-setup: setup install-hooks ## Complete development setup
	@echo "🚀 Development environment ready!"

check: lint test ## Quick development check (lint + test)

# Release workflow
release-check: clean ci ## Full release validation
	@echo "✅ Ready for release"