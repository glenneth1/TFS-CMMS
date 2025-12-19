# TF SAFE CMMS - Makefile
# Build and run the Common Lisp CMMS application

SBCL := sbcl
PROJECT_DIR := $(shell pwd)
QUICKLISP_SETUP := $(HOME)/quicklisp/setup.lisp
CACHE_DIR := $(HOME)/.cache/common-lisp
PORT := 8080
EXECUTABLE := tfs-cmms

.PHONY: all setup deps run run-dev init-db clean clean-cache build package dist help

all: help

help:
	@echo "TF SAFE CMMS - Available targets:"
	@echo ""
	@echo "  make setup      - Install Quicklisp (if needed) and link project"
	@echo "  make deps       - Install project dependencies via Quicklisp"
	@echo "  make init-db    - Initialize the database schema"
	@echo "  make run        - Start the CMMS server on port $(PORT)"
	@echo "  make run-dev    - Start server with REPL for development"
	@echo "  make build      - Build standalone executable"
	@echo "  make package    - Create deployment package (dist/ folder)"
	@echo "  make dist       - Create distribution tarball"
	@echo "  make clean      - Remove compiled files and database"
	@echo "  make clean-cache - Clear ASDF/Quicklisp compile cache"
	@echo ""

# Check if Quicklisp is installed, install if not
setup:
	@echo "Checking Quicklisp installation..."
	@if [ ! -f "$(QUICKLISP_SETUP)" ]; then \
		echo "Installing Quicklisp..."; \
		curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp; \
		$(SBCL) --load /tmp/quicklisp.lisp \
			--eval '(quicklisp-quickstart:install)' \
			--eval '(ql:add-to-init-file)' \
			--quit; \
	else \
		echo "Quicklisp already installed."; \
	fi
	@echo "Linking project to Quicklisp local-projects..."
	@mkdir -p $(HOME)/quicklisp/local-projects
	@ln -sf "$(PROJECT_DIR)" "$(HOME)/quicklisp/local-projects/tfs-cmms"
	@echo "Setup complete."

# Install dependencies
deps:
	@echo "Installing dependencies..."
	$(SBCL) --load "$(QUICKLISP_SETUP)" \
		--eval '(ql:quickload :hunchentoot)' \
		--eval '(ql:quickload :cl-who)' \
		--eval '(ql:quickload :dbi)' \
		--eval '(ql:quickload :dbd-sqlite3)' \
		--eval '(ql:quickload :local-time)' \
		--eval '(ql:quickload :alexandria)' \
		--eval '(ql:quickload :cl-json)' \
		--eval '(ql:quickload :cl-ppcre)' \
		--quit
	@echo "Dependencies installed."

# Initialize database
init-db:
	@echo "Initializing database..."
	@mkdir -p data
	$(SBCL) --load "$(QUICKLISP_SETUP)" \
		--eval '(push #p"$(PROJECT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :tfs-cmms)' \
		--eval '(tfs-cmms:init-database)' \
		--quit
	@echo "Database initialized at data/tfs-cmms.db"

# Clear compile cache before running
clean-cache:
	@echo "Clearing compile cache..."
	@rm -rf "$(CACHE_DIR)"
	@echo "Cache cleared."

# Run the server (production mode - clears cache first)
run: clean-cache
	@echo "Starting TF SAFE CMMS on port $(PORT)..."
	$(SBCL) --load "$(QUICKLISP_SETUP)" \
		--eval '(push #p"$(PROJECT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :tfs-cmms)' \
		--eval '(tfs-cmms:start-server :port $(PORT))' \
		--eval '(loop (sleep 3600))'

# Run with REPL for development
run-dev:
	@echo "Starting TF SAFE CMMS in development mode on port $(PORT)..."
	@echo "Use (tfs-cmms:stop-server) to stop, or Ctrl+D to exit."
	$(SBCL) --load "$(QUICKLISP_SETUP)" \
		--eval '(push #p"$(PROJECT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :tfs-cmms)' \
		--eval '(tfs-cmms:start-server :port $(PORT))'

# Build standalone executable
build: clean-cache
	@echo "Building standalone executable..."
	@mkdir -p dist
	$(SBCL) --load "$(QUICKLISP_SETUP)" \
		--eval '(push #p"$(PROJECT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :tfs-cmms)' \
		--eval '(sb-ext:save-lisp-and-die "dist/$(EXECUTABLE)" :toplevel (lambda () (tfs-cmms:start-server :port 8080) (loop (sleep 3600))) :executable t :compression t)'
	@echo "Executable built: dist/$(EXECUTABLE)"

# Create deployment package
package: build
	@echo "Creating deployment package..."
	@mkdir -p dist/static dist/data
	@cp -r static/* dist/static/
	@touch dist/data/.gitkeep
	@cp README.md dist/ 2>/dev/null || true
	@echo "#!/bin/bash" > dist/start.sh
	@echo "cd \"\$$(dirname \"\$$0\")\"" >> dist/start.sh
	@echo "echo \"Starting TF SAFE CMMS on http://localhost:8080\"" >> dist/start.sh
	@echo "./$(EXECUTABLE)" >> dist/start.sh
	@chmod +x dist/start.sh
	@echo ""
	@echo "=== Deployment Package Created ==="
	@echo "Location: dist/"
	@echo ""
	@echo "Contents:"
	@echo "  dist/$(EXECUTABLE)  - Main application executable"
	@echo "  dist/static/        - CSS, JS, images"
	@echo "  dist/data/          - Database directory (auto-created)"
	@echo "  dist/start.sh       - Startup script"
	@echo ""
	@echo "To deploy:"
	@echo "  1. Copy the 'dist' folder to your server"
	@echo "  2. Run: ./start.sh"
	@echo "  3. Access: http://server-ip:8080"
	@echo ""

# Create tarball for distribution
dist: package
	@echo "Creating distribution tarball..."
	@cd dist && tar -czvf ../tfs-cmms-$(shell date +%Y%m%d).tar.gz *
	@echo "Distribution created: tfs-cmms-$(shell date +%Y%m%d).tar.gz"

# Clean up
clean:
	@echo "Cleaning up..."
	rm -f data/tfs-cmms.db
	rm -rf dist/
	rm -f tfs-cmms-*.tar.gz
	@rm -rf "$(CACHE_DIR)"
	@echo "Clean complete."
