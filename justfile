default:
    @just --list

setup: _check-system-deps _create-venv
    @echo "==> Environment setup complete!"

_check-system-deps:
    @echo "==> Checking system-level packages (QGIS & PyMOL)..."
    @python3 -c "import qgis.core" 2>/dev/null || { \
        echo "Error: System-level 'qgis' Python module not found!"; \
        echo "Please install it via system package manager: sudo pacman -S qgis"; \
        exit 1; \
    }
    @python3 -c "import pymol" 2>/dev/null || { \
        echo "Error: System-level 'pymol' Python module not found!"; \
        echo "Please install it via AUR: yay -S pymol"; \
        exit 1; \
    }
    @echo "✓ System dependencies verified successfully."

_create-venv:
    @if [ -d ".venv" ]; then \
        echo "==> Virtual environment (.venv) already exists. Skipping creation."; \
    else \
        echo "==> Creating virtual environment using /usr/bin/python with --system-site-packages..."; \
        uv venv --python /usr/bin/python --system-site-packages; \
    fi
    @echo "==> Syncing project dependencies with uv..."
    @uv sync

clean:
    @echo "==> Cleaning up virtual environment..."
    @rm -rf .venv
    @echo "Clean complete."
