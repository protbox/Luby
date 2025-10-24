#!/bin/bash
set -e

INSTALL_DIR="/usr/local/luby"
BIN_LINK="/usr/local/bin/luby"
BASE_URL = "https://github.com/protbox/Luby/raw/refs/heads/main"

echo "Installing Luby..."

# Create install directory
mkdir -p "$INSTALL_DIR"

# Download files
echo "Downloading files..."
curl -fsSL "$BASE_URL/luby.lua" -o "$INSTALL_DIR/luby.lua"
curl -fsSL "$BASE_URL/llfs.lua" -o "$INSTALL_DIR/llfs.lua"
curl -fsSL "$BASE_URL/lulpeg.lua" -o "$INSTALL_DIR/lulpeg.lua"

# Make main script executable
chmod +x "$INSTALL_DIR/luby.lua"

# Create symlink in /usr/local/bin
ln -sf "$INSTALL_DIR/luby.lua" "$BIN_LINK"

echo "Luby installed successfully. Enjoy."
