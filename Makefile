DEPS_DIR := deps

LUAJIT_REPO := https://luajit.org/git/luajit.git
LFS_REPO    := https://github.com/lunarmodules/luafilesystem.git
LULPEG_REPO := https://github.com/pygy/LuLPeg.git

LUAJIT_DIR  := $(DEPS_DIR)/luajit
LFS_DIR     := $(DEPS_DIR)/luafilesystem
LULPEG_DIR  := $(DEPS_DIR)/lulpeg

SRC_DIR := src
DIST_DIR := dist
BUILD_DIR := build

LUAJIT_DIR := $(DEPS_DIR)/luajit/src
LFS_DIR := $(DEPS_DIR)/luafilesystem
LULPEG_FILE := $(DEPS_DIR)/lulpeg/lulpeg.lua

# Source files
LUBY_SRC := $(SRC_DIR)/luby.lua

# Setup submodules
.PHONY: init_submodules submodules update_submodules

init_submodules:
	@mkdir -p $(DEPS_DIR)
	@grep -q "path = $(LUAJIT_DIR)" .gitmodules 2>/dev/null || git submodule add $(LUAJIT_REPO) $(LUAJIT_DIR)
	@grep -q "path = $(LFS_DIR)" .gitmodules 2>/dev/null      || git submodule add $(LFS_REPO)    $(LFS_DIR)
	@grep -q "path = $(LULPEG_DIR)" .gitmodules 2>/dev/null   || git submodule add $(LULPEG_REPO) $(LULPEG_DIR)
	@git submodule sync --recursive

submodules:
	@git submodule update --init --recursive

update_submodules:
	@git submodule update --remote --merge --recursive


# Targets
# NOTICE: mingw and luajit doesn't cross-compile properly so Windows is not really 
# supported right now. If you want to get it working, you're welcome to try
# I don't own a mac, so I can't even test.
# Linux seems to work perfectly fine
LINUX_TARGET := $(DIST_DIR)/linux/luby
WINDOWS_TARGET := $(DIST_DIR)/windows/luby.exe
MACOS_TARGET := $(DIST_DIR)/macos/luby

# Compilers
CC_LINUX := gcc
CC_WINDOWS := x86_64-w64-mingw32-gcc
CC_MACOS := clang

# Default target
.PHONY: all
all: linux

# Create directories
.PHONY: dirs
dirs:
	@mkdir -p $(DIST_DIR)/linux $(DIST_DIR)/windows $(DIST_DIR)/macos $(BUILD_DIR)

.PHONY: deps
deps: submodules
	@echo "==> Dependencies ready!"

# Build LuaJIT for Linux
$(BUILD_DIR)/libluajit-linux.a: deps
	@echo "==> Building LuaJIT for Linux..."
	cd $(DEPS_DIR)/luajit && $(MAKE) clean > /dev/null 2>&1 || true
	cd $(DEPS_DIR)/luajit && $(MAKE) BUILDMODE=static -j4
	@cp $(LUAJIT_DIR)/libluajit.a $@

# Build LuaJIT for Windows
$(BUILD_DIR)/libluajit-windows.a: deps
	@echo "==> Building LuaJIT for Windows..."
	cd $(DEPS_DIR)/luajit && $(MAKE) clean > /dev/null 2>&1 || true
	cd $(DEPS_DIR)/luajit && $(MAKE) HOST_CC="gcc -m64" CROSS=x86_64-w64-mingw32- \
		TARGET_SYS=Windows BUILDMODE=static \
		XCFLAGS="-DLUAJIT_ENABLE_GC64" -j4
	@cp $(LUAJIT_DIR)/libluajit.a $@

# Build LuaFileSystem for Linux
$(BUILD_DIR)/liblfs-linux.a: deps
	@echo "==> Building LuaFileSystem for Linux..."
	$(CC_LINUX) -c -O2 -fPIC -I$(LUAJIT_DIR) $(LFS_DIR)/src/lfs.c -o $(BUILD_DIR)/lfs-linux.o
	ar rcs $@ $(BUILD_DIR)/lfs-linux.o

# Build LuaFileSystem for Windows
$(BUILD_DIR)/liblfs-windows.a: deps
	@echo "==> Building LuaFileSystem for Windows..."
	$(CC_WINDOWS) -c -O2 -I$(LUAJIT_DIR) $(LFS_DIR)/src/lfs.c -o $(BUILD_DIR)/lfs-windows.o
	x86_64-w64-mingw32-ar rcs $@ $(BUILD_DIR)/lfs-windows.o

# Build for Linux
.PHONY: linux
linux: dirs $(BUILD_DIR)/libluajit-linux.a $(BUILD_DIR)/liblfs-linux.a
	@echo "==> Building Luby for Linux..."
	@cd $(SRC_DIR) && luastatic luby.lua ../$(LULPEG_FILE) \
		../$(BUILD_DIR)/libluajit-linux.a \
		../$(BUILD_DIR)/liblfs-linux.a \
		-I../$(LUAJIT_DIR) -static
	@mv $(SRC_DIR)/luby $(LINUX_TARGET)
	@rm -f $(SRC_DIR)/luby.luastatic.c
	@echo "==> Linux build complete: $(LINUX_TARGET)"
	@ls -lh $(LINUX_TARGET)

# Build for Windows
.PHONY: windows
windows: dirs $(BUILD_DIR)/libluajit-windows.a $(BUILD_DIR)/liblfs-windows.a
	@echo "==> Building Luby for Windows..."
	@cd $(SRC_DIR) && luastatic luby.lua ../$(LULPEG_FILE) \
		../$(BUILD_DIR)/libluajit-windows.a \
		../$(BUILD_DIR)/liblfs-windows.a \
		-I../$(LUAJIT_DIR)
	@$(CC_WINDOWS) -Os $(SRC_DIR)/luby.luastatic.c \
		-Wl,--whole-archive $(BUILD_DIR)/libluajit-windows.a -Wl,--no-whole-archive \
		$(BUILD_DIR)/liblfs-windows.a \
		-static -static-libgcc -o $(WINDOWS_TARGET) \
		-I$(LUAJIT_DIR) -lm -lmingwex -Wl,--export-all-symbols
	@x86_64-w64-mingw32-strip $(WINDOWS_TARGET)
	@rm -f $(SRC_DIR)/luby.luastatic.c
	@echo "==> Windows build complete: $(WINDOWS_TARGET)"
	@ls -lh $(WINDOWS_TARGET)

# Build all platforms
.PHONY: release
release: linux windows
	@echo "==> All builds complete!"
	@echo "Linux:   $(LINUX_TARGET)"
	@echo "Windows: $(WINDOWS_TARGET)"

# Clean build artifacts
.PHONY: clean
clean:
	@echo "==> Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)
	rm -f $(SRC_DIR)/*.luastatic.c
	cd $(DEPS_DIR)/luajit && $(MAKE) clean > /dev/null 2>&1 || true

# Clean everything including dependencies
.PHONY: distclean
distclean: clean
	@echo "==> Cleaning all generated files..."
	rm -rf $(DIST_DIR) $(DEPS_DIR)

# Test the built executable
.PHONY: test
test: linux
	@echo "==> Testing Luby transpiler..."
	@echo 'puts "Hello from Luby!"' | $(LINUX_TARGET) /dev/stdin

# Show help
.PHONY: help
help:
	@echo "Luby Transpiler Build System"
	@echo ""
	@echo "Targets:"
	@echo "  make deps      - Download all dependencies"
	@echo "  make linux     - Build for Linux (default)"
	@echo "  make windows   - Build for Windows (requires mingw-w64)"
	@echo "  make release   - Build for all platforms"
	@echo "  make test      - Test the Linux build"
	@echo "  make clean     - Remove build artifacts"
	@echo "  make distclean - Remove everything including dependencies"
	@echo "  make help      - Show this help message"