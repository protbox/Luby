# Luby

## Installation
```bash
curl -fsSL https://raw.githubusercontent.com/protbox/Luby/refs/heads/main/install.sh | sudo bash
```

## Usage
```bash
luby source.rb > output.lua
luby source_directory/ (will recusively transpile all *.rb files into the current directory)
```

## Uninstall
I'd prefer you didn't, but if you really have to:

```bash
sudo rm -rf /usr/local/luby /usr/local/bin/luby
```