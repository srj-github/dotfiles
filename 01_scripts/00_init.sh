#!/usr/bin/env bash
# ____
# |_  /_ _ __ _
#  / /| '_/ _` |
# /___|_| \__, |
#         |___/
# . main script file used to copy, link and install everything
# . . . . . . . . . . . . . . . . . . . . .
set -euo pipefail

# Install required packages
grep -v "^#" requirements.txt  | sudo pacman -S --needed -

# Install Doom Emacs
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

# Make symbolic links for each configuration file
# bash
ln -svf ~/.dotfiles/bash/bashrc ~/.bashrc
ln -svf ~/.dotfiles/bash/bash_aliases ~/.bash_aliases
ln -svf ~/.dotfiles/bash/inputrc ~/.inputrc

# alacritty
ln -svf ~/.dotfiles/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# qTile
ln -svf ~/.dotfiles/qtile/config.py ~/.config/qtile/config.py

# Doom Emacs
ln -svf ~/.dotfiles/doom/* ~/.doom.d/

# Copy required fonts
cp -r ~/.dotfiles/00_assets/fonts/* ~/.fonts
