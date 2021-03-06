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
grep -v "^#" requirements.txt | sudo pacman -S --needed -

# Clone dotfiles folder
git clone --recurse-submodules --remote-submodules https://github.com/srj-github/dotfiles ~/.dotfiles

# Make symbolic links for each configuration file
# bash
ln -svf ~/.dotfiles/bash/bashrc ~/.bashrc
ln -svf ~/.dotfiles/bash/bash_aliases ~/.bash_aliases
ln -svf ~/.dotfiles/bash/inputrc ~/.inputrc

# alacritty
mkdir -p ~/.config/alacritty
ln -svf ~/.dotfiles/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# qTile
mkdir -p ~/.config/qtile
ln -svf ~/.dotfiles/qtile/config.py ~/.config/qtile/config.py

# Emacs
mkdir -p ~/.config/emacs
ln -svf ~/.dotfiles/emacs/init.el ~/.config/emacs/init.el

# smartd
sudo ln -svf ~/.dotfiles/smartd/smartd.conf /etc/smartd.conf

# beets
mkdir -p ~/.config/beets
ln -svf ~/.dotfiles/beets/config.yaml ~/.config/beets/config.yaml

# git
ln -svf ~/.dotfiles/git/gitconfig ~/.gitconfig
ln -svf ~/.dotfiles/git/gitignore ~/.gitignore

# Copy required fonts
mkdir -p ~/.fonts
cp -r ~/.dotfiles/00_assets/fonts/* ~/.fonts

# Copy emacs assets
mkdir -p ~/.config/emacs
touch ~/.config/emacs/custom.el
cp -r ~/.dotfiles/00_assets/themes ~/.config/emacs/
cp ~/.dotfiles/00_assets/signature ~/.config/emacs/
