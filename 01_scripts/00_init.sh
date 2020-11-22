#!/usr/bin/env bash
# ____
# |_  /_ _ __ _
#  / /| '_/ _` |
# /___|_| \__, |
#         |___/
# . main script file used to copy, link and install everything
# . . . . . . . . . . . . . . . . . . . . .
set -euo pipefail

# Clone dotfiles folder
git clone --recurse-submodules --remote-submodules https://github.com/srj-github/dotfiles ~/.dotfiles

# Install required packages
grep -v "^#" requirements.txt | sudo pacman -S --needed -

# Make symbolic links for each configuration file
# bash
ln -svf ~/.dotfiles/bash/bashrc ~/.bashrc
ln -svf ~/.dotfiles/bash/bash_aliases ~/.bash_aliases
ln -svf ~/.dotfiles/bash/inputrc ~/.inputrc

# alacritty
ln -svf ~/.dotfiles/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# qTile
ln -svf ~/.dotfiles/qtile/config.py ~/.config/qtile/config.py

# Emacs
ln -svf ~/.dotfiles/emacs/init.el ~/.config/emacs/init.el

# sddm
sudo systemctl enable sddm
sudo cp -r ~/.dotfiles/02_submodules/aerial-sddm-theme /usr/share/sddm/themes/
sudo mkdir /etc/sddm.conf.d
sudo ln -sf ~/.dotfiles/sddm/* /etc/sddm.conf.d/

# smartd
sudo ln -sy ~/.dotfiles/smartd/smartd.conf /etc/smartd.conf

# beets

ln -sy ~/.dotfiles/beets/config.yaml ~/.config/beets/config.yaml

# Copy required fonts
cp -r ~/.dotfiles/00_assets/fonts/* ~/.fonts

# Copy emacs assets
cp -r ~/.dotfiles/00_assets/themes ~/.config/emacs/
cp ~/.dotfiles/00_assets/signature ~/.config/emacs/


