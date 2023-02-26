#!/bin/sh

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

exec ssh-agent dbus-launch i3
