#!/bin/bash

if [ `xrandr | grep HDMI-A-0 | grep -c ' connected '` -eq 1 ]; then
    xrandr --output eDP --auto --output HDMI-A-0 --auto --right-of eDP --primary
fi

