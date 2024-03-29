#!/bin/bash                                                                                                                                                                                                                                                    
# This script is intended to make switching between laptop and external displays easier when using i3+dmenu
# To run this script, map it to some shortcut in your i3 config, e.g:
# bindsym $mod+p exec --no-startup-id $config/display.sh
# IMPORTANT: run chmod +x on the script to make it executable
# The result is 4 options appearing in dmenu, from which you can choose
# This is your default laptop screen, detect by running `xrandr`

INTERNAL_OUTPUT="eDP"

# choices will be displayed in dmenu
choices="laptop\ndual-right\ndual-left\nexternal\nclone"
                                      
# Your choice in dmenu will determine what xrandr command to run
chosen=$(echo -e $choices | dmenu -i)

# This is used to determine which external display you have connected
# This may vary between OS. e.g VGA1 instead of VGA-1
if [ `xrandr | grep HDMI-A-0 | grep -c ' connected '` -eq 1 ]; then
        EXTERNAL_OUTPUT="HDMI-A-0"
fi

# xrander will run and turn on the display you want, if you have an option for 3 displays, this will need some modifications
case "$chosen" in
    external) xrandr --output $INTERNAL_OUTPUT --off --output $EXTERNAL_OUTPUT --auto --primary ;;
    laptop) xrandr --output $INTERNAL_OUTPUT --auto --primary --output $EXTERNAL_OUTPUT --off ;;
    clone) xrandr --output $INTERNAL_OUTPUT --auto --output $EXTERNAL_OUTPUT --auto --same-as $INTERNAL_OUTPUT ;;
    dual-right) xrandr --output $INTERNAL_OUTPUT --auto --output $EXTERNAL_OUTPUT --auto --left-of $INTERNAL_OUTPUT --primary ;;
    dual-left) xrandr --output $INTERNAL_OUTPUT --auto --output $EXTERNAL_OUTPUT --auto --right-of $INTERNAL_OUTPUT --primary ;;
esac
