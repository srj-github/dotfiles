#!/bin/bash

cpuTemp=$(sensors | grep Tctl | cut -c 16- | rev | cut -c 8- | rev)

color="#008000"

if [ $cpuTemp -gt 70 ]; then
    color="#ff0000"
elif [ $cpuTemp -gt 50 ]; then
    color="#ffa500"
fi
  
echo $cpuTemp
echo
echo $color
