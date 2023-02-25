#!/bin/bash

cpuTemp=$(sensors | grep "Package id 0:" | cut -c 16- | rev | cut -c 8- | rev | cut -d' ' -f1)

color="#008000"

if [ ${cpuTemp//[!0-9]/} -gt 700 ]; then
    color="#ff0000"
elif [ ${cpuTemp//[!0-9]/} -gt 500 ]; then
    color="#ffa500"
fi
  
echo $cpuTemp
echo
echo $color
