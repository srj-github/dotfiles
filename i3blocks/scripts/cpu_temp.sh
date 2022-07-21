#!/bin/bash

cpuTemp=$(sensors | grep Tctl)

echo ${cpuTemp: -8}
