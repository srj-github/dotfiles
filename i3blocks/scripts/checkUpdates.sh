#!/bin/bash

command=$(pacman -Qu | wc -l)

echo $command
