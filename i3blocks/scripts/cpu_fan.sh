#!/bin/bash

fanSpeed=$(sensors | grep cpu_fan)

echo ${fanSpeed: -8}
