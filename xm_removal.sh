#!/bin/bash

# Searching for the files
FILES=$(sudo find / -name "xm" 2>/dev/null)

# If files are found, remove them
if [ ! -z "$FILES" ]; then
echo "Files found: $FILES"
echo "Removing files..."
sudo rm -rf $FILES
echo "Files removed."
else
    echo "No files found."
fi

# Kill any 'xm' processes
PROCESSES=$(pgrep xm)
if [ ! -z "$PROCESSES" ]; then
echo "Processes found: $PROCESSES"
echo "Killing processes..."
sudo kill -9 $PROCESSES
echo "Processes killed."
else
    echo "No 'xm' processes found."
fi
