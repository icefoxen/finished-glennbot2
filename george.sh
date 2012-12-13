#!/bin/sh
# strace is my friend.
make
echo "Starting george...."
./glennbot NeoGeorge irc.hwcommunity.com '#bottest'
