#!/bin/sh
make
echo "Starting anna...."
./glennbot Anna irc.hwcommunity.com '#bottest' $@
