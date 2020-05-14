#!/bin/sh
acme -v pm500.b
diff -s pacman.rom original/pm.rom
cmp pacman.rom original/pm.rom