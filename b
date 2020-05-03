#!/bin/sh
acme -v pm500.b
diff -s pm500.rom original/pm.rom
cmp pm500.rom original/pm.rom