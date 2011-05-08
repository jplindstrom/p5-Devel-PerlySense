#!/bin/sh
perl Build.PL
perl Build install
perl Build clean
perl Build realclean
