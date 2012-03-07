#!/bin/sh

NAME=edbi
VER=0.1.0

FULLNAME=$NAME-$VER

mkdir -p $FULLNAME
cp edbi.el edbi-bridge.pl edbi-pkg.el $FULLNAME
tar cvf $FULLNAME.tar $FULLNAME/
