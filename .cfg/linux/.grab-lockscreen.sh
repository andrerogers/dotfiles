#!/bin/bash

PICTURE=/tmp/lock.png
scrot $PICTURE
betterlockscreen -u $PICTURE
rm $PICTURE