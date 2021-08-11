#!/bin/bash

#$SCREENSHOT
#betterlockscreen -u $PICTURE
#rm $PICTURE

PICTURE=/tmp/lock.png
scrot $PICTURE
betterlockscreen -u $PICTURE


if [ "$1" == "1" ]; then
    betterlockscreen -l dimblur
fi

if [ "$1" == "2" ]; then
    betterlockscreen -s dimblur
fi

