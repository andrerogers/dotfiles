#!/bin/bash

#PICTURE=/tmp/lock.png
#SCREENSHOT="scrot $PICTURE"
#$SCREENSHOT
#betterlockscreen -u $PICTURE
#rm $PICTURE

CMD=betterlockscreen

if test -f "$LOCK_PICTURE"; then
    CMD=betterlockscreen -u $LOCK_PICTURE
fi


if [ "$1" == "1" ]; then
	betterlockscreen -l dimblur
fi

if [ "$1" == "2" ]; then
	betterlockscreen -s dimblur
fi

