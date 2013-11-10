#!/bin/bash

if [ ! -f "$1" ] ; then
    echo "Enter MIDI filename."
    exit
fi

timidity "$1" -Ov -o "${1/%.mid/.ogg}"
ffmpeg -y -i "${1/%.mid/.ogg}" "${1/%.mid/.mp3}"
