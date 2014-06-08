#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

# Set X settings
xrdb -load ~/.Xresources
xset +fp /usr/share/fonts/local

VBoxClient-all &

# Swap CAPSLOCK and CTRL
setxkbmap -option ctrl:swapcaps

# Bind keys
xbindkeys

# Set background
feh --bg-fill ~/Pictures/wallpaper

# Start compositing manager
compton -b

# Start terminal
urxvt &

# Start window manager
exec i3
