#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

# Set X settings
xrdb -load ~/.Xresources

# Start VMware tools (for autoresizing window)
vmware-user &

# Swap CAPSLOCK and CTRL
setxkbmap -option ctrl:swapcaps

# Set background
feh --bg-fill ~/Pictures/wallpaper

# Start terminal
urxvt &

# Start window manager
sxhkd &
exec bspwm
