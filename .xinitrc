#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

# Set X settings
xrdb -load ~/.Xresources
xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/misc
xset fp rehash
xset r rate 200 30

# Sound server
start-pulseaudio-x11

# Input module settings
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"

ibus-daemon -drx

VBoxClient-all &

# Swap CAPSLOCK and CTRL
setxkbmap -option ctrl:swapcaps

# Bind keys
#xbindkeys

# Set background
feh --bg-fill ~/Pictures/wallpaper

# Start compositing manager
compton -b

# Start terminal
#urxvt &

xbindkeys

# Start window manager
sxhkd &
exec xmonad
