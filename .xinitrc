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
xset +fp ~/.fonts
xset fp rehash
xset r rate 200 30

#source /home/stephen/bin/gencolorvals

# Sound server
start-pulseaudio-x11

# Input module settings
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"

ibus-daemon -drx

VBoxClient-all &
vmware-user &

# Capslock as additional Ctrl
setxkbmap -option ctrl:nocaps

# Set background
feh --bg-fill ~/Pictures/wallpaper

# Start compositing manager
compton -b

# Start terminal
#urxvt & 

# Start keymapper
xbindkeys

# Start window manager
exec i3
