#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

# Set X settings
xrdb -load ~/.Xresources
xset +fp /usr/share/fonts/local
xset r rate 200 20

# FCITX settings
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

VBoxClient-all &

# Swap CAPSLOCK and CTRL
setxkbmap -option ctrl:swapcaps

# Bind keys
xbindkeys

# Set background
feh --bg-fill ~/Pictures/wallpaper

# Start compositing manager
#compton -b

# Start input method
fcitx &

# Start terminal
sxhkd &
bspwm &

# Start window manager
exec urxvt
