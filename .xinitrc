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

# Input module settings
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"

#VBoxClient-all &

# Swap CAPSLOCK and CTRL
setxkbmap -option ctrl:swapcaps

# Bind keys
#xbindkeys

# Set background
feh --bg-fill ~/Pictures/wallpaper

# Start compositing manager
compton -b

# Start input method
fcitx &

# Start terminal
urxvt &

xrdb -load ~/.Xresources

xbindkeys

# Start window manager
sxhkd &
urxvt &
exec bspwm
