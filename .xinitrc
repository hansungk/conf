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

# Load VirtualBox guest addons
VBoxClient-all &

# Start terminal
urxvt &

xrdb -load ~/.Xresources

xbindkeys

# Start compton daemonized
#compton -b

2bwm &

exec urxvt
