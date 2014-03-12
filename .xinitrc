#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Load VirtualBox guest addons
#VBoxClient-all &

# Set the background
feh --bg-scale /home/stephen/Pictures/wallpaper.jpg

# Start compton daemonized
compton -b

# Now here we go
exec xmonad
