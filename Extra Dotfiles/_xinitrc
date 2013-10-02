# the following variable defines the session which is started if the user
# doesn't explicitely select a session
DEFAULT_SESSION=startkde

case $1 in
xmonad)
	exec xmonad
	;;
xfce4)
	exec startxfce4
	;;
KDE)
	exec startkde
	;;
GNOME)
	exec gnome-session
	;;
Cinnamon)
	exec gnome-session-cinnamon
	;;
MATE)
	exec mate-session
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
