#!/bin/sh

# Register with gnome-session so that it does not kill the whole session thinking it is dead.
test -n "$DESKTOP_AUTOSTART_ID" && {
    dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.RegisterClient "string:exwm-gnome" "string:$DESKTOP_AUTOSTART_ID"
}

# Support for merging .Xresources
test -e $HOME/.Xresources && {
    xrdb -merge $HOME/.Xresources
}

picom &
sudo tlp start &
sudo thermald &
kde-indicator &
feh --bg-fill ~/Downloads/01151_inthedeep_2560x1600.jpg &
lxsession &
lxqt-panel &
flameshot &
emacs --fullscreen --eval "(exwm-enable)"

# Logout process.
test -n "$DESKTOP_AUTOSTART_ID" && {
	dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.Logout "uint32:1"
}
