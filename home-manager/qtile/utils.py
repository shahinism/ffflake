AUTOSTART_APPS = [
    # zoom-in (required for the framework resolution)
    # "xrandr --output eDP-1 --scale 0.8",  # TODO check if it's wayland, run `kanshi` instead.
    # network manager applet
    "nm-applet",
    # bluetooth applet
    "blueman-applet",
    # run daemon to auto-mount USB disks
    "udiskie --no-automount --no-notify --tray",
    # trigger session lock after 5 minutes of inactivity,
    # turn display off 2 minutes later
    'xset s 300; xss-lock -- /home/shahin/.config/qtile/bin/lock_screen.py',
    # run notification daemon
    'dunst',
    # The awesome screenshot applet
    'flameshot',
    # Encrypt Everything!
    'KEYBASE_AUTOSTART=1 keybase-gui',
    # Run Espanso
    'espanso service start --unmanaged',
    # Apply screen definition
    "autorandr --change"
]

GET_SPEAKERS_VOLUME_SHELL_CMD = '''
    pactl get-sink-volume @DEFAULT_SINK@ \
    | grep -i Volume \
    | awk '{print $5}' \
    | sed 's/%//'
'''

RAISE_SPEAKERS_VOLUME_SHELL_CMD = '''
    pactl set-sink-mute @DEFAULT_SINK@ 0 && \
    pactl set-sink-volume @DEFAULT_SINK@ +5%
'''

LOWER_SPEAKERS_VOLUME_SHELL_CMD = 'pactl set-sink-volume @DEFAULT_SINK@ -5%'
RAISE_MICROPHONE_VOLUME_SHELL_CMD = '''
    pactl set-source-mute @DEFAULT_SOURCE@ 0 && \
    pactl set-source-volume @DEFAULT_SOURCE@ +5%
'''
LOWER_MICROPHONE_VOLUME_SHELL_CMD = 'pactl set-source-volume @DEFAULT_SOURCE@ -5%'

ARE_SPEAKERS_MUTED_SHELL_CMD = '''
    pactl get-sink-mute @DEFAULT_SINK@ \
        | grep -q 'no' \
        && echo 0 \
        || echo 1
'''
TOGGLE_SPEAKERS_MUTE_SHELL_CMD = 'pactl set-sink-mute @DEFAULT_SINK@ toggle'

GET_MICROPHONE_VOLUME_SHELL_CMD = '''
    pactl get-source-volume @DEFAULT_SOURCE@ \
    | grep -i Volume \
    | awk '{print $5}' \
    | sed 's/%//'
'''
IS_MICROPHONE_MUTED_SHELL_CMD = '''
    pactl get-source-mute @DEFAULT_SOURCE@ \
        | grep -q 'no' \
        && echo 0 \
        || echo 1
'''
TOGGLE_MICROPHONE_MUTE_SHELL_CMD = '''
    pactl set-source-mute @DEFAULT_SOURCE@ toggle
'''

