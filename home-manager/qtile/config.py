import os
import re
import socket
import subprocess
from libqtile import qtile
from libqtile.config import Click, Drag, Group, KeyChord, Key, Match, Screen, EzKey
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from typing import List  # noqa: F401
from libqtile.widget.backlight import ChangeDirection

import hooks
import widgets
from utils import *

# from qtile_extras.widget.decorations import BorderDecoration

mod = "mod4"  # Sets mod key to SUPER/WINDOWS
mod1 = "mod1"

launcher = 'rofi -show combi -combi-modi "drun,run"'
font = "RobotoMono Nerd Font"

import subprocess
import os
from libqtile import hook
from bin.lock_screen import lock_screen


@lazy.function
def toggle_kbd_layout(qtile):
    ENGLISH = "us"
    PERSIAN = "IR"

    kbd_query = subprocess.run(
        ["setxkbmap", "-query"], stdout=subprocess.PIPE
    ).stdout.decode()
    current_layout = re.findall("layout:\s+(.+)", kbd_query)

    if ENGLISH in current_layout:
        subprocess.run(["setxkbmap", "ir"])
    else:
        subprocess.run(["setxkbmap", "us"])


@lazy.function
def lazy_lock_screen(qtile):
    lock_screen()

colors = [
    ["#000000", "#000000"],
    ["#595959", "#595959"],
    ["#ffffff", "#ffffff"],
    ["#feacd0", "#feacd0"],
    ["#44bc44", "#44bc44"],
    ["#ef8b50", "#ef8b50"],
    ["#2fafff", "#2fafff"],
    ["#b6a0ff", "#b6a0ff"],
    ["#6ae4b9", "#6ae4b9"],
    ["#79a8ff", "#79a8ff"],
]


class Colors:
    background = ["#000000", "#000000"]
    inactive = ["#595959", "#595959"]
    foreground = (["#ffffff", "#ffffff"],)
    # ["#feacd0", "#feacd0"],
    # ["#44bc44", "#44bc44"],
    alert = (["#ef8b50", "#ef8b50"],)
    active = ["#2fafff", "#2fafff"]
    # ["#b6a0ff", "#b6a0ff"],
    # ["#6ae4b9", "#6ae4b9"],
    # ["#79a8ff", "#79a8ff"],


keys = [
    ### The essentials
    Key([mod], "Return", lazy.spawn("kitty"), desc="Launch kitty terminal"),
    Key([mod], "d", lazy.spawn(launcher), desc="Run Launcher"),
    Key([mod], "q", lazy.window.kill(), desc="Kill active window"),
    Key([mod], "c", lazy.reload_config(), desc="Reload Qtile config"),
    Key([mod, "shift"], "c", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "shift"], "e", lazy.shutdown(), desc="Shutdown Qtile"),
    # BSP bindings
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "v", lazy.layout.toggle_split()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "shift"], "n", lazy.layout.normalize()),
    # Window management
    Key([mod], "f", lazy.window.toggle_floating()),
    Key([mod], "m", lazy.window.toggle_fullscreen()),
    Key([mod], "n", lazy.window.toggle_minimize()),
    Key([mod1], "Tab", lazy.spawn("rofi -show window")),
    Key([mod], "x", lazy_lock_screen()),
    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout()),
    Key([mod, "shift"], "space", lazy.prev_layout()),
    Key([], "CAPS_LOCK", toggle_kbd_layout()),
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.widget["speakers_volume"].raise_volume(),
        desc="Increase speakers volume",
    ),
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.widget["speakers_volume"].lower_volume(),
        desc="Decrease speakers volume",
    ),
    Key(
        [],
        "XF86AudioMute",
        lazy.widget["speakers_volume"].toggle_mute_volume(),
        desc="Toggle mute speakers volume",
    ),
    Key(
        [],
        "XF86AudioMicMute",
        lazy.widget["microphone_volume"].toggle_mute_volume(),
        desc="Toggle mute microphone volume",
    ),
    Key(
        [],
        "XF86MonBrightnessUp",
        lazy.widget["backlight"].change_backlight(ChangeDirection.UP),
        desc="Increase screen brightness",
    ),
    Key(
        [],
        "XF86MonBrightnessDown",
        lazy.widget["backlight"].change_backlight(ChangeDirection.DOWN),
        desc="Decrease screen brightness",
    ),
]

groups = [
    Group("1", layout="bsp"),
    Group("2", layout="bsp"),
    Group("3", layout="bsp"),
    Group("4", layout="bsp"),
    Group("5", layout="bsp"),
    Group("6", layout="bsp"),
    Group("7", layout="bsp"),
    Group("8", layout="bsp"),
    Group("9", layout="bsp"),
    Group("0", layout="max"),
]

group_screen = {
    "1": 0,
    "2": 0,
    "3": 0,
    "4": 2,
    "5": 2,
    "6": 2,
    "7": 1,
    "8": 1,
    "9": 1,
}

if len(qtile.screens) == 1:
    group_screen = {k: 0 for k in group_screen.keys()}

groups = [Group(i) for i in group_screen.keys()]

for group in groups:
    screen = group_screen[group.name]
    keys.append(Key([mod], group.name, lazy.group[group.name].toscreen(screen), lazy.to_screen(screen)))
    keys.append(Key([mod, "shift"], group.name, lazy.window.togroup(group.name)))


# Allow MODKEY+[0 through 9] to bind to groups, see https://docs.qtile.org/en/stable/manual/config/groups.html
# MOD4 + index Number : Switch to Group[index]
# MOD4 + shift + index Number : Send active window to another Group
# from libqtile.dgroups import simple_key_binder

# dgroups_key_binder = simple_key_binder("mod4")

layout_theme = {
    "border_width": 1,
    "margin": 5,
    "margin_on_single": 0,
    "border_on_single": False,
    "border_focus": "79a8ff",
    "border_normal": "1D2330",
}

layouts = [
    layout.Bsp(**layout_theme),
    layout.Max(**layout_theme),
]


prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())


##### DEFAULT WIDGET SETTINGS #####
widget_defaults = dict(
    font="RobotoMono Nerd Font", fontsize=14, padding=5, background=colors[2]
)
extension_defaults = widget_defaults.copy()


def init_widgets_list():
    default_args = dict(
        font=font,
        fontsize=14,
    )
    sep = widget.Sep(background=Colors.background, foreground=Colors.inactive)

    widgets_list = [
        widget.GroupBox(
            block_highlight_text_color=Colors.background,
            hide_unused=False,
            highlight_method="block",
            rounded=False,
            this_current_screen_border=Colors.active,
            background=Colors.background,
            foreground=Colors.foreground,
            urgent_border=Colors.alert,
            **default_args,
        ),
        sep,
        widget.CurrentLayoutIcon(
            custom_icon_paths=[os.path.expanduser("~/.config/qtile/icons")],
            padding=0,
            scale=0.7,
            background=Colors.background,
            foreground=Colors.foreground,
            **default_args,
        ),
        widget.CurrentLayout(background=Colors.background, **default_args),
        sep,
        widget.Spacer(background=Colors.background),
        widget.Pomodoro(
            background=colors[0],
            color_active=colors[4],
            color_break=colors[6],
            color_inactive=colors[5],
            prefix_inactive="🍅",
            prefix_break="🎉 ",
            prefix_long_break="🥳 ",
            prefix_paused="🥝"
        ),
        sep,
        widget.CPUGraph(background=Colors.background),
        widget.Memory(
            background=Colors.background,
            # TODO make me float
            mouse_callbacks={"Button1": lambda: qtile.cmd_spawn("kitty -e htop")},
            fmt=" {}",
            **default_args,
        ),
        sep,
        widget.KeyboardLayout(
            background=Colors.background,
            fmt=" {}",
            **default_args,
        ),
        sep,
        widgets.Volume(
            name="speakers_volume",
            background=Colors.background,
            get_volume_shell_cmd=GET_SPEAKERS_VOLUME_SHELL_CMD,
            raise_volume_shell_cmd=RAISE_SPEAKERS_VOLUME_SHELL_CMD,
            lower_volume_shell_cmd=LOWER_SPEAKERS_VOLUME_SHELL_CMD,
            get_muted_status_shell_cmd=ARE_SPEAKERS_MUTED_SHELL_CMD,
            toggle_mute_shell_cmd=TOGGLE_SPEAKERS_MUTE_SHELL_CMD,
            icons={"muted": "婢", "low": "奄", "medium": "奔", "high": "墳"},
            update_interval=1,
        ),
        widgets.Volume(
            name="microphone_volume",
            background=Colors.background,
            get_volume_shell_cmd=GET_MICROPHONE_VOLUME_SHELL_CMD,
            raise_volume_shell_cmd=RAISE_MICROPHONE_VOLUME_SHELL_CMD,
            lower_volume_shell_cmd=LOWER_MICROPHONE_VOLUME_SHELL_CMD,
            get_muted_status_shell_cmd=IS_MICROPHONE_MUTED_SHELL_CMD,
            toggle_mute_shell_cmd=TOGGLE_MICROPHONE_MUTE_SHELL_CMD,
            icons={"muted": "", "low": "", "medium": "", "high": ""},
            update_interval=1,
        ),
        sep,
        widget.ThermalSensor(background=colors[0], fmt=" {}"),
        widgets.Battery(
            update_interval=5, background=Colors.background, **default_args
        ),
        widget.Backlight(
            background=Colors.background,
            backlight_name="intel_backlight",  # Directory name in /sys/class/backlight which provides backlight control interface
            change_command="brightnessctl set {}%",
            format=" {percent:2.0%}",
        ),
        sep,
        widget.Systray(background=colors[0], padding=5),
        sep,
        widget.Clock(
            foreground=colors[0],
            background=colors[6],
            format="%d %b %Y %H:%M",
            # decorations=[
            #     BorderDecoration(
            #         colour=colors[6],
            #         border_width=[0, 0, 2, 0],
            #         padding_x=5,
            #         padding_y=None,
            #     )
            # ],
        ),
    ]
    return widgets_list


def init_screens():
    background = "~/.config/qtile/backgrounds/mountain.jpg"
    wallpaper_mode = "stretch"
    left = Screen(
            wallpaper=background,
            wallpaper_mode=wallpaper_mode,
            # top=bar.Bar(widgets=init_widgets_list(), opacity=1.0, size=25),
        )
    right = Screen(
            wallpaper=background,
            wallpaper_mode=wallpaper_mode,
            # top=bar.Bar(widgets=screen_2_widgets, opacity=1.0, size=25),
        )
    middle = Screen(
            wallpaper=background,
            wallpaper_mode=wallpaper_mode,
            top=bar.Bar(widgets=init_widgets_list(), opacity=1.0, size=25),
        )
    if len(qtile.screens) == 1:
        return [middle]
    else:
        return [left, right, middle]


def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)


def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)


def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)


def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)


def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)


mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()

# FIXME indentation and organization
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        # default_float_rules include: utility, notification, toolbar, splash, dialog,
        # file_progress, confirm, download and error.
         *layout.Floating.default_float_rules,
        Match(title="Confirmation"),  # tastyworks exit box
        Match(title="Qalculate!"),  # qalculate-gtk
        Match(wm_class="kdenlive"),  # kdenlive
        Match(wm_class="pinentry-gtk-2"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
