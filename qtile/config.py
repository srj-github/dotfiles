#  ____
# |_  /_ _ __ _
#  / /| '_/ _` |
# /___|_| \__, |
#         |___/
#  qTile configuration file
# . . . . . . . . . . . . . . . . . . . . .

import subprocess
import re

from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = guess_terminal()

keys = [
        # Switch between windows in current stack pane
    Key([mod], "k", lazy.layout.down(),
        desc="Move focus down in stack pane"),
        Key([mod], "j", lazy.layout.up(),
            desc="Move focus up in stack pane"),

    # Move windows up or down in current stack
    Key([mod, "control"], "k", lazy.layout.shuffle_down(),
        desc="Move window down in current stack "),
        Key([mod, "control"], "j", lazy.layout.shuffle_up(),
            desc="Move window up in current stack "),

    # Switch window focus to other pane(s) of stack
    Key([mod], "Tab", lazy.layout.down(),
        desc="Switch window focus to other pane(s) of stack"),

        Key([mod], "t", lazy.window.toggle_floating(),
            desc="Switch window to and from floating"),

    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.rotate(),
        desc="Swap panes of split stack"),

    # Toggle between split and unsplit sides of stack.
        # Split = all windows displayed
        # Unsplit = 1 window displayed, like Max layout, but still with
        # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
        Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),

    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),
        Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "control"], "r", lazy.restart(), desc="Restart qtile"),
        Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown qtile"),
        Key([mod], "r", lazy.spawncmd(),
            desc="Spawn a command using a prompt widget"),

]

groups = [
        Group(name="1", layout="stack", label="main(1)"),
        Group(name="2", spawn="emacs", layout="monadwide", label="code(2)", init=True),
        Group(name="3", spawn="alacritty", layout="verticaltile", label="terminals(3)", init=True),
        Group(name="m", spawn="thunderbird", label="mail(m)", init=True),
        Group(name="0", spawn="qbittorrent", label="torrent(0)", init=True)
]

for i in groups:
        keys.append(
                Key([mod, 'shift'], i.name, lazy.window.togroup(i.name))
        )
        keys.append(
                Key([mod], i.name, lazy.group[i.name].toscreen())
        )

layouts = [
    # layout.Max(),
    layout.Stack(num_stacks=1, margin=5, border_width=0),
    # Try more layouts by unleashing below layouts.
    # layout.Bsp(),
    # layout.Columns(),
    # layout.Matrix(),
    layout.MonadTall(margin=5),
    layout.MonadWide(margin = 5),
    # layout.RatioTile(),
    # layout.Tile(),
    #layout.TreeTab(),
    layout.VerticalTile(margin = 5),
    # layout.Zoomy(),
]


def wallpaper():
        path = "/home/zrg/Documents/wallpapers"
        #subprocess.run(f'feh --bg-fill --randomize {path}', shell=True)
        subprocess.run(['feh', '--bg-fill', '--randomize', path])

def getCPUTemp():
        sensors = subprocess.run('sensors', capture_output=True)
        currentTemp = re.search(r'(?<=Packageid0:\+)\d\d.\d.', str(sensors).replace(" ", "")).group(0)

        return currentTemp

widget_defaults = dict(
    font='Prime',
    fontsize=15,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
        Screen(
                top=bar.Bar(
                        [
                                widget.CurrentLayout(foreground="#dde7c7"),
                                widget.GroupBox(
                                        active="#80b918",
                                        block_highlight_text_color="#eeef20",
                                        borderwidth=0,
                                        disable_drag=True
                                        ),
                                widget.Prompt(),
                                widget.Pomodoro(),
                                widget.Spacer(),
                                widget.Chord(
                                        chords_colors={
                                                'launch': ("#ff0000", "#ffffff"),
                                        },
                                        name_transform=lambda name: name.upper(),
                                ),
                                widget.CPU(
                                        format=("' {freq_current}GHz {load_percent}%'"),
                                        foreground="#00b2ca"
                                        ),
                                widget.Sep(),
                                widget.Memory(
                                        foreground="#da2c38",
                                        format=" {MemUsed}M  {SwapUsed}M"
                                        ),
                                widget.Sep(),
                                widget.DF(
                                        visible_on_warn=False,
                                        format=" {p} {uf}{m} FREE",
                                        foreground='#0EAD69'
                                        ),
                                widget.Sep(),
                                widget.TextBox(
                                        font="fontawesome",
                                        text="",
                                        foreground="#ee4266"
                                        ),
                                widget.GenPollText(
                                        func=getCPUTemp,
                                        update_interval=5,
                                        foreground="#ee4266"
                                        ),
                                widget.Sep(),
                                widget.Battery(
                                        full_char="",
                                        empty_char="",
                                        charge_char=" ",
                                        discharge_char=" ",
                                        format='{char} {percent:2.0%} {hour:d}:{min:02d}',
                                        show_short_text=False,
                                        foreground="#ffd23f"
                                        ),
                                widget.Sep(),
                                widget.TextBox(
                                        font="fontawesome",
                                        text="",
                                        foreground="#3bceac"
                                        ),
                                widget.Volume(
                                        foreground="#3bceac"
                                        )
                        ],
                        25,
                ),
                bottom=bar.Bar(
                        [
                                widget.TaskList(),
                                widget.Net(
                                        format=' {down}  {up}',
                                        foreground="#87c38f"
                                        ),
                                widget.Sep(),
                                widget.Wlan(
                                        interface="wlp3s0",
                                        format=" {essid} {percent:2.0%}",
                                        foreground="#1c7293"
                                        ),
                                widget.Sep(),
                                widget.CheckUpdates(
                                        colour_have_updates="#7dcfb6",
                                        colour_no_updates="#fbd1a2"
                                        ),
                                widget.Sep(),
                                widget.Clock(
                                        format=" %H:%M   %a %d-%m-%y",
                                        foreground="#f79256"
                                ),
                                widget.Systray()
                        ],
                        25,
                )
        ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

# Hooks

@hook.subscribe.startup
def autostart():
    wallpaper()
