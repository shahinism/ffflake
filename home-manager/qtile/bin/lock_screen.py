#!/usr/bin/env python3

import subprocess


def lock_screen():
    subprocess.run(["setxkbmap", "us"])
    subprocess.run(["betterlockscreen", "-l", "--off", "120"])


if __name__ == "__main__":
    lock_screen()
