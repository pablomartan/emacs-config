# My personal emacs config

## Intro

I am just starting to use emacs on feb 19 2025, as I need a better way
to organize and track my work. I am hoping org-mode will give me that,
and I think other emacs features will only make it easier (if I get to
sync corporate mail and JIRA with emacs).

This file will likely become and .org file in the future, and I may
even try to imitate [o-santi](https://github.com/o-santi/emacs)'s
config which reads the source blocks from it. We'll see.

## Nix

As nix is my package manager of choice, this emacs configuration is
tightly coupled with it. In its current initial, experimental state,
it is a flake that lets me run this config on any machine with nix
installed on it.