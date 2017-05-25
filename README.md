# Emacs dotfiles by @squiter

[![Build Status](https://travis-ci.org/squiter/emacs-dotfiles.svg?branch=master)](https://travis-ci.org/squiter/emacs-dotfiles)

This is my emacs setup with a lot of packages and customizations. This
repository was build for my personal use, If you wan't to try, do it
by yourself.

## Requirements

This repository needs a `init-secrets.el` file inside `emacs.d/conf/`
directory that will load some constants in the environment.

## Organization

The first file to see here is
the
[emacs](https://github.com/squiter/emacs-dotfiles/blob/master/emacs). In
this file we add the `conf` directory to our `'load-path` and require
the `'init-bootstrap`.  

Here we use the helper function `rr/safe-load-init-files` to load all
required files in our configuration. This function was writen
by [Milhouse](https://github.com/rranelli/emacs-dotfiles).

Let's list some important files:
- init-packages: All configurations to install a package and the
  package list itself;
- init-simple-packages: If the package needs only a `require` or just
  set one variable I add that configuration here, if the package needs
  more complex tipe of configuration I create a file to do that;
- init-keybindings: I try to keep all custom keybinds here;
- init-path: Fix some issues with `$PATH`;
