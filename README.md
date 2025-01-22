# Emacs dotfiles by @squiter

This is my emacs setup with a lot of packages and customizations. This
repository was build for my personal use, If you wan't to try, do it
by yourself.

## Requirements

It uses the `stow` to make the installation in you machine.

## Installation

Enter in the `emacs-dotfiles` directory and run: `stow --target=$HOME dotfiles/`

## Organization

The first file to see here is the
[emacs](https://github.com/squiter/emacs-dotfiles/blob/master/emacs). In
this file we add the `conf` directory to our `'load-path` and require
the `'init-bootstrap`.
