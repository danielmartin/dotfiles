Dotfiles
========

This repository contains some configuration files for typical Unix tools. If you want them, follow these instructions:

Clone this repository on your $HOME directory (typically /home/username or /Users/username).

```sh
cd $HOME
git clone https://github.com/danielmartin/dotfiles.git
```

Change directory to `dotfiles` and run this command:

```sh
make install
```

This will create the necessary symbolic links so that everything works correctly.

If you deem that you don't want this package on your system, change directory to `dotfiles` and run this command:

```sh
make clean
```

Then simply delete the `dotfiles` directory from your home folder.
