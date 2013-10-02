Dotfiles
========

This repository contains some configuration files for Emacs. In the future, I'll add configuration files for other Unix tools that I use. If you want them, follow these instructions:

```sh
cd $HOME
git clone https://github.com/danielmartin/dotfiles.git
mv .emacs.d .emacs.d~
ln -s dotfiles/.emacs.d .
```

This will create a `dotfiles` directory inside your home directory and a corresponding symbolic link for each configuration file/folder.
