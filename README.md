Dotfiles
========

This repository contains some configuration files for typical Unix tools. If you want them, follow these instructions:

```sh
cd $HOME
git clone --recursive https://github.com/danielmartin/dotfiles.git
mv .emacs.d .emacs.d~
mv .bash_profile .bash_profile~
ln -s dotfiles/.emacs.d .
ln -s dotfiles/.bash_profile .
ln -s dotfiles/.git-prompt.sh .
```

This will create a `dotfiles` directory inside your home directory and a corresponding symbolic link for each configuration file/folder. The `--recursive` option will automatically download every submodule in the repository.
