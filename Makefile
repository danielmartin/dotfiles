install:
	-mv ~/.emacs.d ~/.emacs.d~
	-mv ~/.bash_profile ~/.bash_profile~
	-mv ~/.git-prompt.sh ~/.git-prompt.sh~
	ln -s dotfiles/.emacs.d ~
	ln -s dotfiles/.bash_profile ~
	ln -s dotfiles/.git-prompt.sh ~
clean:
	rm ~/.emacs.d
	rm ~/.bash_profile
	rm ~/.git-prompt.sh
	-mv ~/.emacs.d~ ~/.emacs.d
	-mv ~/.bash_profile~ ~/.bash_profile
	-mv ~/.git-prompt.sh~ ~/.git-prompt.sh
