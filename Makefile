install:
	-mv ~/.emacs.d ~/.emacs.d~
	-mv ~/.bash_profile ~/.bash_profile~
	-mv ~/.git-prompt.sh ~/.git-prompt.sh~
	-mv ~/git-completion.bash ~/git-completion.bash~
	ln -s dotfiles/.emacs.d ~
	ln -s dotfiles/.bash_profile ~
	ln -s dotfiles/.git-prompt.sh ~
	ln -s dotfiles/git-completion.bash ~
clean:
	rm ~/.emacs.d
	rm ~/.bash_profile
	rm ~/.git-prompt.sh
	rm ~/git-completion.bash
	-mv ~/.emacs.d~ ~/.emacs.d
	-mv ~/.bash_profile~ ~/.bash_profile
	-mv ~/.git-prompt.sh~ ~/.git-prompt.sh
	-mv ~/git-completion.bash~ ~/git-completion.bash
