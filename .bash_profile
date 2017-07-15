PATH="/usr/local/bin:$ANDROID_HOME/tools:${PATH}"
export PATH

# We want to install Casks without root privileges.
export HOMEBREW_CASK_OPTS="--appdir=~/Applications"

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ll='ls -alF'

source ~/.git-prompt.sh
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$(__git_ps1)$ "

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1

source ~/git-completion.bash

eval "$(rbenv init -)"
alias xcode='sudo xcode-select -s /Applications/Xcode.app/Contents/Developer/ && xcode-select -p'
alias xcodeb='sudo xcode-select -s /Applications/Xcode-beta.app/Contents/Developer/ && xcode-select -p'
