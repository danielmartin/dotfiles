PATH="/usr/local/bin:$ANDROID_HOME/tools:${PATH}"
export PATH

OCLINT_HOME=~/oclint
export PATH=$OCLINT_HOME/bin:$PATH

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ll='ls -alF'

source ~/.git-prompt.sh
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$(__git_ps1)$ "

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1

source ~/git-completion.bash
