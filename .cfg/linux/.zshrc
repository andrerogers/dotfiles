export LANG=en_US.UTF-8
export TERM=xterm-256color
export ZSH=$HOME/.oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export GOPATH=$HOME/go

export PATH=$HOME/.local/bin:$HOME/bin:/usr/local/bin:$HOME/.anaconda3/bin:$GOPATH/bin:/usr/bin:$PATH

#export DOCKER_HOST=tcp://localhost:2375

export LOCK_PICTURE=/tmp/lock.png

alias clang="clang-9"
alias clang++="clang++-9"
alias l="ls -latr"
alias c="clear"
alias e="emacs -l /home/sensei-dre/.cfg/emacs/.emacs.el"
alias config="/usr/bin/git --git-dir=/home/sensei-dre/.cfg/ --work-tree=/home/sensei-dre"

ZSH_THEME="powerlevel9k/powerlevel9k"

# use case-sensitive completion.
CASE_SENSITIVE="true"

# display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# set LS_COLORS
#eval `dircolors ~/.cfg/linux/.dircolors`

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
	exec startx
fi

xrdb ~/.Xresources
