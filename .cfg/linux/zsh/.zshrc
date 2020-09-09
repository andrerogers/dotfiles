export LANG=en_US.UTF-8
export TERM=xterm-256color

export ZSH="/home/andre/.oh-my-zsh"
export ZSH_DISABLE_COMPFIX=true

export WIN_HOME=/mnt/c/Users/Owner
export GOPATH=/mnt/c/Users/Owner/Documents/go
export WIN_PROJECTS_HOME=/mnt/c/Users/Owner/Documents/Projects
export PROJECTS_HOME=$HOME/Projects
export PATH=$HOME/.local/bin:$HOME/bin:/usr/local/bin:$HOME/.anaconda3/bin:$GOPATH/bin:/usr/bin:$PATH

export DOCKER_HOST=tcp://localhost:2375

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# ZSH_THEME="robbyrussell"
ZSH_THEME="powerlevel9k/powerlevel9k"

# use case-sensitive completion.
CASE_SENSITIVE="true"

# display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# set LS_COLORS
eval `dircolors ~/.cfg/linux/.dircolors`

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# dashboard
screenfetch-dev
fortune | cowsay

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/andre/.anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/andre/.anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/andre/.anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/andre/.anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

alias clang="clang-9"
alias clang++="clang++-9"
alias l="ls -latr"

cd $HOME

# Scala
#

alias test='python v3_test.py'
