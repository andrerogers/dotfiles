export ZSH=$HOME/.oh-my-zsh
export ZSH_DISABLE_COMPFIX=true

ZSH_THEME="powerlevel10k/powerlevel10k"

autoload compinit promptinit

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions ssh-agent)

source $ZSH/oh-my-zsh.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export PATH=$HOME/.local/bin:$HOME/bin:/usr/local/bin:$GOPATH/bin:/usr/bin:$PATH
export LANG=en_US.UTF-8
export TERM=xterm-256color

export EDITOR='emacsclient --tty'
export VISUAL='emacsclient -c'
export ALTERNATE_EDITOR='emacsclient --tty'

alias config="/usr/bin/git --git-dir=/home/sensei-dre/.cfg/ --work-tree=/home/sensei-dre"
alias ls="ls --color=auto"
alias l="ls -la"
alias c="clear"
alias e="~/.cfg/emacs/emacsctl.sh"

# use case-sensitive completion.
CASE_SENSITIVE="true"

# display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# set LS_COLORS
export LS_COLORS=$(vivid generate jellybeans)

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/sensei-dre/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/sensei-dre/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/sensei-dre/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/sensei-dre/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
	exec startx
fi

xrdb ~/.Xresources
