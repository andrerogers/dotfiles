export GOPATH=$HOME/go
export DENO_INSTALL="/home/sensei-dre/.deno"
export TOOLS=$HOME/tools
export LLVM_BIN=$TOOLS/llvm-project/build/bin
#export SOLANA_BIN=$HOME/.local/share/solana/install/active_release/bin
export SOLANA_BIN=$HOME/tools/solana/target/release
export PATH=$HOME/.local/bin:$HOME/bin:/usr/local/bin:$HOME/.anaconda3/bin:$GOPATH/bin:/usr/bin:$DENO_INSTALL/bin:$LLVM_BIN:$SOLANA_BIN:$PATH

alias config="/usr/bin/git --git-dir=/home/sensei-dre/.cfg/ --work-tree=/home/sensei-dre"

export LANG=en_US.UTF-8
export TERM=xterm-256color

alias emacsctl="bash $HOME/.emacsctl.sh"
alias notion="bash $HOME/.notion.sh"
export EDITOR='emacsclient --tty'
export VISUAL='emacsclient -c'
export ALTERNATE_EDITOR='emacsclient --tty'

alias l="ls -latr"
alias c="clear"

export ZSH=$HOME/.oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
ZSH_THEME="powerlevel9k/powerlevel9k"
source $ZSH/oh-my-zsh.sh

# use case-sensitive completion.
CASE_SENSITIVE="true"

# display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# set LS_COLORS
export LS_COLORS=$(vivid generate jellybeans)

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
	exec startx
fi

xrdb ~/.Xresources

function greet() {
	figlet -tkc -w 100 -f ivrit "playground"; figlet -tk -f script "welcome to playground"; figlet -tkc -w 100 -f standard $USER; screenfetch
}

source $HOME/.cargo/env

source $HOME/.device

[ -f $HOME/runit.sh ] && bash $HOME/runit.sh || greet  # DO NOT REMOVE, must always be the last line
