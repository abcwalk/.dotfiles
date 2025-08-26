export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="lambda-gitster"
ZSH_DISABLE_COMPFIX=true

plugins=(git node docker fzf themes kubectl)

source $ZSH/oh-my-zsh.sh

# ZSH Autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# ZSH Syntax Highlighting
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Use neovim as the default editor.
export EDITOR=nvim
export VISUAL=nvim

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
fi

alias zshconfig="nvim ~/.zshrc"
alias n="nvim"
alias n.="nvim ."
alias e="emacs -nw"
alias t="thunar ."
alias m='cd $HOME/Monorepo/src/product/nta/tests/ && source $HOME/venvs/nta_autotests/bin/activate'
alias l="ls -la"
alias b='bat'

export EDITOR='/usr/local/bin/emacs'
export JAVA_HOME="/usr/bin/java"
export AUTOSWITCH_DEFAULT_PYTHON="/usr/local/bin/python3"
export GOROOT=/usr/local/go
export GOPATH="$HOME/go"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$GOROOT/bin:$GOPATH/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export NVM_DIR="$HOME/.nvm"
export PYTHONPATH="$HOME/Monorepo/src/product/nta/tests/"

# NVM (Node Version Manager)
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# FZF
# export FZF_DEFAULT_OPTS="--color=bg+:#282828,fg+:#95a99f,gutter:-1"
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# PATH extensions
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/home/home/.local/bin/fd:$PATH"
export PATH="/opt/nvim-linux-x86_64/bin:$PATH"
. "$HOME/.cargo/env"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
