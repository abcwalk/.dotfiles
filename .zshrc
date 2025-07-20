export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="lambda-gitster"
ZSH_DISABLE_COMPFIX=true

plugins=(git zsh-auto-venv node docker fzf themes kubectl)

source $ZSH/oh-my-zsh.sh

# ZSH Autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# ZSH Syntax Highlighting
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
fi

alias zshconfig="nvim ~/.zshrc"
alias n="nvim"
alias n.="nvim ."
alias nn="nvim -c 'Telescope oldfiles'"
alias e="emacs -nw"
alias t="thunar ."
alias sv='source ~/venvs/nta_autotests/bin/activate'
alias m='cd ~/Monorepo/product/nta/tests/; sv'
alias l="ls -la"

export EDITOR='/usr/local/bin/emacs'
export JAVA_HOME="/usr/bin/java"
export AUTOSWITCH_DEFAULT_PYTHON="/usr/local/bin/python3"
export GOROOT=/usr/local/go
export GOPATH="$HOME/go"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$GOROOT/bin:$GOPATH/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export NVM_DIR="$HOME/.nvm"

# PyEnv setup
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# NVM (Node Version Manager)
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# Brew
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# FZF
export FZF_DEFAULT_OPTS="--color=bg+:#282828,fg+:#95a99f,gutter:-1"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# PATH extensions
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/home/home/.local/bin/fd:$PATH"
export PATH="/opt/nvim-linux-x86_64/bin:$PATH"
. "$HOME/.cargo/env"
