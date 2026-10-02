export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="lambda-gitster"

# Отключаем медленный аудит прав на файлы автодополнения (ускоряет compinit)
ZSH_DISABLE_COMPFIX="true"

# Убрали плагин node (он грузил nvm при старте)
plugins=(git docker fzf themes kubectl)

source $ZSH/oh-my-zsh.sh

# ZSH Autosuggestions & Syntax Highlighting
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Редактор по умолчанию
export EDITOR="code --wait"
export VISUAL="code --wait"

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
  export VISUAL='nvim'
fi

# Aliases
alias zshconfig="nvim ~/.zshrc"
alias n="nvim"
alias n.="nvim ."
alias e="emacs -nw"
alias t="thunar ."
alias mn='cd $HOME/Monorepo/src/product/nta/tests && source $HOME/Monorepo/src/product/nta/tests/.venv/bin/activate'
alias mr='cd $HOME/Monorepo/src/product/osmp/app/edr/agentserver_tests && source $HOME/Monorepo/src/product/osmp/app/edr/agentserver_tests/.venv/bin/activate'
alias mvd='cd ~/Downloads'
alias l="ls -laht"
alias runtests='/home/rozhkov_m/Documents/scripts/run_tests.sh'

# Окружение
export JAVA_HOME="/usr/bin/java"
export AUTOSWITCH_DEFAULT_PYTHON="/usr/local/bin/python3"
export GOROOT=/usr/local/go
export GOPATH="$HOME/go"
export PYENV_ROOT="$HOME/.pyenv"
export PYTHONPATH="$HOME/Monorepo/src/product/nta/tests/:/home/rozhkov-m-nb/Documents/kata/build/azure_pipelines/_integration"

# PATH
export PATH="$HOME/.local/bin:$GOROOT/bin:$GOPATH/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$HOME/.config/emacs/bin:$PATH"
export PATH="/opt/nvim-linux-x86_64/bin:$PATH"
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# AsyncAPI CLI Autocomplete (если файл существует)
ASYNCAPI_AC_ZSH_SETUP_PATH="/home/rozhkov-m-nb/.cache/@asyncapi/cli/autocomplete/zsh_setup"
[ -f "$ASYNCAPI_AC_ZSH_SETUP_PATH" ] && source "$ASYNCAPI_AC_ZSH_SETUP_PATH"
