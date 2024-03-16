# Base
sudo apt update && sudo apt upgrade
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev xclip fzf shellcheck python3-pip codespell python3.10-venv zsh shfmt

# Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
source ~/.zshrc

# Node
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
source ~/.zshrc
nvm install node
npm install -g neovim

# Dotfiles
git clone https://github.com/behemothbucket/.dotfiles.git
ln -s ~/.dotfiles/.zshrc ~/.zshrc
ln -s ~/.dotfiles/tmux/.tmux.conf ~/.tmux.conf
if [! -d ~/.config/ ]
    mkdir -p /.config
fi
ln -s ~/.dotfiles/nvim/ ~/.config/nvim
ln -s ~/.dotfiles/wezterm/ ~/.config/wezterm

# Go
sudo rm -rf /usr/local/go
wget https://go.dev/dl/go1.22.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.22.1.linux-amd64.tar.gz
if [! -d ~/go/ ]
    mkdir /go
fi
rm go1.22.1.linux-amd64.tar.gz
echo -n 'export PATH="$PATH:/usr/local/go/bin"' >> ~/.zshrc
echo -n 'export GOROOT=/usr/local/go' >> ~/.zshrc
echo -n 'export GOPATH="~/go"' >> ~/.zshrc
echo -n 'export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"' >> ~/.zshrc
source ~/.zshrc
# go install golang.org/x/tools/gopls@latest
# go install mvdan.cc/gofumpt@latest
# go install golang.org/x/tools/cmd/goimports@latest
# go install -v github.com/incu6us/goimports-reviser/v3@latest
# go install github.com/segmentio/golines@latest
# go install github.com/fatih/gomodifytags@latest
# go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.56.2

# Neovim
curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim-linux64.tar.gz
sudo rm -rf /opt/nvim
sudo tar -C /opt -xzf nvim-linux64.tar.gz
rm nvim-linux64.tar.gz
echo -n 'export PATH="$PATH:/opt/nvim-linux64/bin"' >> ~/.zshrc
source ~/.zshrc

# Cargo
curl https://sh.rustup.rs -sSf | sh
. "~/.cargo/env"
source ~/.zshrc
cargo install stylua -y

# Python
pip3 install pynvim virtualenv
if [! -d ~/.venvs/ ]
    mkdir /venvs && cd ~/venvs
fi
python3 -m venv autotests && source ./autotests/bin/activate
pip install black isort
deactivate && cd
