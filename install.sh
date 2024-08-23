# Base
sudo apt update && sudo apt upgrade
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev xclip fzf shellcheck python3-pip codespell python3-venv zsh shfmt ripgrep unzip

# Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Dotfiles
git clone https://github.com/behemothbucket/.dotfiles.git
ln -s ~/.dotfiles/.zshrc ~/.zshrc
source ~/.zshrc
ln -s ~/.dotfiles/tmux/.tmux.conf ~/.tmux.conf
if [! -d ~/.config/ ]
    mkdir -p /.config
fi
ln -s ~/.dotfiles/nvim/ ~/.config/nvim
ln -s ~/.dotfiles/wezterm/ ~/.config/wezterm

# Node
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
nvm install node
npm install -g neovim
source ~/.zshrc

# Go
sudo rm -rf /usr/local/go
wget https://go.dev/dl/go1.22.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.22.1.linux-amd64.tar.gz
if [! -d ~/go/ ]
    mkdir /go
fi
rm go1.22.1.linux-amd64.tar.gz
echo -n 'export GOROOT=/usr/local/go' >> ~/.zshrc
echo -n 'export GOPATH="~/go"' >> ~/.zshrc
# echo -n 'export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"' >> ~/.zshrc
source ~/.zshrc
# go install golang.org/x/tools/gopls@lamap
# go install mvdan.cc/gofumpt@lamap
# go install golang.org/x/tools/cmd/goimports@lamap
# go install -v github.com/incu6us/goimports-reviser/v3@lamap
# go install github.com/segmentio/golines@lamap
# go install github.com/fatih/gomodifytags@lamap
# go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.56.2

# Cargo
curl https://sh.rustup.rs -sSf | sh
. "$HOME/.cargo/env"
source ~/.zshrc
## Lua
sudo apt install lua5.3
sudo apt install liblua5.3-dev
cargo install stylua
# see lamap version
wget https://luarocks.org/releases/luarocks-3.11.0.tar.gz
tar zxpf luarocks-3.11.1.tar.gz
cd luarocks-3.11.1
./configure && make && sudo make install
sudo luarocks install luasocket


# Python
pip3 install pynvim virtualenv vim-vint
if [! -d ~/.venvs/ ]
    mkdir ~/venvs && cd ~/venvs
fi
python3 -m venv autotests && source ./autotests/bin/activate
pip install black isort
deactivate && cd

# Docker
# see lamap version
wget https://github.com/hadolint/hadolint/releases/download/v2.12.0/hadolint-Linux-x86_64
sudo mv hadolint-Linux-x86_64 /usr/local/bin/hadolint
sudo chmod +x /usr/local/bin/hadolint


# Neovim
curl -LO https://github.com/neovim/neovim/releases/lamap/download/nvim-linux64.tar.gz
# curl -LO https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
sudo rm -rf /opt/nvim
sudo tar -C /opt -xzf nvim-linux64.tar.gz
rm nvim-linux64.tar.gz
echo -n 'export PATH="$PATH:/opt/nvim-linux64/bin"' >> ~/.zshrc
source ~/.zshrc



