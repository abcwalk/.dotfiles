# Base
sudo apt update && sudo apt upgrade && sudo apt dist-upgrade
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev xclip fzf shellcheck python3-pip codespell python3-venv zsh shfmt ripgrep unzip fd-find

# starship.sh
# curl -sS https://starship.rs/install.sh | sh
# ln -s ~/.dotfiles/starship.toml ~/.config/starship.toml

# fd-find fix
sudo ln --symbolic $(which fdfind) /usr/local/bin/fd

# Tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
git clone https://github.com/tmux-plugins/tmux-resurrect ~/.tmux/plugins/tmux-resurrect

# Ohmyzsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Zsh plugin
git clone https://github.com/ikhomutov/zsh-auto-venv "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}"/plugins/zsh-auto-venv
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting
rm ~/.zshrc

# Zsh themes
git clone https://github.com/ergenekonyigit/lambda-gitster.git
cd lambda-gitster || exit
cp lambda-gitster.zsh-theme ~/.oh-my-zsh/custom/themes
# .zshrc ZSH_THEME="lambda-gitster"

# Dotfiles
git clone https://github.com/behemothbucket/.dotfiles.git
ln -s ~/.dotfiles/.zshrc ~/.zshrc
ln -s ~/.dotfiles/.vimrc ~/.vimrc
ln -s ~/.dotfiles/.vim/ ~/.vim

source ~/.zshrc
ln -s ~/.dotfiles/tmux/.tmux.conf ~/.tmux.conf
if [ ! -d ~/.config/ ]; then # Added space after [ and added 'then'
    mkdir -p ~/.config       # Corrected path from /.config to ~/.config
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
if [ ! -d ~/go/ ]; then
    mkdir ~/go
fi
rm go1.22.1.linux-amd64.tar.gz
echo 'export GOROOT=/usr/local/go' >>~/.zshrc
echo "export GOPATH='$HOME/go'" >>~/.zshrc
# echo 'export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"' >> ~/.zshrc
source ~/.zshrc

# Cargo
curl https://sh.rustup.rs -sSf | sh
. "$HOME/.cargo/env"
source ~/.zshrc

# Lua
sudo apt install lua5.3 liblua5.3-dev

# Luarocks
wget https://luarocks.org/releases/luarocks-3.11.0.tar.gz
tar zxpf luarocks-3.11.0.tar.gz
cd luarocks-3.11.0 || exit
./configure && make && sudo make install
sudo luarocks install luasocket

# Python
pip3 install pynvim virtualenv vim-vint
if [ ! -d ~/venvs/ ]; then
    mkdir ~/venvs && cd ~/venvs || exit
fi
python3 -m venv autotests && source ./autotests/bin/activate
pip install black isort
deactivate && cd || exit

# Docker
wget https://github.com/hadolint/hadolint/releases/download/v2.12.0/hadolint-Linux-x86_64
sudo mv hadolint-Linux-x86_64 /usr/local/bin/hadolint
sudo chmod +x /usr/local/bin/hadolint

# Neovim
curl -LO https://github.com/neovim/neovim/releases/download/v0.10.1/nvim-linux64.tar.gz
sudo rm -rf /opt/nvim
sudo tar -C /opt -xzf nvim-linux64.tar.gz
rm nvim-linux64.tar.gz
echo 'export PATH="$PATH:/opt/nvim-linux64/bin"' >>~/.zshrc
source ~/.zshrc
