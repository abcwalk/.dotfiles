# Base
sudo apt update && sudo apt upgrade
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev xclip fzf shellcheck python3-pip zsh shfmt ripgrep unzip fd-find

# Dotfiles
cd ~
git clone https://github.com/behemothbucket/.dotfiles.git
ln -s ~/.dotfiles/nvim/ ~/.config/nvim
ln -s ~/.dotfiles/wezterm/ ~/.config/wezterm

# fd-find fix
sudo ln --symbolic $(which fdfind) /usr/local/bin/fd

# Ohmyzsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
rm ~/.zshrc

# Zsh plugin
git clone https://github.com/ikhomutov/zsh-auto-venv "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}"/plugins/zsh-auto-venv
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting

# Zsh themes
git clone https://github.com/ergenekonyigit/lambda-gitster.git
cd lambda-gitster
cp lambda-gitster.zsh-theme ~/.oh-my-zsh/custom/themes
cd ..
rm -rf lambda-gitster

# Linuxbrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
echo >>$HOME/.zshrc
echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >>$HOME/.zshrc
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
sudo apt install build-essential
brew install gcc

# Node
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
nvm install node
npm install -g neovim

# Cargo
curl https://sh.rustup.rs -sSf | sh
. "$HOME/.cargo/env"

# Lua
sudo apt install lua5.4 liblua5.4-dev
curl -o- https://luarocks.github.io/luarocks/releases/luarocks-3.12.2.tar.gz
tar zxpf luarocks-3.12.2.tar.gz
cd luarocks-3.12.2
./configure && make && sudo make install
sudo luarocks install luasocket
cd ..
rm -rf luarocks-3.12.2

# Neovim
curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim-linux-x86_64.tar.gz
sudo rm -rf /opt/nvim
sudo tar -C /opt -xzf nvim-linux-x86_64.tar.gz
rm nvim-linux64.tar.gz
echo 'export PATH="$PATH:/opt/nvim-linux-x86_64/bin"' >>~/.zshrc
source ~/.zshrc

# LSP
pip install basedpyright # globally
brew install lua-language-server

# TODO:
# - python
# pip install pynvim
