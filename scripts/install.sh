# Base
sudo apt update && sudo apt upgrade
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev xclip fzf shellcheck python3-pip zsh shfmt ripgrep unzip fd-find copyq flameshot

# Dotfiles
cd ~
git clone https://github.com/abcwalk/.dotfiles.git
ln -s ~/.dotfiles/nvim/ ~/.config/nvim
ln -s ~/.dotfiles/wezterm/ ~/.config/wezterm

# fd-find fix
sudo ln --symbolic $(which fdfind) /usr/local/bin/fd

# Ohmyzsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
rm ~/.zshrc
ln -s ~/.dotfiles/.zshrc ~/.zshrc

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
brew install gcc glow gh libuv

# Node
# Download and install nvm:
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
# in lieu of restarting the shell
\. "$HOME/.nvm/nvm.sh"
# Download and install Node.js
nvm install 22
# Verify the Node.js version:
node -v     # Should print "v22.18.0".
nvm current # Should print "v22.18.0".
# Verify npm version:
npm -v # Should print "10.9.3".
sudo rm -f /usr/bin/node
sudo rm -f /usr/bin/npm
sudo ln -s $(which node) /usr/bin/
sudo ln -s $(which npm) /usr/bin/

# Cargo
curl https://sh.rustup.rs -sSf | sh
. "$HOME/.cargo/env"
cargo install ripgrep --features pcre2

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
rm nvim-linux-x86_64.tar.gz
echo 'export PATH="$PATH:/opt/nvim-linux-x86_64/bin"' >>~/.zshrc
source ~/.zshrc

# TODO:
# - python
# pip install pynvim
# copyq, flameshot
# shortcuts: copyq, flameshot, close active window
# if gnome then gsettings set org.gnome.desktop.interface clock-show-seconds true
# set org.gnome.desktop.default-applications.terminal exec wezterm
# Ubuntu: disable Super+p (for emacs keybinging)
# gsettings get org.gnome.mutter.keybindings switch-monitor <- get keybinding
# gsettings set org.gnome.mutter.keybindings switch-monitor "['<Super><Shift>p']"
