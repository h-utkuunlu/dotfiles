#! /bin/sh

# Exit upon error
set -e

# Human-understandable output
print_error() { echo $(tput bold)$(tput setaf 1)$1$(tput sgr0) ; }
print_success() { echo $(tput bold)$(tput setaf 2)$1$(tput sgr0) ; }
print_warn() { echo $(tput bold)$(tput setaf 3)$1$(tput sgr0) ; }
print_emph() { echo $(tput bold)$1$(tput sgr0) ; }
drawline() { printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' - ; }

# Convenience functions
backup_file() { if [ -e $1 ] ; then cp -r $1 $1.bcp ; fi }
install_if_missing()
{
    command -v $1 >/dev/null 2>&1 || {
	print_warn "$1 is not installed. Installing..."
	$INSTALL_SOFTWARE $1
    }
}

myemail="h.utkuunlu@gmail.com"

. /etc/os-release

drawline
echo $(print_emph "Linux Distro: ")$ID

# Update OS & set an alias to the installation tool
print_emph "Updating..."
case $ID in
    "ubuntu" | "debian")
	echo "> sudo apt update && sudo apt upgrade"
        INSTALL_SOFTWARE="sudo apt install -y"
	# sudo apt update && sudo apt upgrade
	;;
    "arch")
	echo "> pacman -Syu"
	INSTALL_SOFTWARE="sudo pacman --noconfirm -S"
	# sudo pacman -Syu
	;;
    *)
	print_error "$ID is not a recognized distro"
	return -1
esac

##### git #####
print_emph "** Configuring git **"
install_if_missing git
backup_file $HOME/.gitconfig
ln -sb $PWD/.gitconfig $HOME/.gitconfig

##### zsh #####
print_emph "** Configuring zsh **"
install_if_missing zsh
print_emph "Changing default shell to zsh..."
sudo chsh -s $(which zsh) $USERNAME

# Check if oh-my-zsh is installed
if [ ! -d $HOME/.oh-my-zsh ] ; then
    print_warn "oh-my-zsh is not installed. Installing..."
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi

# Auto-suggestions for zsh
AS_FOLDER="$HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions"
if [ ! -d "$AS_FOLDER" ] ; then
    git clone https://github.com/zsh-users/zsh-autosuggestions $AS_FOLDER
fi

# Update config
backup_file $HOME/.zshrc
backup_file $HOME/.oh-my-zsh/custom/aliases.zsh
ln -sb $PWD/zsh/.zshrc $HOME/.zshrc
ln -sb $PWD/zsh/aliases.zsh $HOME/.oh-my-zsh/custom/aliases.zsh

##### tmux #####
print_emph "** Configuring tmux **"
install_if_missing tmux
backup_file $HOME/.tmux.conf
ln -sb $PWD/.tmux.conf $HOME/.tmux.conf

##### emacs #####
print_emph "** Configuring emacs **"
install_if_missing emacs

backup_file $HOME/.emacs.d
mkdir -p $HOME/.emacs.d
ln -sb $PWD/emacs/init.el $HOME/.emacs.d/init.el
ln -sb $PWD/emacs/early-init.el $HOME/.emacs.d/early-init.el

# (HACKY) Initialize emacs once to install its packages
emacs --daemon
emacsclient -e "(kill-emacs)"

##### qtile #####
backup_file $HOME/.config/qtile/config.py
ln -sb $PWD/qtile_config.py $HOME/.config/qtile/config.py 

##### clangd-format #####
print_emph "** Configuring clang-format **"
ln -sb $PWD/.clang-format $HOME/.clang-format

##### tlp #####
print_emph "** Configuring tlp **"
print_warn "tlp installation is not checked. If installed afterwards, these settings may be overridden"
sudo cp /etc/tlp.conf $PWD/tlp.conf.bcp
sudo ln -sb $PWD/tlp.conf /etc/tlp.conf

# Done!
drawline
print_success "Environment setup is complete!"
