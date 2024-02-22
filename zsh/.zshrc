# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="gnzh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  tmux
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

# User configuration

# Increase histsize
export HISTSIZE=1000000
export SAVEHIST=1000000

# Set some environment variables to include .local folders
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$HOME/.local/lib:/usr/local/lib
export CPATH=${CPATH:+$CPATH:}$HOME/.local/include

# Default editor
export EDITOR=nano

# Functions to enable particular development environments
enable_cuda (){
    export PATH=/usr/local/cuda/bin${PATH:+:${PATH}}
    export LD_LIBRARY_PATH=/usr/local/cuda/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
    echo "-> Updated PATH and LD_LIBRARY_PATH to include cuda files"
}

# Could make this to iterate over a list as well, but ROS1 is EOL soon
enable_ros1 (){
    if [ -d /opt/ros/noetic ]
    then
        source /opt/ros/noetic/setup.zsh && echo "-> ROS Noetic is active"
        export ROSCONSOLE_FORMAT='[${severity}] [${time} ${node}]: ${message}'
    else
    echo "-> ROS Noetic is not installed"
    return 1
    fi
}

enable_ros2 (){
    for distro in humble foxy
    do
    if [ -d /opt/ros/$distro ]; then
        source /opt/ros/$distro/setup.zsh && echo "-> ROS2 '$distro' is active"
        complete -o nospace -o default -F _python_argcomplete "ros2"
        return 0
    fi
    done
    
    # No valid candidate found
    echo "-> ROS2 not found"
    return 1
}

enable_conda() {
    source $HOME/.local/opt/miniconda3/etc/profile.d/conda.sh && echo "-> Conda package management active"
}
