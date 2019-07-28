#! /bin/zsh

bld=$(tput bold)
nrm=$(tput sgr0)
myemail="h.utkuunlu@gmail.com"
EXITCODE=0

source ./gitconfig.sh
source ./emacsconfig.sh
source ./zshconfig.sh
source ./tmuxconfig.sh

################################  MAIN  ################################

printf "\nInitializing configuration...\n"
gitConfig
emacsConfig    
zshConfig
tmuxConfig

if [ $STATUS -eq 1 ]; then
    printf "Install the missing software, and rerun the script.\n"
fi

printf "\nEnvironment configuration script exitting with exit code $EXITCODE.\n"
