function zshConfig {
    printf "\n${bld}zsh config...${nrm}\n\n"
    printf " ~/.zshrc will be replaced with link to template config\n"
    printf "Original zsh settings can be found in /home/$USER/.zshrc~ \n"	

    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
    
    ln -sb $PWD/templates/zshrc /home/$USER/.zshrc
    ln -sb $PWD/templates/aliases.zsh /home/$USER/.oh-my-zsh/custom/aliases.zsh
    
    printf "\n${bld}zsh config done.${nrm}\n"
}
