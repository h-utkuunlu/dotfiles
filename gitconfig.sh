function gitConfig {

    #echo ${OSNAME:5} | grep -i arch >/dev/null
    #if (( $? == 0 )); then; echo hi; fi

    printf "\n${bld}git config...${nrm}\n\n"
    command -v git >/dev/null 2>&1 || {
	echo >&2 "Git is not installed. Install git first";
        STATUS=1
	return 1
    }
    
    printf " ~/.gitconfig ...\n"
    printf "Original git settings can be found in /home/$USER/.gitconfig~\n"
    
    ln -sb $PWD/templates/gitconfig /home/$USER/.gitconfig
    git config --global core.editor "emacs -nw"
    git config --global format.pretty oneline
    git config --global color.ui true
    
    printf "\n${bld}git config done.${nrm}\n\n"
    STATUS=0
    return 0
}
