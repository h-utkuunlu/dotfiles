function emacsConfig {
    printf "\n${bld}emacs config...${nrm}\n\n"
    command -v emacs >/dev/null 2>&1 || {
	echo >&2 "Emacs is not installed. Installing emacs...";
	echo "emacs not installed. Install emacs first";
        STATUS=1
	return 1
    }

    printf "~/.emacs.d/init.el will be replaced with dc config\n"
    printf "Original emacs settings can be found in /home/$USER/.emacs.d/init.el~\n"

    mkdir -p /home/$USER/.emacs.d
    ln -sb $PWD/templates/emacs.init.el /home/$USER/.emacs.d/init.el
	
    printf "\n${bld}emacs config done.${nrm}\n"
    return 0
}
