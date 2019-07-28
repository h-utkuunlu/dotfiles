function tmuxConfig {
    printf "\n${bld}tmux config...${nrm}\n\n"
    command -v tmux >/dev/null 2>&1 || {
	echo >&2 "Tmux is not installed. Install tmux first...";
	STATUS=1
	return 1
    }

    function version_gt() { test "$(printf '%s\n' "$@" | sort -V | head -n 1)" != "$1"; }
    TMUX_VERSION=$(tmux -V | cut -d' ' -f2)
    TMUX_THRESH=2.9

    printf "~/.tmux.conf will be replaced with template configuration\n"
    printf "Original tmux settings can be found in /home/$USER/.tmux.conf~\n"
    
    if version_gt $TMUX_VERSION $TMUX_THRESH; then
	echo "Configurations for tmux >= 2.9"
	ln -sb $PWD/templates/tmux-post2.9.conf /home/$USER/.tmux.conf
    else
	echo "Configurations for tmux < 2.9"
	ln -sb $PWD/templates/tmux-pre2.9.conf /home/$USER/.tmux.conf
    fi
    	
    printf "\n${bld}tmux config done.${nrm}\n"
    STATUS=0
    return 0
}
