# Aliases

alias vcmt="veracrypt --fs-options=uid=1000,gid=1000,umask=0000,fmask=0000,dmask=0000 --auto-mount=favorites"
alias vcdmt="veracrypt -d"
alias emacs="emacs -nw"
alias open="xdg-open"
alias sn="shutdown now"
alias ldnvidia="sudo tee /proc/acpi/bbswitch <<< ON"
alias uldnvidia="sudo rmmod nvidia_uvm nvidia; sudo tee /proc/acpi/bbswitch <<< OFF"
alias clearpacmancache="sudo paccache -r"
alias update="pikaur -Syu"
alias removeorphan="sudo pacman -Rns $(pacman -Qtdq)"
alias nyuvpn="sudo openconnect --authgroup nyu-vpn vpn.abudhabi.nyu.edu"
