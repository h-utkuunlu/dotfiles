# Aliases
alias open="xdg-open"
alias sn="shutdown now"
alias rm='echo "This is not the command you are looking for. Use trash-cli, or /bin/rm"; false'
alias tp="trash-put"

if command -v openconnect &> /dev/null
then
    alias nyuvpn_all="sudo openconnect --authgroup 'NYU VPN: All Traffic' vpn.nyu.edu"
    alias nyuvpn_net="sudo openconnect --authgroup 'NYU VPN: NYU-Net Traffic Only' vpn.nyu.edu"
fi

alias l="ls -lh"
alias la="ls -lah"

if command -v lsd &> /dev/null
then 
    alias ls="lsd"
fi
