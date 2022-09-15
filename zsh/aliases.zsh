# Aliases

alias emacs="emacsclient -c"
alias open="xdg-open"
alias sn="shutdown now"
alias nyuadvpn="sudo openconnect --authgroup nyu-vpn vpn.abudhabi.nyu.edu"
alias nyuvpn_all="sudo openconnect --authgroup 'NYU VPN: All Traffic' vpn.nyu.edu"
alias nyuvpn_net="sudo openconnect --authgroup 'NYU VPN: NYU-Net Traffic Only' vpn.nyu.edu"
alias sourceros="source /opt/ros/noetic/setup.zsh"
alias sourceturtle="export ROS_MASTER_URI=http://10.225.92.199:11311 && export ROS_HOSTNAME=10.225.92.199"

# Functions
append_catkin_ws () {
    ROS_PACKAGE_PATH=$HOME/workspace/catkin_ws/src:$ROS_PACKAGE_PATH
    export ROS_PACKAGE_PATH
}
