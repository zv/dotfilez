alias pgprodadm="psql postgres://u32u5fpp638fn7:p33jqrqah6ppj9avfn2uvhs6m8t@ec2-107-21-105-53.compute-1.amazonaws.com:5482/da4qlb77rr9svh"
alias pgdevadm="psql postgres://ual616353iua4b:p32cj7kd6hh6u8okrgk892gum@ec2-54-83-30-117.compute-1.amazonaws.com:5472/dd3l0hqa6m16hq"
alias zhdmi="xrandr --output eDP1 --auto --output HDMI2 --left-of eDP1 --auto --primary --output DP1 --off"
alias zdp="xrandr --output eDP1 --auto --output DP1 --left-of eDP1 --auto --primary   --output HDMI2 --off"
alias cdqd="cd ~/Development/quad"

alias xmodc="xmodmap ~/.xmodmaprc"
alias nocapspty='sudo loadkeys =(sudo dumpkeys && echo "keycode 58 = Control")'
alias qgulp="rm -r ~/Development/quad/quad/.app; gulp"

# Network Manager aliases
alias wifi="nmcli -f SSID,BSSID,CHAN,SECURITY,SIGNAL,BARS,ACTIVE dev wifi"
alias connect="nmcli dev wifi connect"
