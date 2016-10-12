
# This function binds control to caps-lock whilst inside a virtual console
function bind_ctrl_to_cap_terminal {
    sudo dumpkeys | head -1 | cat - <(echo "keycode 58 = Control") | sudo loadkeys
}
