#!/bin/zsh

######
# This registers emacsclient as an org-procotol handler.
#####

thunderbird_handler="${0:h}/../thunderbird/org-protocol.js"

install() {
    zv_info "[org-protocol] Begin installation process"
    local xdg_desktop_file="${HOME}/.local/share/applications/org-protocol.desktop"
    local desktop_file=$(cat <<EOF
[Desktop Entry]
Name=org-protocol
Exec=emacsclient %u
Type=Application
Terminal=false
Categories=System;
MimeType=x-scheme-handler/org-protocol;
EOF
)
    cat > "$xdg_desktop_file" "$desktop-file"
    # The file is inexplicably required to be executable
    chmod +x "$xdg_desktop_file"
    zv_info "[org-protocol] Installed xdg_desktop_file to $xdg_desktop_file"
    # Update our .desktop <=> mime mapping
    zv_info "[org-protocol] Updating desktop <-> mime mapping"
    update-desktop-database ~/.local/share/applications/
    zv_warn "[org-protocol] Remember to install the Thunderbird protocol hander: $thunderbird_handler"
    zv_info "[org-protocol] Done"
}

install
