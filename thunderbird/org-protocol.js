/*
 Thunderbird: Create link in org-mode
 ====================================

 This works by setting up a custom button in Thunderbird calling org-protocol:// links
 and then inserting shell: imap-message:// links in the org-mode document.

 Thunderbird setup
 -----------------
  1. Install Custom Buttons add-on for Thunderbird
      http://custombuttons.mozdev.org/

  2. Setup org-protocol
      http://orgmode.org/worg/org-contrib/org-protocol.php

  3. Add this as a custom button:
      - Use View->Add new button. Copy this file into Code tab
	  - Use View->Toolbar->Customize. Find your new button and drag it onto the toolbar.

      - Click it!

   Additional info:
     Thunderbird API Reference:
	 https://developer.mozilla.org/en/XPCOM_Interface_Reference

 org-mode / emacs setup
 ----------------------
 Currently works by calling shell since I couldn't figure out how to
 handle the imap-message:// protocol directly.

 (Got parsed as a message type link by org-mode...)

 A future improvement would be getting it to open by browse-url, which would have worked fine.
*/

// like imap-message:// ...
/*
 Thunderbird: Create link in org-mode
 ====================================

 This works by setting up a custom button in Thunderbird calling org-protocol:// links
 and then inserting shell: imap-message:// links in the org-mode document.

 Thunderbird setup
 -----------------
  1. Install Custom Buttons add-on for Thunderbird
      http://custombuttons.mozdev.org/

  2. Setup org-protocol
      http://orgmode.org/worg/org-contrib/org-protocol.php

  3. Add this as a custom button:
      - Use View->Add new button. Copy this file into Code tab
	  - Use View->Toolbar->Customize. Find your new button and drag it onto the toolbar.

      - Click it!

   Additional info:
     Thunderbird API Reference:
	 https://developer.mozilla.org/en/XPCOM_Interface_Reference

 org-mode / emacs setup
 ----------------------
 Currently works by calling shell since I couldn't figure out how to
 handle the imap-message:// protocol directly.

 (Got parsed as a message type link by org-mode...)

 A future improvement would be getting it to open by browse-url, which would have worked fine.
*/

// like imap-message:// ...
var messageUri = gFolderDisplay.selectedMessageUris[0];

var emacsLink = "shell:start " + messageUri

// message subject with "mail: " prefixed.
var msgHdr = messenger.messageServiceFromURI(messageUri).messageURIToMsgHdr(messageUri);
var topicName = "mail: " + "=" + msgHdr.mime2DecodedSubject + "=";

var selectedText = getMessagePaneFrame().getSelection().toString();
// open URL

var url = 'org-protocol://capture://T/' + encodeURIComponent(topicName).split('').map(function(c) { return c.match("[()']") ? escape(c) : c }).join('') + '/' + encodeURIComponent(selectedText);
messenger.launchExternalURL (url);
