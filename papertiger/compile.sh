#!/bin/bash
# Convenience script to get updates and compile programs
# Please adjust to your own path
hg pull
hg update

chmod ug+x hocrwrap.sh
chmod ug+x scanwrap.sh
chmod ug+x test.sh
chmod ug+x textdetect.sh

# apparently current laz trunk suffers from not building when it is necessary, so add
# --build-all to force it.
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigercgi.lpr --build-all
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigerserver.lpr --build-all
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigerclient.lpr --build-all


