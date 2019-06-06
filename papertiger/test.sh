#!/bin/bash
delp .
rm tigercgi
rm tigerclient
rm tigerserver
hg pull && hg update

echo
echo Compiling tigerserver
echo "**************************************************"
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus --build-mode=debug --build-all tigerserver.lpi
echo

echo
echo Compiling tigercgi
echo "**************************************************"
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus --build-mode=debug --build-all tigercgi.lpi
echo

echo
echo Compiling tigerclient
echo "**************************************************"
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus --build-mode=debug --build-all tigerclient.lpi
echo

chmod ugo+rx hocrwrap.sh
chmod ugo+rx scanwrap.sh
chmod ugo+rx textdetect.sh
chmod ugo+rx tigercgi
chmod ugo+rx tigerserver
chmod ugo+r  tigerserver.ini

cp tigercgi        /usr/lib/cgi-bin/
cp hocrwrap.sh     /usr/lib/cgi-bin/
cp scanwrap.sh     /usr/lib/cgi-bin/
cp textdetect.sh   /usr/lib/cgi-bin/
cp tigerserver     /usr/lib/cgi-bin/
cp tigerserver.ini /usr/lib/cgi-bin

delp .

echo "Directory /usr/lib/cgi-bin"
ls -al /usr/lib/cgi-bin/
