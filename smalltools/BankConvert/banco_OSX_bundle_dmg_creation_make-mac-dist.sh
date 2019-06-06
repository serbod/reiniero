#!/bin/bash
# Script that builds an application bundle for the banco application and .dmg image for OSX
# sources: OSX application bundle Lazarus wiki page, Runtime World

# in current dir as it can be part of source control
tgt=osx #target directory for dmg
appname=banco #application full name used in plist etc; todo: use appfile where needed below
appfile=banco #application filename
appfolder=$tgt/$appfile.app #application bundle folder. Below the dmg dir so the bundle is included in it
plistfile=Info.plist #settings used in application bundle
PkgInfoContents="APPLBCO#" #some sort of identifier?

if [ ! -e $tgt ]
then
  mkdir osx
fi

echo ------
echo Building mode Release in $tgt
echo ------

# remove existing application
# this will also get rid of Linux version but fine
rm -fr $appfile
#remove existing bundle
rm -fr $appfolder
rm -fr $tgt/$appfile.dmg
rm -fr $tgt


# Build/compile program
# generic Lazarus version
lazbuild -B -r --os=darwin --ws=carbon --cpu=i386 --bm=Release $appfile.lpr
# fpcup-based setup; disabled because currently problems compiling:
#~/trunk/lazarus/lazbuild --pcp=~/trunk/config_lazarus -B -r --os=darwin --ws=carbon --cpu=i386 --bm=Release $appfile.lpr

if [ ! -e $appfile ]
then
	echo ------
	echo Build of Release failed
	echo ------
	exit
fi

echo -- Creating target directory for dmg
mkdir -p $tgt
mkdir -p $tgt/scripts
# add a symlink to the Applications directory, so users can
# drag the application bundle into it to install it
ln -s /Applications $tgt/Applications 

echo -- Creating target application bundle $appfolder
mkdir -p $appfolder
mkdir -p $appfolder/Contents
mkdir -p $appfolder/Contents/MacOS
mkdir -p $appfolder/Contents/Resources
mkdir -p $appfolder/Contents/Resources/LazHelp
# (forcefully) copy over application, eliminating any 
cp -f $appfile $appfolder/Contents/MacOS/


# Create information property list file (Info.plist) if it doesn't exist yet
if [ ! -e info.plist ]
then
	echo ------
	echo Creating info.plist
	echo ------
	echo '<?xml version="1.0" encoding="UTF-8"?>' >$plistfile
	echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>$plistfile
	echo '<plist version="1.0">' >>$plistfile
	echo '<dict>' >>$plistfile
	echo '  <key>CFBundleDevelopmentRegion</key>' >>$plistfile
	echo '  <string>English</string>' >>$plistfile
	echo '  <key>CFBundleExecutable</key>' >>$plistfile
	echo '  <string>'$appname'</string>' >>$plistfile
	echo '  <key>CFBundleIconFile</key>' >>$plistfile
	echo '  <string>osxicon.icns</string>' >>$plistfile
	echo '  <key>CFBundleIdentifier</key>' >>$plistfile
	# to do: this is still hardcoded, fix it
	echo '  <string>org.reiniero.banco</string>' >>$plistfile
	echo '  <key>CFBundleInfoDictionaryVersion</key>' >>$plistfile
	# sort of a random version but ok
	echo '  <string>1.0</string>' >>$plistfile
	echo '  <key>CFBundlePackageType</key>' >>$plistfile
	echo '  <string>APPL</string>' >>$plistfile
	echo '  <key>CFBundleSignature</key>' >>$plistfile
	# mmm, what to put here?
	echo '  <string>BCO#</string>' >>$plistfile
	echo '  <key>CFBundleVersion</key>' >>$plistfile
	echo '  <string>1.0</string>' >>$plistfile
	echo '</dict>' >>$plistfile
	echo '</plist>' >>$plistfile
fi
cp $plistfile $appfolder/Contents/Info.plist

# create pkginfo if it does not exist
if [ ! -e PkgInfo ]
then
  echo $PkgInfoContents > PkgInfo
fi
cp -f PkgInfo $appfolder/Contents/

# copy over any help files
cp -f help/* $appfolder/Contents/Resources/LazHelp/

# copy executable over and strip debug info
cp -f $appfile $appfolder/Contents/MacOS
strip $appfolder/Contents/MacOS/$appfile

# any bmp files - there are none right now
#cp *.bmp $appfolder/Contents/Resources/

# icon:
cp -f $appfile.ico $appfolder/Contents/Resources/

# mac specific icons?
# must match the one in info.plist
cp -f osxicon.icns $appfolder/Contents/Resources
cp -f osxreadme.rtf $tgt/Readme.rtf
# set file hidden
SetFile -a E $tgt/Readme.rtf

echo -- Removing DS_Store files
find $tgt -name ".DS_Store" -depth -exec rm {} \;

echo -- Creating DMG
#-ov: overwrite existing files
#UDBZ - UDIF bzip2-compressed image (OS X 10.4+ only)
hdiutil create $tgt/$appfile.dmg -srcfolder $tgt -ov -volname $appfile -format UDBZ

echo -- Removing any duplicate app bundle
echo ------
# may have been created by Lazarus compilation; remove to minimize confusion
rm -fr $appfile.app

echo ------
echo Build of Release in $tgt done successfully
echo ------
