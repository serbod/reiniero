#!/bin/bash -e
# broth.sh - the mother of all soups
#
# ---
#
# Notes: 
# * bash -e == exits on errors
#
# ---
#
#    Copyright (C) 2008-2011 Puredyne Team
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as
#    published by the Free Software Foundation, version 3 of the license.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


## What are we cooking?
#######################

BUILDER=$(whoami)
BROTH_DIRECTORY=$(pwd)
BROTH_LINUX="linux-image"
BROTH_FLAVOUR="puredyne"
BROTH_ARCH="i386"
BROTH_SOUP="gazpacho"
BROTH_VERSION="1010"
#PARENTBUILD_DIRECTORY="/home/$BUILDER"
#BUILD_DIRECTORY="$PARENTBUILD_DIRECTORY/$BROTH_FLAVOUR-build-$BROTH_ARCH"


## How you like them soups?
###########################
root_in_kitchen()
{
    # We don't want to run broth as a regular user
    if [ "$(whoami)" != "root" ]
    then
        echo "[!] Sorry only root can cook! Bye."
        exit -1
    fi
}

prepare_kitchen()
{
    # Where is the kitchen?
    if [ "$PARENTBUILD_DIRECTORY" == "" ]
    then
        echo "[!] Destination build directory is not set! Bye."
        exit -1
    fi

    # Kernel specific settings
    if [ "${BROTH_ARCH}" == "i386" ]
    then
        BROTH_LINUX_FLAVOUR="liquorix-686"
    elif [ "${BROTH_ARCH}" == "amd64" ]
    then
        BROTH_LINUX_FLAVOUR="liquorix-amd64"
    fi

    # builder specific settings
    if [ "${BOB_THE_BUILDER}" != "" ]
    then
        BUILD_MIRRORS="--mirror-bootstrap \"http://gb.archive.ubuntu.com/ubuntu\" \
        --mirror-chroot \"http://gb.archive.ubuntu.com/ubuntu\" \
        --mirror-chroot-security \"http://security.ubuntu.com/ubuntu\""
    else
        BROTH_SOUP="${BROTH_SOUP} remix"
        BUILD_MIRRORS="--mirror-bootstrap \"http://gb.archive.ubuntu.com/ubuntu\" \
        --mirror-chroot \"http://gb.archive.ubuntu.com/ubuntu\" \
        --mirror-chroot-security \"http://security.ubuntu.com/ubuntu\""
    fi

    # create or clean existing kitchen
    if [ ! -d $BUILD_DIRECTORY ]
    then
        mkdir -p $BUILD_DIRECTORY
	cd $BUILD_DIRECTORY
    else
        cd $BUILD_DIRECTORY
        sudo lb clean
        rm -rf $BUILD_DIRECTORY/config
    fi
}

choose_recipe()
{
    lb config \
	$BUILD_MIRRORS \
	--ignore-system-defaults \
	--verbose \
	--mirror-binary "http://gb.archive.ubuntu.com/ubuntu" \
	--mirror-binary-security "http://security.ubuntu.com/ubuntu" \
	--binary-indices "true" \
	--bootappend-live "persistent preseed/file=/live/image/pure.seed quickreboot" \
	--hostname "$BROTH_FLAVOUR" \
	--iso-application "$BROTH_FLAVOUR" \
	--iso-preparer "live-build VERSION" \
	--iso-publisher "Puredyne team; http://puredyne.org; puredyne-team@goto10.org" \
	--iso-volume "$BROTH_FLAVOUR ${BROTH_SOUP}" \
	--binary-images "iso" \
	--syslinux-splash "config/binary_syslinux/splash.png" \
	--syslinux-timeout "10" \
	--syslinux-menu "true" \
	--username "lintian" \
	--language "en_US.UTF-8" \
	--linux-packages $BROTH_LINUX \
	--linux-flavours $BROTH_LINUX_FLAVOUR \
	--archive-areas "main restricted universe multiverse" \
	--architecture $BROTH_ARCH \
	--mode "ubuntu" \
	--distribution "maverick" \
	--initramfs "live-initramfs" \
	--apt "apt" \
	--apt-recommends "false" \
	--apt-options "--yes --force-yes" \
	--keyring-packages "ubuntu-keyring medibuntu-keyring puredyne-keyring liquorix-keyrings liquorix-keyring liquorix-archive-keyring"
}

secret_ingredient()
{
    # copy the modified config over vanilla config
    cp -r $BROTH_DIRECTORY/stock/* $BUILD_DIRECTORY/config/

    # move common hooks
    HOOKS_ROOT="$BUILD_DIRECTORY/config/chroot_local-hooks"
    rm -rf $HOOKS_ROOT
    mv $HOOKS_ROOT-common $HOOKS_ROOT

    # copy target specific hooks
    if [ -d $HOOKS_ROOT-$PACKAGES_LISTS ]
    then
        cp $HOOKS_ROOT-$PACKAGES_LISTS/* $HOOKS_ROOT
    fi
    
    # copy architecture specific hooks
    if [ -d $HOOKS_ROOT-$BROTH_ARCH ]
    then
        cp $HOOKS_ROOT-$BROTH_ARCH/* $HOOKS_ROOT
    fi

    # choose the "master" package list
    PACKAGES_LISTS_DIR="$BUILD_DIRECTORY/config/chroot_local-packageslists"
    mv $PACKAGES_LISTS_DIR/$PACKAGES_LISTS $PACKAGES_LISTS_DIR/$PACKAGES_LISTS.list
    # append architecture specific packages
    cat $PACKAGES_LISTS_DIR/$PACKAGES_LISTS-$BROTH_ARCH >> $PACKAGES_LISTS_DIR/$PACKAGES_LISTS.list 
}

make_soup()
{
    sudo lb build  2>&1| tee broth.log
}

serve_soup()
{
    if [ -e ${BUILD_DIRECTORY}/binary.iso ]
    then
	RELEASE="${BROTH_FLAVOUR}-${BROTH_VERSION}-${BROTH_SOUP}-${BROTH_MEDIUM}-${BROTH_ARCH}-dev"
	cd ${BUILD_DIRECTORY}
	mv binary.iso ${RELEASE}.iso
	md5sum -b ${RELEASE}.iso > ${RELEASE}.md5
	echo "soup is ready!"
	if [ "${BOB_THE_BUILDER}" != "" ]
        then
	    rsync -P ${RELEASE}.md5 ${BOB_THE_BUILDER}@10.80.80.40::puredyne-iso/${BROTH_SOUP}/
	    rsync -P ${RELEASE}.iso ${BOB_THE_BUILDER}@10.80.80.40::puredyne-iso/${BROTH_SOUP}/
        fi
    fi
}


## Au menu ce soir
##################

usage()
{
cat << EOF
BROTH - The mother of all soups
Copyright (C) 2008-2011  Puredyne Team
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, version 3 of the license.

usage: $0 options
   -h      Show this message
   -d      Destination build directory
   -o      Choose output (CD, DVD or "CUSTOM")
   -a      Choose target architecture (i386|amd64, default:i386)
   -b      Bob the Builder mode

Please check http://en.wikibooks.org/wiki/Puredyne/How_to_broth

EOF
}


if [ "$1" == "" ]; then
    usage ; exit 1
else
    while getopts "ho:a:d:tb:" OPTION ; do
	case $OPTION in
	    h)  usage ; exit 1;;
            o)  OPTARG=`echo $OPTARG | tr '[:lower:]' '[:upper:]'`
		if [ $OPTARG == "CD" -o $OPTARG == "DVD" -o $OPTARG == "CUSTOM" ]
		then
		    BROTH_MEDIUM="$OPTARG"
                    PACKAGES_LISTS="${BROTH_FLAVOUR}-${BROTH_MEDIUM}"
		    echo "starting building of $PACKAGES_LISTS"
		else
                    echo "profile unknown, kthxbye"; exit -1
		fi
		;;
            a)  if [ $OPTARG == "i386" -o $OPTARG == "amd64" ]; then
                    BROTH_ARCH=$OPTARG
                    BUILD_DIRECTORY="$PARENTBUILD_DIRECTORY/$BROTH_FLAVOUR-build-$BROTH_ARCH"
                    echo "building $BROTH_FLAVOUR for $OPTARG"
                else
                    echo "architecture unknown, kthxbye"; exit -1
                fi
                ;;
	    d)  PARENTBUILD_DIRECTORY=$OPTARG 
		BUILD_DIRECTORY="$PARENTBUILD_DIRECTORY/$BROTH_FLAVOUR-build-$BROTH_ARCH"
		echo "parent build directory set to $PARENTBUILD_DIRECTORY"
		;;
	    t)  TMPFS=1 # WIP, INACTIVE NOW
		echo "enabling tmpfs, hit CTRL-C if you do not know what you're doing"
		sleep 5s
		;;
            b)  BOB_THE_BUILDER=$OPTARG
                echo "BOB THE BUILDER MODE!"
                ;;
	    *) echo "Not recognized argument, kthxbye"; exit -1 ;;
	esac
    done
fi


## Finally!
###########

root_in_kitchen
prepare_kitchen
choose_recipe
secret_ingredient
make_soup
serve_soup

