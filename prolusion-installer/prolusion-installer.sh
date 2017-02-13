#!/bin/bash

# Version: $Id$
#
#

# Commentary:
#
#

# Change Log:
#
#

# Code:

## #################################################################
## Helper functions
## #################################################################

prolusion_colortext () {
    echo "[38;5;$2m$1[0m"
}

prolusion_green () {
    echo $2 $(prolusion_colortext "$1" "82")
}

prolusion_red () {
    echo $2 $(prolusion_colortext "$1" "196")
}

prolusion_blue () {
    echo $2 $(prolusion_colortext "$1" "27")
}

prolusion_purple () {
    echo $2 $(prolusion_colortext "$1" "141")
}

prolusion_yellow () {
    echo $2 $(prolusion_colortext "$1" "190")
}

prolusion_cyan () {
    echo $2 $(prolusion_colortext "$1" "45")
}

prolusion_clear () {
    echo "$1"
}

prolusion_install () {

    prolusion_yellow "Cloning the Prolusion GitHub repository..."

    /usr/bin/env git clone $PROLUSION_URL "$PROLUSION_INSTALL_DIR" > /dev/null

    prolusion_yellow "Setting up links ..."

    /usr/bin/env ln -s $PROLUSION_INSTALL_DIR $HOME/.emacs.d
    /usr/bin/env ln -s $PROLUSION_INSTALL_DIR/prolusion-mail/msmtprc $HOME/.msmtprc
    /usr/bin/env ln -s $PROLUSION_INSTALL_DIR/prolusion-mail/mbsyncrc $HOME/.mbsyncrc
    /usr/bin/env ln -s $PROLUSION_INSTALL_DIR/prolusion-shell/bashrc $HOME/.bashrc
    /usr/bin/env ln -s $PROLUSION_INSTALL_DIR/prolusion-shell/bash_profile $HOME/.bash_profile

    if ! [ $? -eq 0 ]
    then
        prolusion_red "A fatal error occurred during Prolusion's installation. Aborting..."
        exit 1
    fi
}

prolusion_usage () {
    prolusion_clear "Usage: $0 [OPTION]"
    prolusion_clear "  -h, --help \t \t \t Display this help and exit"
    prolusion_clear ""
}

## #################################################################
## Command line options
## #################################################################

while [ $# -gt 0 ]
do
    case $1 in
        -h | --help)
            prolusion_usage
            exit 0
            ;;
        *)
            prolusion_red "Unkown option: $1"
            shift 1
            ;;
    esac
done

## #################################################################
## Behavior
## #################################################################

PROLUSION_URL="https://github.com/jwintz/prolusion.git"
PROLUSION_INSTALL_DIR="$HOME/.prolusion.d"

if [ -d "$PROLUSION_INSTALL_DIR" ]
then
    prolusion_red   "You already have Prolusion installed."
    prolusion_clear "You'll need to remove $PROLUSION_INSTALL_DIR if you want to install Prolusion again."
    prolusion_clear "If you want to update your copy of prolusion, run 'git pull origin master' from your prolusion directory"
    exit 1;
fi

prolusion_cyan "Checking to see if git is installed... " -n
if hash git 2>&-
then
    prolusion_green "found."
else
    prolusion_red "not found. Aborting installation!"
    exit 1
fi;

prolusion_cyan "Checking to see if emacs is installed... " -n
if hash emacs 2>&-
then
    prolusion_green "found."
else
    prolusion_red "not found. Aborting installation!"
    exit 1
fi;

prolusion_cyan "Checking to see if cmake is installed... " -n
if hash cmake 2>&-
then
    prolusion_green "found."
else
    prolusion_red "not found. Aborting installation!"
    exit 1
fi;

prolusion_cyan "Checking to see if clang is installed... " -n
if hash clang 2>&-
then
    prolusion_green "found."
else
    prolusion_red "not found. Aborting installation!"
    exit 1
fi;

prolusion_cyan "Checking to see if ack is installed... " -n
if hash ack 2>&-
then
    prolusion_green "found."
else
    prolusion_red "not found. Aborting installation!"
    exit 1
fi;

prolusion_cyan "Checking to see if ag is installed... " -n
if hash ag 2>&-
then
    prolusion_green "found."
else
    prolusion_red "not found. Aborting installation!"
    exit 1
fi;

prolusion_install

#
# prolusion-installer.sh ends here
