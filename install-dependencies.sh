#!/bin/bash

srcdir=$(pwd)
_gemdir="$srcdir/gems"
old=$(ruby -e 'print RUBY_VERSION =~ /1.8/ ? 1 : 0')

## Functions ####################################

function Error {

    tput setaf 1
    tput bold
    echo Error: $1 >&2
    tput sgr0
}

function Victory {

    tput setaf 2
    tput bold
    echo $1
    tput sgr0
}

function Bold {

    tput bold
    echo $1
    tput sgr0
}

function Distro {
    local distro="Unknown"
    if [ -e /etc/arch-release ]; then               distro="Arch Linux"
    elif [ -e /etc/debian_version ]; then           distro="Debian"
    elif [ -e /etc/init.d/functions.sh ]; then      distro="Gentoo"    #FIXME: Does it work?
    elif [ -s /etc/slackware-version ]; then        distro="Slackware" #FIXME: Does it work?
    # TBD: Ubuntu
    # TBD: Fedora
    fi
    echo $distro
}

function Packages {
    distro=$*
    packages=""
    if [[ $distro == "Arch Linux" ]]; then packages="xmms2 ruby-glib2 emacs-color-theme emacs ruby rubygems"
    elif [[ $distro == "Debian" ]]; then   packages="xmms2 libglib2-ruby emacs-goodies-el libxmmsclient-ruby"
        # TBD: Ubuntu
        # TBD: Slackware
        # TBD: Gentoo
        # TBD: Fedora
    fi

    echo $packages
}

  ## Packages #####################################
distro=$(Distro)
if [[ $distro == "Unknown" ]]; then
    packages="xmms2 ruby-glib2 emacs-color-theme emacs ruby rubygems"
    Error "Couldn't figure out which distribution you are running"
    Bold "Install the required packages manually and then press any key."
    echo "Look for variations of: "
    for x in $packages; do echo -n "- "; Bold $x; done
    read
else
    Victory "You are running $distro"
    Bold "1. Installing required packages..."
    packages=$(Packages $distro)
    echo $packages
    if [[ $distro == "Arch Linux" ]]; then sudo pacman -S $packages
    elif [[ $distro == "Debian" ]]; then sudo aptitude install $packages
        # TBD: Ubuntu
        # TBD: Slackware
        # TBD: Gentoo
        # TBD: Fedora
    fi
fi

if [[ $old -eq "1" ]]; then
  ## Ruby 1.8 #####################################
    Bold "2. Installing Sexp and Parsec from Rubygems..."
    sudo gem install sexp
    Bold "3. Installing Mechanize from Rubygems..."
    sudo gem install mechanize
    Bold "4. Checking Installation..."
    errors=0
    for x in Sexp Parsec Mechanize; do
        if [ $(gem list | grep -i $x | wc -l) -ne "1" ]; then
            Error "$x was not correctly installed"
            errors=$((errors+1))
        fi
    done

else
  ## Parsec #######################################
    Bold "2. Cloning patched version of Parsec..."
    git clone http://github.com/crnixon/rparsec.git --depth=1
    echo "Building Parsec gem"
    cd rparsec
    gem build rparsec.gemspec
    gem install rparsec-1.0.gem --install-dir $_gemdir
  ## Sexp #########################################
    Bold "3. Installing and patching Sexp..."
    cd $srcdir
    gem install sexp --install-dir $_gemdir
    sed -i '16 s/.*/string = (open >> (escape|other).many << close).map {|strings| strings.join}/' \
        $_gemdir/gems/sexp-0.2.1/sexpressions.rb
  ## Mechanize ####################################
    Bold "4. Installing Mechanize..."
    sudo gem install mechanize
  ## Done #########################################
    Bold "5. Cleaning up..."
    cd $srcdir
    rm -Rf rparsec
  ## Checking #####################################
    Bold "6. Checking installation..."
    errors=0
    if [ $(gem list | grep mechanize | wc -l) -ne "1" ]; then
        Error "Mechanize was not correctly installed"
        errors=$((errors+1))
    fi

    if [ $(ls $_gemdir/gems/sexp-* 2> /dev/null | wc -l) -lt "1" ]; then
        Error "Sexp was not correctly installed"
        errors=$((errors+1))
    fi

    if [ $(ls $_gemdir/gems/rparsec-* 2> /dev/null | wc -l) -lt "1" ]; then
        Error "RParsec was not correctly installed"
        errors=$((errors+1))
    fi
fi

if [ $errors -eq 0 ]; then
  ## Did it go right? #############################
    Victory "Everything was correctly installed!"
else
    exit 1
fi
