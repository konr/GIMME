#!/bin/bash

# FIXME: Check out if the user is using Ruby 1.8

srcdir=$(pwd)
_gemdir="$srcdir/gems"

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

## Parsec #######################################
Bold "1. Cloning patched version of Parsec..."
git clone http://github.com/crnixon/rparsec.git --depth=1
echo "Building Parsec gem"
cd rparsec
gem build rparsec.gemspec
gem install rparsec-1.0.gem --install-dir $_gemdir
## Sexp #########################################
Bold "2. Installing and patching Sexp..."
cd $srcdir
gem install sexp --install-dir $_gemdir
sed -i '16 s/.*/string = (open >> (escape|other).many << close).map {|strings| strings.join}/' \
    $_gemdir/gems/sexp-0.2.1/sexpressions.rb
## Mechanize ####################################
Bold "3. Installing Mechanize..."
sudo gem install mechanize
## Done #########################################
Bold "4. Cleaning up..."
cd $srcdir
rm -Rf rparsec
# Checking #####################################
Bold "5. Checking installation..."
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

if [ $errors -eq 0 ]; then
    Victory "Everything was correctly installed!"
else
    exit 1
fi
    
