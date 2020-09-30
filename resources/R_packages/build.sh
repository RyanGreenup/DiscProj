#!/bin/bash
# abort on nonzero exitstatus
set -o errexit
# abort on unbound variable
set -o nounset
# don't hide errors within pipes
set -o pipefail

readonly script_name=$(basename "${0}")
         script_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd ) # this had a problem maybe?
readonly script_dir=$(realpath "${script_dir}""/""${script_name}" | xargs dirname)
cd "${script_dir}"

## Program


### Description
# This will print 'Hello World' to the STDOUT.

### Code
main () {
#     make_docs
#     installPackages
Rscript ./build.R
Rscript ./build.R
}

## I just redid this inside R
make_docs () {
    for dir in $(find ./ -maxdepth 1 -type d | grep '[^./]'); do
	cd "${dir}"
	R -e 'library(devtools); library(roxygen2); document()'
    done
}

installPackages () {
    for dir in $(find ./ -maxdepth 1 -type d | grep '[^./]'); do
	installR "${dir}"
    done
}

installR () {

    if [[ -d "${1}" ]]; then
        R -e "library(devtools); install(\"${1}\")" && echo "Installed ${1}" || echo "failed on ${1}"
    else
        echo"No Directory with that name, R Packages must be a directory"
        ls
    fi

    }


# No arguments
main

## vim:fdm=expr:fdl=0
## vim:fde=getline(v\:lnum)=~'^##'?'>'.(matchend(getline(v\:lnum),'##*')-2)\:'='

