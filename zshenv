export EMACS_SERVER_SOCKET="~/.emacs.d/server/server"
export EDITOR="emacsclient -s ${EMACS_SERVER_SOCKET}"
export VISUAL=${EDITOR}

export WORKSPACE=${HOME}/workspace
export GOPATH=${WORKSPACE}
export SOURCES=${WORKSPACE}/src/github.com/latrokles
export WIKIDIR=${SOURCES}/wiki
export WIKIWEB=${SOURCES}/wiki-web
export DOTFILES=${SOURCES}/dotfiles

source ${DOTFILES}/aliases
source ${DOTFILES}/functions

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="/usr/local/opt/openjdk/bin:${PATH}:${WORKSPACE}/bin:${HOME}/.toolbox/bin"
