#
# oh my zsh
#
ENABLE_CORRECTION="true"
ZSH_THEME="ys"
plugins=(git)
source $ZSH/oh-my-zsh.sh

#
# User configuration
#

source ${DOTFILESDIR}/aliases

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
