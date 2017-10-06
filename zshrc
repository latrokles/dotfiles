#
# oh my zsh
#
export ZSH=/Users/duqruben/.oh-my-zsh
ENABLE_CORRECTION="true"
ZSH_THEME="ys"
plugins=(git)

source $ZSH/oh-my-zsh.sh
#

# User configuration
#
DOTFILESDIR="${HOME}/.dotfiles"

# some osx-linux branching here (if this gets too specific, I will split these files)
case `uname` in
    Darwin)
        plugins=(git osx docker)
        RPROMPT='(osx)'
        ;;
    Linux)
        plugins=(git)
        RPROMPT='(linux)'
        ;;
esac

export PATH="/opt/npm/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin"
export SSH_KEY_PATH="~/git/config/ssh/dsa_id"
export EDITOR='nvim'
export VISUAL=${EDITOR}

source ${DOTFILESDIR}/dockerfunc
source ${DOTFILESDIR}/functions
source ${DOTFILESDIR}/aliases
