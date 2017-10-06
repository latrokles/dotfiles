DOTFILESDIR="${HOME}/.dotfiles"

# Path to your oh-my-zsh installation.
export ZSH=${HOME}/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="amuse"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

########    User configuration
#####################################
# export MANPATH="/usr/local/man:$MANPATH"
source $ZSH/oh-my-zsh.sh

################
# CUSTOM STUFF #
################

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

# use vim keybindings for the shell, because... why not?
#set -o vi
#bindkey '^R' history-incremental-search-backward # but let's keep familiar search

export PATH="/opt/npm/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin"
export SSH_KEY_PATH="~/git/config/ssh/dsa_id"
export EDITOR='nvim'

source ${DOTFILESDIR}/dockerfunc
source ${DOTFILESDIR}/functions
source ${DOTFILESDIR}/aliases
