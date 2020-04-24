export ZSH=${HOME}/.oh-my-zsh
export DOTFILESDIR="${HOME}/.dotfiles"
export SSH_KEY_PATH="~/git/config/ssh/dsa_id"
export EDITOR='emacsclient'
export VISUAL=${EDITOR}
export PATH="${HOME}/.rbenv/shims:${HOME}/.cargo/bin:/opt/npm/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:${HOME}/dev:/usr/local/opt/go/libexec/bin:${HOME}/.toolbox/bin"

# So I can use these from emacs
source ${DOTFILESDIR}/dockerfunc
source ${DOTFILESDIR}/functions
