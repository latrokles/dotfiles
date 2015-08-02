#!/usr/bin/zsh

create_vim_tree(){
    mkdir -p ${HOME}/.vim/{autoload,bundle}
}

install_pathogen(){
    curl -LSso ${HOME}/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
}

install_plugins(){
    pushd ${HOME}/.vim/bundle
    git clone https://github.com/kien/ctrlp.vim &&
    git clone https://github.com/tpope/vim-surround &&
    git clone https://github.com/Shougo/vimproc.vim &&
    git clone https://github.com/wlangstroth/vim-racket &&
    popd
}

setup_vimrc(){
    pushd ${HOME}
    ln -s ${DOTFILES}/.vimrc .vimrc && popd
}

create_vim_tree && install_pathogen && install_plugins && setup_vimrc
