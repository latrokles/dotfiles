#!/usr/bin/zsh

create_vim_tree(){
    mkdir -p ${HOME}/.vim/{autoload,bundle}
}

install_pathogen(){
    curl -LSso ${HOME}/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
}

install_plugins(){
    pushd ${HOME}/.vim/bundle
    git clone https://github.com/tomasr/molokai &&
    git clone https://github.com/kien/ctrlp.vim &&
    git clone https://github.com/tpope/vim-surround &&
    git clone https://github.com/Shougo/vimproc.vim &&
    git clone https://github.com/wlangstroth/vim-racket &&
    git clone https://github.com/othree/html5.vim &&
    git clone https://github.com/tpope/vim-commentary &&
    git clone https://github.com/groenewege/vim-less &&
    git clone https://github.com/plasticboy/vim-markdown &&
    git clone https://github.com/jiangmiao/auto-pairs &&
    git clone https://github.com/chriskempson/base16-vim &&
    git clone https://github.com/itchyny/lightline.vim &&
    git clone https://github.com/scrooloose/syntastic &&
    git clone https://github.com/mxw/vim-jsx &&
    git clone https://github.com/jaxbot/syntastic-react &&
    git clone https://github.com/justinj/vim-react-snippets &&
    git clone https://github.com/scrooloose/nerdtree &&
    git clone https://github.com/majutsushi/tagbar &&
    git clone https://github.com/fatih/vim-go &&
    popd
}


setup_vimrc(){
    pushd ${HOME}
    ln -s ${DOTFILES}/.vimrc .vimrc && popd
}

create_vim_tree && install_pathogen && install_plugins && setup_vimrc
