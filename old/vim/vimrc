"Plugin Management with Vundle
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" general
Plugin 'Shougo/vimproc.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'ervandew/supertab'
Plugin 'groenewege/vim-less'
Plugin 'itchyny/lightline.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'plasticboy/vim-markdown'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'

" language support
Plugin 'fatih/vim-go'
Plugin 'jaxbot/syntastic-react'
Plugin 'justinj/vim-react-snippets'
Plugin 'mxw/vim-jsx'
Plugin 'othree/html5.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'vim-ruby/vim-ruby'
Plugin 'wlangstroth/vim-racket'
Plugin 'keith/swift.vim'
Plugin 'elmcast/elm-vim'
Plugin 'raichoo/haskell-vim'

" themes
Plugin 'morhetz/gruvbox.git'
Plugin 'tomasr/molokai'

call vundle#end()

syntax enable
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
autocmd Filetype ruby setlocal ts=2 sw=2 softtabstop=2 expandtab
autocmd Filetype python setlocal ts=2 sw=2 softtabstop=2 expandtab
autocmd Filetype javascript setlocal ts=2 sw=2 shiftwidth=2 softtabstop=2 expandtab
set backspace=indent,eol,start

"DISABLE ARROW KEYS (INSERT MODE)
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

"DISABLE ARROW KEYS (NORMAL MODE)
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

"UI
set number
set showcmd
set cursorline
filetype indent on
set wildmenu
set lazyredraw
set showmatch
set ruler
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#393b40

"SEARCHING
set incsearch
set hlsearch
nnoremap <leader><space> :nohlsearch<CR> ",<space> disables search highlights

"FOLDING
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent   " fold based on indent level

" colorscheme
colorscheme gruvbox
set background=dark    " Setting dark mode
let g:gruvbox_contrast_dark = 'soft'

au BufRead,BufNewFile *.md setlocal textwidth=80
