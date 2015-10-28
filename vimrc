colorscheme desert
syntax enable
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
autocmd Filetype ruby setlocal ts=2 sw=2 expandtab

"UI
set number
set showcmd
set cursorline
filetype indent on
set wildmenu
set lazyredraw
set showmatch
set ruler

"SEARCHING
set incsearch
set hlsearch
nnoremap <leader><space> :nohlsearch<CR> ",<space> disables search highlights

"FOLDING
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent   " fold based on indent level

"PATHOGEN (make sure to install it)
call pathogen#infect()                      " use pathogen
