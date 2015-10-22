colorscheme desert
syntax enable
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4

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
nnoremap <space> za "re-map <space> to open/close folds
set foldmethod=indent   " fold based on indent level

"PATHOGEN (make sure to install it)
call pathogen#infect()                      " use pathogen
