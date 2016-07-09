colorscheme desert
syntax enable
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
autocmd Filetype ruby setlocal ts=2 sw=2 softtabstop=2 expandtab
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

"PATHOGEN (make sure to install it)
call pathogen#infect()                      " use pathogen
