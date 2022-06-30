set nocompatible
filetype off

" Vundle Package Manager
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'
Plugin 'tomasiser/vim-code-dark'
Plugin 'itchyny/calendar.vim'
Plugin 'gerw/vim-latex-suite'
Plugin 'preservim/nerdtree'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'vim-scripts/csv.vim'

call vundle#end()
filetype plugin indent on

set grepprg=grep\ -nH\ $*
let g:airline#extensions#tabline#enabled = 1
let g:rainbow_active = 1
let g:airline_theme='zenburn'
let g:airline_powerline_fonts = 1
let g:airline#extensions#branch#enabled = 1
let g:rainbow_active = 1

let g:tex_flavor = "latex"
let g:Tex_CompileRule_dvi = 'latex --interaction=nonstopmode $*'
let g:Tex_CompileRule_ps = 'dvips -Ppdf -o $*.ps $*.dvi'
let g:Tex_CompileRule_pdf = 'ps2pdf $*.ps'
let g:Tex_FormatDependency_pdf = 'dvi,ps,pdf'
let g:NERDTreeDirArrowExpandable = '❯'
let g:NERDTreeDirArrowCollapsible = '▼'

"Haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

colorscheme codedark
let &t_ut=''
syntax on
filetype plugin indent on
highlight LineNr ctermfg=grey
set sw=2

"Enable mouse in vim
set mouse=a
set ttymouse=xterm2

set number
set smartcase
set tabstop=3
set shiftwidth=3
set expandtab
set incsearch
set hlsearch
set linebreak
set nomodeline
set relativenumber

nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

"map <F5> : !gcc -o main -Wall -std=c99 -pedantic % && ./main
"map <F9> : !pdflatex -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory="_build"  "/home/ben/Documents/Informatik/5.Semester/Projektphase/projektbericht/projektbericht.tex"
