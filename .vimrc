" vim:fdm=marker
" .vimrc
" Author: Stephen Kim <stephen422@gmail.com>
" Repo: https://github.com/stephen422/dotfiles.git
" Reference: YouTube "A Whirlwind Tour of my Vimrc" / " http://bitbucket.org/sjl/dotfiles

" TODO
" Start clean when executed from TTY (no X)

" PLUGINS {{{

" Vundle {{{
filetype off " required by Vundle
if has('vim_starting')
	set nocompatible

	if has('win32') || has('win64') " running on windows
		set rtp+=~/vimfiles/bundle/Vundle.vim/
		set rtp+=$VIM/vimfiles/bundle/Vundle.vim/
		call vundle#begin('~/vimfiles/bundle/')
	else
		set rtp+=~/.vim/bundle/Vundle.vim
		call vundle#begin()
	endif
endif

" Internal
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-surround'
Plugin 'kien/ctrlp.vim'
"Plugin 'bling/vim-airline'
Plugin 'tomasr/molokai'
"Plugin 'altercation/vim-colors-solarized'
Plugin 'jnurmine/Zenburn'
"Plugin 'vim-scripts/paredit.vim'
Plugin 'tpope/vim-fireplace'
Plugin 'scrooloose/nerdtree'
Plugin 'guns/vim-clojure-static'
Plugin 'guns/vim-sexp'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
"Plugin 'eagletmt/ghcmod-vim'
"Plugin 'eagletmt/neco-ghc'
"Plugin 'lukerandall/haskellmode-vim'
Plugin 'vim-scripts/haskell.vim'
Plugin 'wting/rust.vim'

call vundle#end()
filetype plugin indent on	" Required!

" }}}
" NERDTree {{{
let NERDTreeQuitOnOpen=1
" }}}
" CtrlP {{{
" set wildignore=*.pdf,*.jpg,*.png,*.iso,*.dmg,*.dylib,*.so,*.o,*.out
"let g:ctrlp_max_files=1000
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v(Library|lib|Applications|Backups|Music)',
	\ 'file': '\v\.(pdf|so|out)$',
	\ }
" }}}
" Haddock {{{
let g:haddock_browser = "/usr/bin/firefox"
" }}}
" Powerline {{{
"set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim " For powerline
"let g:Powerline_symbols= 'fancy'
" }}}
" Airline {{{
let g:airline_theme= 'dark'
" }}}
" Lilypond {{{
" set rtp+=/usr/share/lilypond/2.16.2/vim/ " For lilypond vim mode
" }}}
" Paredit {{{
set rtp+=~/.vim/bundle/paredit0912/
" }}}

" }}}

" ESSENTIAL {{{

syntax on
filetype plugin indent on
set autoindent
set autowrite
set encoding=utf-8	" Not sure this will work well
"set expandtab
set hlsearch
set ignorecase
set incsearch
set nolist
set listchars=tab:▸\ ,eol:¬,extends:>,precedes:<
set nocompatible
set nu
set smartcase	" Should be used with ignorecase on
set splitbelow
set splitright
set title		" Why is this not default?
set titleold=	" No more Thanks, vim
set visualbell	" Turns off that annoying beeps.
set wildmenu

" Tabs
set tabstop=4
set shiftwidth=4

" Backups
set undodir=~/.vim/tmp/undo,.
set backupdir=~/.vim/tmp/backup,.
set directory=~/.vim/tmp/swap,.
set nobackup
set noswapfile " It's 2013, Vim.

" Automatas
au FocusLost * :wa

" }}}

" KEY MAPPINGS {{{

"" Leader
"let mapleader = "\<Space>"

"" Avoid the Esc monster
"nnoremap <Tab> <Esc>
"vnoremap <Tab> <Esc>gV
"onoremap <Tab> <Esc>
"inoremap <Tab> <Esc>
"inoremap <Leader><Tab> <Tab>
"inoremap kj <Esc>

"" Save, Open, Close
nmap <C-S>		:w<CR>
imap <C-S>		<Esc>:w<CR>
nmap <C-Q>		:q<CR>
imap <C-Q>		<Esc>:q<CR>
imap <Leader>w	<Esc>:w<CR>a
nmap <Leader>w	:w<CR>
nmap <Leader>q	:q<CR>
nmap <Leader>Q	:q!<CR>

"" Substitution
nnoremap <Leader>s :%s/<C-r><C-w>/

"" Folding
"nnoremap <Space> za
"nnoremap <S-Space> zA
" Fold everything except where the cursor is located, and center it
nnoremap <Leader>z zMzvzz

"" Convenience
nnoremap ; :
nnoremap <tab> %

"" Find
" Focus find
nnoremap n nzz
nnoremap N Nzz

" Clear find
nnoremap <Leader>/ :noh<CR>

" Don't jump to next when star-searching
nnoremap * *<C-o>

" 
" 'Strong' h/l
noremap H ^
noremap L g_

" Emacs bindings in insert mode
"inoremap <C-f> <Esc>la
"inoremap <C-b> <Esc>i
"inoremap <C-n> <Esc>ja
"inoremap <C-p> <Esc>ka
"inoremap <C-a> <Esc>I
"inoremap <C-e> <Esc>A

" Quick resizing
nnoremap <C-left> 5<C-w>>
nnoremap <C-right> 5<C-w><
nnoremap <C-up> 5<C-w>+
nnoremap <C-down> 5<C-w>-

" Quick copying/pasting
nnoremap <Leader>p "0p
vnoremap <C-c> "*y
"inoremap <C-v> <Esc>"*pa

" Quick external ex command
nnoremap <Leader>r :!!<CR>

" Opening .vimrc
nmap <Leader>v :e ~/.vimrc<CR>
nmap <Leader><S-v> :e ~/dotfiles.git/.vimrc<CR>

" Source ( source: bitbucket.org/sjl/dotfiles/vim/vimrc )
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" NERDTree
map <F4>		<Esc>:NERDTreeToggle<CR>
imap <F4>		<Esc>:NERDTreeToggle<CR>

" }}}

" FUNCTIONS {{{

command Make call s:make()
command CMake call s:cmake()
command Run call s:run()
command Debug call s:debug()
command -nargs=1 Target let $binary = "<args>"


""" Functions
let $binary = "main"
function! s:updatecmakepaths()
  let g:cmake_build_directory = finddir("build", ";")
  if !empty(g:cmake_build_directory)
    let g:cmake_binary_path = findfile($binary, g:cmake_build_directory."**3") " Takes a LOOOOOOOONG Time
  endif
  let &makeprg='make --directory='.g:cmake_build_directory
endfunction

function! s:make()
  call s:updatecmakepaths()
  make
endfunction

function! s:cmake()
  call s:updatecmakepaths()
  let cmakecmd = "!echo \"Building project in directory ".g:cmake_build_directory." >>>>\" && echo"
  let cmakecmd .= " && cd ".g:cmake_build_directory." && cmake .."
  execute cmakecmd
endfunction

function! s:run()
  call s:updatecmakepaths()
  let runcmd = "!clear && echo \"Running built binary file 'main' >>>>\" && echo"
  let runcmd .= " && ".g:cmake_binary_path
  let runcmd .= " && echo && echo \"[TERMINATED AT ".strftime("%c")."]\""
  execute runcmd
endfunction

function! s:debug()
  call s:updatecmakepaths()
  execute "!konsole -e gdb ".g:cmake_binary_path
endfunction

let s:iscopen = 0
function! Togglecopen()
  if !s:iscopen
    copen
    let s:iscopen = 1
  else
    cclose
    let s:iscopen = 0
  endif
endfunction

" Startup calls
" call s:updatecmakepaths()

" }}}

" AUTOCOMMANDS {{{

au BufNewFile,BufRead *.md set filetype=markdown

" }}}

" CONVENIENCE {{{

set showcmd " Show pressed keys

set laststatus=2 " Always display statusline (especially useful for Powerline)
"set noshowmode " Powerline shows mode instead

"autocmd InsertEnter * set cursorline
"autocmd InsertLeave * set nocursorline

if !has("gui_running")
    " Terminal specific
    set t_Co=256 " For vim terminals
    colorscheme zenburn
endif

" }}}
