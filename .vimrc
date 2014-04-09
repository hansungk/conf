" vim:fdm=marker
" .vimrc
" Author: Stephen Kim <stephen422@gmail.com>
" Source: https://github.com/stephen422/dotfiles.git
" Reference: YouTube "A Whirlwind Tour of my Vimrc" / " http://bitbucket.org/sjl/dotfiles

" PLUGINS {{{

" Vundle {{{
filetype off " required by Vundle
if has('vim_starting')
	set nocompatible

	if has('win32') || has('win64') " running on windows
		set rtp+=~/vimfiles/bundle/vundle/
		set rtp+=$VIM/vimfiles/bundle/vundle/
		call vundle#rc('~/vimfiles/bundle/')
	else
		set rtp+=~/.vim/bundle/vundle/
		call vundle#rc()
	endif
endif

" Internal
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-surround'
Bundle 'kien/ctrlp.vim'
Bundle "tomasr/molokai"
Bundle 'altercation/vim-colors-solarized'
Bundle 'jnurmine/Zenburn'
Bundle 'vim-scripts/paredit.vim'
Bundle 'tpope/vim-fireplace'
Bundle 'scrooloose/nerdtree'
Bundle 'guns/vim-clojure-static'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'eagletmt/neco-ghc'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'vim-scripts/haskell.vim'

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
set hlsearch
set ignorecase
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

"" Attempts to avoid the Esc key
"nnoremap <Tab> <Esc>
"vnoremap <Tab> <Esc>gV
"onoremap <Tab> <Esc>
"inoremap <Tab> <Esc>
"inoremap <Leader><Tab> <Tab>
"vnoremap kj <Esc>
"cnoremap kj <Esc>
"onoremap kj <Esc>
"inoremap kj <Esc>

"" Save, Open, Close
nmap <C-S>		:w<CR>
imap <C-S>		<Esc>:w<CR>
nmap <C-Q>		:q<CR>
imap <C-Q>		<Esc>:q<CR>
imap <Leader>w	<Esc>:w<CR>a
nmap <Leader>w	:w<CR>
nmap <Leader>q	:q<CR>

"" Substitution
nnoremap S <Esc>:%s/<C-r><C-w>/

"" Folding
nnoremap <Space> za
nnoremap <S-Space> zA
" Fold everything except where the cursor is located, and center it
nnoremap <Leader>z zMzvzz

"" Convenience
nnoremap ; :
nnoremap <tab> %

" New find lines are always at the middle of the window
nnoremap n nzz
nnoremap N Nzz

" Don't jump to next when star-searching
nnoremap * *<C-o>

" 'Strong' h/l
noremap H ^
noremap L g_

" Emacs bindings in insert mode
inoremap <C-f> <Esc>la
inoremap <C-b> <Esc>i
inoremap <C-n> <Esc>ja
inoremap <C-p> <Esc>ka
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A

" Quick resizing
nnoremap <C-left> 5<C-w>>
nnoremap <C-right> 5<C-w><
nnoremap <C-up> 5<C-w>+
nnoremap <C-down> 5<C-w>-

" Quick copying/pasting
nnoremap <Leader>p "0p
vnoremap <C-c> "*y
inoremap <C-v> <Esc>"*pa
" Opening .vimrc
nmap <Leader>v :e ~/.vimrc<CR>
nmap <Leader><S-v> :e ~/dotfiles.git/.vimrc<CR>
" Source ( source: bitbucket.org/sjl/dotfiles/vim/vimrc )
nnoremap <Leader>s :
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" CMake, Run, Errors
"map <F7>		<Esc>:CMake<CR>
map <F8>		<Esc>:make<CR>
"map <S-F8>		<Esc>:call Togglecopen()<CR>
"map <S-F9>		<Esc>:Run<CR>
"map <F9>		<Esc>:Debug<CR><Z>
"map <C-C><C-C>	<Esc>:cc<CR>
"map <C-C><C-N>	<Esc>:cn<CR>
"map <C-C><C-P>	<Esc>:cn<CR>
" NERDTree
map <F4>		<Esc>:NERDTreeToggle<CR>
imap <F4>		<Esc>:NERDTreeToggle<CR>

" }}}

" USER COMMANDS {{{

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

" CONVENIENCE {{{

set showcmd " Show pressed keys
"set laststatus=2 " Always display statusline (especially useful for Powerline)
"set noshowmode " Powerline shows mode instead

"autocmd InsertEnter * set cursorline
"autocmd InsertLeave * set nocursorline

if !has("gui_running")
    " Terminal specific
    set t_Co=256 " For vim terminals
    colorscheme molokai
endif

" }}}
