" vim:fdm=marker
" .vimrc
" Author: Stephen Kim <stephen422@gmail.com>
" Source: https://github.com/stephen422/dotfiles.git
" Reference: YouTube "A Whirlwind Tour of my Vimrc" / " http://bitbucket.org/sjl/dotfiles

" PLUGINS {{{

" Pathogen {{{
"execute pathogen#infect()
"execute pathogen#helptags()
" }}}
" Vundle {{{
filetype off " required by Vundle
let os = substitute(system('uname'), "\n", "", "")
if has('win32') || has('win64') " running on windows
	set rtp+=~/vimfiles/bundle/vundle/
	set rtp+=$VIM/vimfiles/bundle/vundle/
	call vundle#rc('~/vimfiles/bundle/')
else
	set rtp+=~/.vim/bundle/vundle/
	call vundle#rc()
endif

" GitHub repos
Bundle 'gmarik/vundle'
Bundle 'altercation/vim-colors-solarized'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fireplace'
Bundle 'scrooloose/nerdtree'
Bundle 'guns/vim-clojure-static'
Bundle 'Lokaltog/powerline'
" Bundle 'dag/vim2hs'
" Bundle 'kana/vim-filetype-haskell'
" Bundle 'lukerandall/haskellmode-vim'
" vim-scripts repos
" non github repos
"Bundle 'git://git.wincent.com/command-t.git'
" git repos on your local machine (ie. when working on your own plugin)
"Bundle 'file:///Users/gmarik/path/to/plugin'
" }}}
" NERDTree {{{
let NERDTreeQuitOnOpen=1
" }}}
" Haddock {{{
let g:haddock_browser = "/usr/bin/firefox"
" }}}
" Powerline {{{
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim " For powerline
" }}}
" Lilypond {{{
set rtp+=/usr/share/lilypond/2.16.2/vim/ " For lilypond vim mode
" }}}

" }}}

" ESSENTIAL {{{
syntax on
filetype plugin indent on
set autoindent
set autowrite
set encoding=utf-8	" Not sure this will work well
set hlsearch
set listchars=tab:▸\ ,eol:¬,extends:>,precedes:<
set nocompatible
set nu
set splitbelow
set splitright
set title		" Why is this not default?
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
nmap <C-S>		<Esc>:w<CR>
imap <C-S>		<Esc>:w<CR>
nmap <C-Q>		<Esc>:q<CR>
imap <C-Q>		<Esc>:q<CR>
nmap <Leader>w	<Esc>:w<CR>
nmap <Leader>q	<Esc>:q<CR>

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
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A
" Quick resizing
nnoremap <C-left> 5<C-w>>
nnoremap <C-right> 5<C-w><
nnoremap <C-up> 5<C-w>+
nnoremap <C-down> 5<C-w>-
" Quick copying/pasting
vnoremap <C-c> "*y
inoremap <C-v> <Esc>"*pa
" Opening .vimrc
nnoremap <Leader>v :e $MYVIMRC<CR>
" Source ( source: bitbucket.org/sjl/dotfiles/vim/vimrc )
nnoremap <Leader>s :
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" CMake, Run, Errors 
"map <F7>		<Esc>:CMake<CR>
"map <F8>		<Esc>:Make<CR>
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
set laststatus=2 " Always display statusline (especially useful for Powerline)
set noshowmode " Powerline shows mode instead

"autocmd InsertEnter * set cursorline
"autocmd InsertLeave * set nocursorline

if !has("gui_running")
    " Terminal specific
    set t_Co=256 " For vim terminals
    colorscheme default
endif
" }}}
