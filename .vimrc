""" PLUGINS
"" Pathogen
"execute pathogen#infect()
"execute pathogen#helptags()

"" Vundle
filetype off " required by Vundle
let os = substitute(system('uname'), "\n", "", "")
if has('win32') || has('win64') " running on windows
	set rtp+=~/vimfiles/bundle/vundle/
	set rtp+=$VIM/vimfiles/bundle/vundle/
	call vundle#rc('$VIM/vimfiles/bundle/')
else
	set rtp+=~/.vim/bundle/vundle/
	call vundle#rc()
endif

" Own Vundle
Bundle 'gmarik/vundle'
" GitHub repos
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

let NERDTreeQuitOnOpen=1
let g:haddock_browser = "/usr/bin/firefox"
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim " For powerline

""" ESSENTIAL
syntax on
filetype plugin indent on
set autoindent
set nocompatible
set noswapfile
set nu
set wildmenu
set backupdir=~/.vim/tmp,.
set directory=~/.vim/tmp,.

""" KEY MAPPING
" Avoid to Esc key
"nnoremap <Tab> <Esc>
"vnoremap <Tab> <Esc>gV
"onoremap <Tab> <Esc>
"inoremap <Tab> <Esc>
"inoremap <Leader><Tab> <Tab>
"vnoremap kj <Esc>
"cnoremap kj <Esc>
"onoremap kj <Esc>
"inoremap kj <Esc>
" Save, Open, Close
nmap <C-S>		<Esc>:w<CR>
imap <C-S>		<Esc>:w<CR>
nmap <C-Q>		<Esc>:q<CR>
imap <C-Q>		<Esc>:q<CR>
nmap <Leader>w	<Esc>:w<CR>
nmap <Leader>q	<Esc>:q<CR>
" Misc
nmap ; :
nmap <Leader>v :e $MYVIMRC<CR>
" CMake, Run, Errors 
map <F7>		<Esc>:CMake<CR>
map <F8>		<Esc>:Make<CR>
map <S-F8>		<Esc>:call Togglecopen()<CR>
map <S-F9>		<Esc>:Run<CR>
map <F9>		<Esc>:Debug<CR><Z>
map <C-C><C-C>	<Esc>:cc<CR>
map <C-C><C-N>	<Esc>:cn<CR>
map <C-C><C-P>	<Esc>:cn<CR>
" NERDTree
map <F4>		<Esc>:NERDTreeToggle<CR>
imap <F4>		<Esc>:NERDTreeToggle<CR>


""" USER COMMANDS
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
call s:updatecmakepaths()

""" CTAGS
set tags+=/home/stephen/.vim/tags/cpp
" Build tags for my own project with Ctrl-F12
map <C-F11> :!ctags -R --sort=yes --c++-kinds=+pl --fields=+iaS --extra=+q .<CR>

""" CONVENIENCE
set title " Why is this not default?
set tabstop=4 " SO PRO
set shiftwidth=4 " SO PRO
set showcmd " Show pressed keys
set laststatus=2 " Always display statusline
set noshowmode " Powerline shows instead

"autocmd InsertEnter * set cursorline
"autocmd InsertLeave * set nocursorline

if has("gui_running")
    " .gvimrc
else
    " Terminal specific
    set t_Co=256 " For vim terminals
    colorscheme default
endif
