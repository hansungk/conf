" vim: foldmethod=marker
"unlet! skip_defaults_vim
"source $VIMRUNTIME/defaults.vim

call plug#begin('~/.local/share/nvim/plugged')
Plug 'tpope/vim-fugitive'
"Plug 'dag/vim-fish'
"Plug 'octol/vim-cpp-enhanced-highlight'
"Plug 'prabirshrestha/async.vim'
"Plug 'prabirshrestha/vim-lsp'
"Plug 'w0rp/ale'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'autozimu/LanguageClient-neovim', {
"     \ 'branch': 'next',
"     \ 'do': 'bash install.sh',
"     \ }
"Plug 'natebosch/vim-lsc'
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'ziglang/zig.vim'
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
"Plug 'itchyny/lightline.vim'
Plug 'fxn/vim-monochrome'
Plug 'romainl/Apprentice'
Plug 'dracula/vim'
Plug 'skielbasa/vim-material-monokai'
Plug 'sjl/badwolf'
Plug 'rakr/vim-one'
Plug 'tomasr/molokai'
Plug 'chriskempson/base16-vim'
Plug 'jnurmine/Zenburn'
Plug 'croaker/mustang-vim'
Plug 'lifepillar/vim-solarized8'
Plug 'morhetz/gruvbox'
call plug#end()

set autoindent
set cinoptions=:0 " case:
set expandtab
set fillchars+=vert:│
set hlsearch
set ignorecase
set laststatus=1
"set listchars=tab:▸\ ,eol:¬,trail:·
"set listchars=tab:▸\ ,trail:·
"set list
set mouse=a
set modeline
"set nu
set showbreak=↪
set tags=./tags;
set scrolloff=3
set smartcase
set smartindent
set ts=8 sw=4
set undodir=~/.vimdid
set undofile

inoremap jk <Esc>

let mapleader = " "
nnoremap <silent> <leader><space> :Buffers<cr>
nnoremap <silent> <leader>f :Files<cr>
nnoremap <silent> <leader>l :BLines<cr>
nnoremap <leader>g :Rg 
nnoremap <silent> <leader>n :ALENextWrap<cr>
nnoremap <silent> <leader>c :make<cr>
nnoremap <silent> <leader>q :copen<cr>
nnoremap <silent> <leader>/ :noh<cr>
nnoremap =q :cc<cr>
nnoremap ]q :cnext<cr>
nnoremap [q :cprev<cr>

" nnoremap <silent> gd :ALEGoToDefinition<cr>
" nnoremap <silent> gr :ALEFindReferences<cr>
" nnoremap <silent> <M-a> :ALESymbolSearch<cr>
" nnoremap <silent> gh :ALEHover<cr>

nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_references({'includeDeclaration': v:false})<cr>
nnoremap <silent> gR :call LanguageClient#textDocument_rename()<CR>

autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
    \ |   exe "normal! g`\""
    \ | endif

" autocmd BufEnter * call ncm2#enable_for_buffer()
" " IMPORTANT: :help Ncm2PopupOpen for more information
" set completeopt=noinsert,menuone,noselect
" " When the <Enter> key is pressed while the popup menu is visible, it only
" " hides the menu. Use this mapping to close the menu and also start a new
" " line.
" inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

set makeprg=ninja\ -C\ build
" let g:deoplete#enable_at_startup = 1
" call deoplete#custom#option({
"             \ 'auto_complete_delay': 200,
"             \ })
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

cabbr <expr> %% expand('%:p:h')

" let g:lightline = {
"             \ 'colorscheme': 'one',
"             \ }

augroup workaround_fish_issue
    autocmd!
    autocmd DirChanged * let $PWD = v:event.cwd
augroup END

" LSP {{{
let g:LanguageClient_serverCommands = {
    \ 'cpp': ['ccls'],
    \ 'cuda': ['ccls'],
    \ 'objc': ['ccls'],
    \ }

let g:LanguageClient_loadSettings = 1 " Use an absolute configuration path if you want system-wide settings
let g:LanguageClient_settingsPath = '/home/YOUR_USERNAME/.config/nvim/settings.json'
" https://github.com/autozimu/LanguageClient-neovim/issues/379 LSP snippet is not supported
"let g:LanguageClient_hasSnippetSupport = 0

let g:lsp_diagnostics_enabled = 0
let g:lsp_highlights_enabled = 0
let g:lsp_textprop_enabled = 0
let g:lsp_highlight_references_enabled = 0

if executable('ccls')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'ccls',
      \ 'cmd': {server_info->['ccls']},
      \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'compile_commands.json'))},
      \ 'initialization_options': {},
      \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
      \ })
endif

let g:lsc_server_commands = {
    \ 'cpp': {
    \    'command': 'ccls',
    \    'message_hooks': {
    \        'initialize': {
    \            'rootUri': {m, p -> lsc#uri#documentUri(fnamemodify(findfile('compile_commands.json', expand('%:p') . ';'), ':p:h'))}
    \        },
    \    },
    \    'suppress_stderr': v:true,
    \  },
    \}
let g:lsc_auto_map = v:true
let g:lsc_reference_highlights = v:false
" }}}
" ALE {{{
let g:ale_linters = {
\   'cpp': ['ccls'],
\}
"\   'cpp': ['ccls', 'clang', 'clangd', 'clangtidy', 'cppcheck', 'cquery', 'flawfinder', 'gcc'],
"let g:ale_linters_explicit = 1
let g:ale_c_parse_compile_commands = 1
let g:ale_cpp_gcc_options = '-fdiagnostics-color=never'
let g:ale_cpp_clang_options = '-fdiagnostics-color=never'
let g:ale_cpp_clangd_options = '-background-index'

" Only lint after save
"let g:ale_lint_on_text_changed = 'never'
"let g:ale_lint_on_insert_leave = 0
" You can disable this option too
" if you don't want linters to run on opening a file
"let g:ale_lint_on_enter = 0
" }}}
" FZF {{{
command W echo "Did you mean :w?"

" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep(
  \   'git grep --line-number '.shellescape(<q-args>), 0,
   \   { 'dir': systemlist('git rev-parse --show-toplevel')[0] }, <bang>0)

" Customize fzf colors to match your color scheme
" let g:fzf_colors =
" \ { 'fg':      ['fg', 'Normal'],
"   \ 'bg':      ['bg', 'Normal'],
"   \ 'hl':      ['fg', 'Comment'],
"   \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
"   \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
"   \ 'hl+':     ['fg', 'Statement'],
"   \ 'info':    ['fg', 'PreProc'],
"   \ 'border':  ['fg', 'Ignore'],
"   \ 'prompt':  ['fg', 'Conditional'],
"   \ 'pointer': ['fg', 'Exception'],
"   \ 'marker':  ['fg', 'Keyword'],
"   \ 'spinner': ['fg', 'Label'],
"   \ 'header':  ['fg', 'Comment'] }

" }}}

" clang-format
map <C-K> :py3f /usr/lib/llvm/9/share/clang/clang-format.py<cr>
imap <C-K> <c-o>:py3f /usr/lib/llvm/9/share/clang/clang-format.py<cr>

au FileType go set ts=8 sw=8 noexpandtab
au FileType tex set ts=2 sw=2 expandtab

set background=dark
set termguicolors
colo apprentice
" hi Macro           guifg=#ffcfaf gui=bold                     ctermfg=223 cterm=bold
" hi Macro           gui=NONE
highlight MatchParen guifg=none guibg=none