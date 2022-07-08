set shell=/bin/zsh " set default shell

" Vim-Plug
call plug#begin()

" Sensible defaults
Plug 'tpope/vim-sensible'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

" git-related 
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

Plug 'tpope/vim-commentary'

Plug 'jpalardy/vim-slime'

Plug 'christoomey/vim-tmux-navigator'

Plug 'lervag/vimtex'

Plug 'tpope/vim-surround'

Plug 'dense-analysis/ale'

Plug 'sheerun/vim-polyglot'

Plug 'arcticicestudio/nord-vim'

call plug#end()

syntax on
filetype indent plugin on
set showmatch
set ts=2
set sts=2
set sw=2
set autoindent
set smarttab
set expandtab
set number
" set relativenumber
" Enable mouse/trackpad scrolling
set mouse=a 
" Wrap text on entire words, not characters
set linebreak
" Enable folding with tab
nnoremap <tab> za
" Make j/k go up or down visual lines, rather than logical lines.
nnoremap j gj
nnoremap k gk
" Always have folds open by default
set foldlevel=99
" Make backspace behave like a usual text editor
set backspace=indent,eol,start

" Exit terminal mode with usual vim bindings
tnoremap <Esc> <C-\><C-n>

" Configure slime to use tmux
let g:slime_target = "tmux"

" Configure slime and ipython to handle nested code
let g:slime_python_ipython=1

" ################### Vimtex ###################
" vimtex<->Skim integration
let g:vimtex_view_method = "zathura"
" let g:vimtex_view_general_viewer = '/Applications/Skim.app/Contents/SharedSupport/displayline'
let g:vimtex_view_general_options = '-r @line @pdf @tex'
let g:vimtex_compiler_progname = 'nvr'

augroup vimtex_mac
    autocmd!
    autocmd User VimtexEventCompileSuccess call UpdateSkim()
augroup END

function! UpdateSkim() abort
    let l:out = b:vimtex.out()
    let l:src_file_path = expand('%:p')
    let l:cmd = [g:vimtex_view_general_viewer, '-r']

    if !empty(system('pgrep Skim'))
    call extend(l:cmd, ['-g'])
    endif

    call jobstart(l:cmd + [line('.'), l:out, l:src_file_path])
endfunction

let g:vimtex_quickfix_ignore_filters = [
            \ 'Underfull',
            \ 'Overfull',
            \ 'Missing "journal"',
            \ 'Command \\small invalid in math mode on input line'
            \]

" Reverse lookup in Skim (requires neovim-remote)
function! SetServerName()
  if has('win32')
    let nvim_server_file = $TEMP . "/curnvimserver.txt"
  else
    let nvim_server_file = "/tmp/curnvimserver.txt"
  endif
  let cmd = printf("echo %s > %s", v:servername, nvim_server_file)
  call system(cmd)
endfunction

augroup vimtex_common
    autocmd!
    autocmd FileType tex call SetServerName()
augroup END

" Don't open quickfix window on warning
" (https://github.com/lervag/vimtex/issues/864)
let g:vimtex_quickfix_open_on_warning = 0

let g:vimtex_overload_DoNoMatchParen = 1
" ################### End Vimtex ###################

colorscheme industry

