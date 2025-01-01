"------ file/format
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,sjis,iso-2022-jp,euc-jp
set fileformats=unix,dos
"------ edit/input
set autoread
set autoindent
set cursorline
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set smartindent
"------ search
set hlsearch
set incsearch
set ignorecase
set smartcase
set wrapscan
set list
set whichwrap=b,s,h,l,<,>,[,]
"------ display
set listchars=eol:$,tab:>\ ,extends:<
set number
syntax on
set title
set showmatch
set matchtime=1
set ruler
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P
set linespace=0
set showcmd
"------ color
set termguicolors
colorscheme github-dimmed
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
"highlight LineNr ctermfg=DarkGrey
"highlight CursorLineNr ctermfg=Grey
"------ function
let s:appearance_status=1

function! Toggle_Appearance()

  if s:appearance_status == 0
    set listchars=eol:$,tab:>\ ,extends:<
    set number
    syntax on
  else
    set listchars=
    set nonumber
    syntax off
  endif

  let s:appearance_status=1-s:appearance_status
endfunction
"------ keybind
nnoremap ;; :call Toggle_Appearance()<CR>
