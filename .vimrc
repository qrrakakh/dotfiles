"------ edit/input
set autoindent
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set smartindent
"------ search
set incsearch
set ignorecase
set smartcase
set wrapscan
set list
set whichwrap=b,s,h,l,<,>,[,]
"------ display
set listchars=eol:$,tab:>\ ,extends:<
set number
set title
set showmatch
set matchtime=1
syntax on
set ruler
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P
set linespace=0
set showcmd
"------ function
let s:appearance_status=1

command! SwAppear call s:sw_appear()
function! s:sw_appear()
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
