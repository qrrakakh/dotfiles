## Environmetal Variable configulation
#
export LESS=MrXE
export PATH=$HOME/local/bin:$PATH
export C_INCLUDE_PATH=$HOME/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH
export LIBRARY_PATH=$HOME/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBRARY_PATH
export EDITOR='emacsclient -nw'

## prompt
#
COLOR_DEFAULT='\e[0m'
COLOR_BLACK='\e[1;30m'
COLOR_RED='\e[1;31m'
COLOR_GREEN='\e[1;32m'
COLOR_YELLOW='\e[1;33m'
COLOR_BLUE='\e[1;34m'
COLOR_MAGENTA='\e[1;35m'
COLOR_CYAN='\e[1;36m'
COLOR_WHITE='\e[1;37m'

if [ -n "${SSH_CONNECTION}" ]; then
    PSCOLOR=${COLOR_RED}
else
    PSCOLOR=${COLOR_BLUE}
fi
PS1="${PSCOLOR}[\u@\H] \w\n%\[\033[0m\] "

unset COLOR_DEFAULT COLOR_RED COLOR_GREEN COLOR_YALLOW COLOR_BLUE\
 COLOR_MAGENTA COLOR_CYAN COLOR_WHITE COLOR_BLACK PSCOLOR

## aliases and functions
#
if [ $TERM == "dumb" ]; then
    #stty -echo nl
    alias ls='ls -F -CF --color=never --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
else
    alias ls='ls -F --show-control-chars --color -CF --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
fi

alias where="command -v"
alias lsc='/bin/ls'
alias la='ls -a'  
alias lf="ls -F"
alias ll='ls -l'

alias du="du -h"
alias df="df -h"

alias screen='screen -U'
alias javac='javac -J-Dfile.encoding=utf8'
alias bye='exit'
alias g++11='g++ --std=c++0x'
alias clang++11='clang++ --std=c++0x'

alias ec='emacsclient -c'
alias en='emacsclient -nw'

function ff { diff $1~ $1 ; }
function ffc { diff -c $1~ $1 ; }
function venv-activate { source $1/bin/activate }
function va { v -a "$@" | more ; }

## Misc.option
#
HISTCONTROL=ignoredups
HISTFILESIZE=50000
HISTSIZE=50000
complete -cf sudo
