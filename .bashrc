export LESS=MrXE
export PATH=$HOME/local/bin:$PATH
export C_INCLUDE_PATH=$HOME/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH
export LIBRARY_PATH=$HOME/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBRARY_PATH
export editor='emacsclient -nw'
if [ $TERM == "dumb" ]; then
    #stty -echo nl
    alias ls='ls -F -CF --color=never --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
else
    alias ls='ls -F --show-control-chars --color -CF --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
fi

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

alias lsc='/bin/ls'
alias la='ls -a'  
alias v='ls -l'
alias screen='screen -U'
alias javac='javac -J-Dfile.encoding=utf8'
alias bye='exit'
alias ec='emacsclient -c'
alias en='emacsclient -nw'
alias g++11='g++ --std=c++0x'
alias clang++11='clang++ --std=c++0x'

function ff { diff $1~ $1 ; }
function ffc { diff -c $1~ $1 ; }
function va { v -a "$@" | more ; }
HISTCONTROL=ignoredups
HISTFILESIZE=5000
HISTSIZE=5000

unset COLOR_DEFAULT COLOR_RED COLOR_GREEN COLOR_YALLOW COLOR_BLUE\
 COLOR_MAGENTA COLOR_CYAN COLOR_WHITE COLOR_BLACK PSCOLOR

if [ -e "viewmessage.txt" ]; then
    less "viewmessage.txt"
fi

if [ -e "$HOME/.bash_path" ]; then
    source $HOME/.bash_path
fi

#if [ "$PS1" ] ; then
#mkdir -p -m 0700 /dev/cgroup/cpu/user/$$ > /dev/null 2>&1
#echo $$ > /dev/cgroup/cpu/user/$$/tasks
#echo "1" > /dev/cgroup/cpu/user/$$/notify_on_relase
#fi
