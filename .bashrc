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
COLOR_RESET='\[\033[0m\]'

#if [ -n "${SSH_CONNECTION}" ]; then
#    PSCOLOR=${COLOR_RED}
#else
#    PSCOLOR=${COLOR_BLUE}
#fi
#PS1="${PSCOLOR}[\u@\H \D{%y/%m/%d %H:%M:%S}] \w${COLOR_RESET}\n% "

PS1="[\u@\H \D{%y/%m/%d %H:%M:%S}] \w\n$ "

unset COLOR_DEFAULT COLOR_RED COLOR_GREEN COLOR_YALLOW COLOR_BLUE \
      COLOR_MAGENTA COLOR_CYAN COLOR_WHITE COLOR_BLACK PSCOLOR

## aliases and functions

# ls: hide OS-specific files
if [ "$(uname)" == 'Darwin' ]; then
    alias ls='ls -CF'
elif [ $TERM == "dumb" ]; then
    #stty -echo nl
    alias ls='ls -CF --color=never --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
else
    alias ls='ls -F --show-control-chars --color -CF --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
fi

alias where="command -v"
alias lsc='/bin/ls'
alias la='ls -a'  
alias lf="ls -F"
alias ll='ls -la'

alias du="du -h"
alias df="df -h"

alias screen='screen -U'
alias bye='exit'

function ff { diff $1~ $1 ; }
function ffc { diff -c $1~ $1 ; }
function va { v -a "$@" | more ; }

# Emacs
alias ec='emacsclient -c -a ""'
alias en='emacsclient -nw -a ""'

# Development
alias javac='javac -J-Dfile.encoding=utf8'
alias g++11='g++ --std=c++0x'
alias clang++11='clang++ --std=c++0x'

# screen (for WSL)
export SCREENDIR=${HOME}/.screen

if [ ! -d ${SCREENDIR} ]
then
    rm -f ${SCREENDIR}
    mkdir ${SCREENDIR}
    chmod 700 ${SCREENDIR}
fi

# Python3
function venv-activate { source $1/bin/activate; }
function venv { 
    venv_dir="${1:-venv}"
    get_pip_url="https://bootstrap.pypa.io/get-pip.py"
    pip_required_debian_version=("jessie/sid" "stretch/sid");

    if [ -e /etc/debian_version ]; then
        debian_version=`cat /etc/debian_version`
        if [ 1 -le `echo ${pip_required_debian_version[@]} | grep "${debian_version}" | wc -l` ];then
            python3 -m venv "${venv_dir}" --without-pip
            source "${venv_dir}/bin/activate"
            echo "Setting up pip..."
            curl -L ${get_pip_url} | python3
            deactivate
            return
        fi
    fi
    python3 -m venv "${venv_dir}"
}


# AtCoder
function atc_cc_gen {
  prog_prefix=$1;
  test_count=$2;
  touch ${prog_prefix}.cc;
  for i in `seq 1 ${test_count}`
  do
    touch ${prog_prefix}_${i}.txt;
  done
}

function atc_cc_clean {
  rm -f  ./${prog_prefix}.out 2>/dev/null;
}

function atc_cc { 
  prog_prefix=$1;
  trap 'atc_cc_clean' 1 2 3 15;
  g++ --std=c++0x -Wall ./${prog_prefix}.cc -o ./${prog_prefix}.out;
  if [ "$?" -ne 0 ]; then
     return;
  fi;
  for file in ./${prog_prefix}*.txt
  do 
    echo "---TEST ${file}";
    cat ${file} | ./${prog_prefix}.out;
    echo "----------";
  done
  atc_cc_clean
}



## Misc.option
#
unset HISTCONTROL
HISTTIMEFORMAT='%F %T '
HISTFILESIZE=50000
HISTSIZE=50000
HISTCONTROL=ignoreboth
complete -cf sudo
