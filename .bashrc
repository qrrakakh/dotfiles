## Identify if bash is intreractive, else return
if [ -z "$PS1" ]; then
    return
fi
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

if [ -x `which vim` ]; then
    alias vi='vim'
    alias view="vim -M"
fi

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

## Commandline tools
function command_exist_warning {
    cmd=$1
    if [ -x "`which ${cmd}`" ] || [ "1" -eq "`alias ${cmd} | wc -l`" ]; then
        rtn=0
    else 
        echo Warning: "${cmd} is not installed." `which ${cmd}`
        rtn=1
    fi
}

### fzf
command_exist_warning fzf
fzf_exist=$rtn
if [ "0" -eq "$fzf_exist" ]; then
    fe() {
        local files
        IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
        [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
    }
    fr() {
        local files
        IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
        [[ -n "$files" ]] && ${EDITOR:-view} "${files[@]}"
    }
    # fbr - checkout git branch (including remote branches)
    fbr() {
        local branches branch
        branches=$(git branch --all | grep -v HEAD) &&
        branch=$(echo "$branches" |
            fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
        git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
    }
    # fcd - cd to selected directory
    fcd() {
        local dir
        dir=$(find ${1:-.} -path '*/\.*' -prune \
            -o -type d -print 2> /dev/null | fzf +m) &&
            cd "$dir"
    }
    fssh() {
        local sshLoginHost
        sshLoginHost=`cat ~/.ssh/config | grep -i ^host | awk '{print $2}' | fzf`

        if [ "$sshLoginHost" = "" ]; then
        # ex) Ctrl-C.
            return 1
        fi
        ssh ${sshLoginHost}
    }
    # tmux attach with fzf
    ta() {
        local session session_count
        if [ -n "$TMUX" ]; then
            maybe_tmux_pid=$(echo $TMUX | cut -d, -f2)
            if [ -e /proc/${maybe_tmux_pid} ]; then
                echo 'sessions should be nested with care, unset $TMUX to force'
                return 1
            fi
        fi
        session_count=`tmux list-sessions | wc -l`
        if [ 0 -eq $session_count ]; then
            tmux new-session -s Default
            return 0
        elif [ 1 -eq $session_count ]; then
            session=`tmux list-sessions | cut -d: -f1`
        else
            session=`tmux list-sessions | fzf | cut -d: -f1`
            if [ "$session" = "" ]; then
            # ex) Ctrl-C.
                return 1
            fi
        fi
        echo 'Attaching to '$session
        tmux attach -t "$session"
    }
fi

### rg
command_exist_warning rg
rg_exist=$rtn
if [ "0" -eq "$rg_exist" ]; then
    command_exist_warning expect
    expect_exist=$rtn
    if [ "0" -eq "$expect_exist" ]; then
        rgl() {
                unbuffer rg $@ | less -R
        }
    fi
fi

### rg+fzf+bat
if [ ! -x "`which batcat`" ] && [ -x "`which bat`" ]; then
    alias batcat=bat
fi
command_exist_warning batcat
bat_exist=$rtn
if [ "0" -eq "$fzf_exist" ] && [ "0" -eq "$rg_exist" ] && [ "0" -eq "$bat_exist" ];then
    rgp() {
        local v list filepath linenum
        v=`rg --line-number --with-filename --field-match-separator ':' "$@" 2>/dev/null\
        | fzf --preview "batcat --color=always {1} --highlight-line {2}" \
        --delimiter : --preview-window=~3,+{2}+3/2`
        if [ "0" -ne $? ]; then
            return
        fi
        list=(${v//:/ })
        filepath=${list[0]}
        linenum=${list[1]}
        (( 
          (${linenum}<13)?(
            linenum_jump=1
          ):(
            linenum_jump=linenum-13
          )
        )) 
        batcat --color=always "${filepath}" --highlight-line ${linenum} --pager=never --style=plain | less -NR +${linenum_jump}
        echo ${filepath}
    }

    fgp() {
        local filepath
        filepath=`find -type f | rg "$@" 2>/dev/null\
        | fzf --preview "batcat --color=always {1}" \
        --preview-window=~3,+{2}+3/2`
        if [ "0" -ne $? ]; then
            return
        fi
        batcat --color=always "${filepath}" --pager=never --style=plain | less -NR
        echo ${filepath}
    }

  alias view='batcat --color=always --pager "less -NR" --style=plain'
fi

## PATH
export PATH=$PATH:~/.local/bin

## Misc.option
#
unset HISTCONTROL
HISTTIMEFORMAT='%F %T '
HISTFILESIZE=50000
HISTSIZE=50000
HISTCONTROL=ignoreboth
complete -cf sudo
