## Environment variable configuration
export LESS=MrXE
export PATH=$HOME/local/bin:$HOME/.local/bin:$PATH
export C_INCLUDE_PATH=$HOME/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH
export LIBRARY_PATH=$HOME/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$LIBRARY_PATH
export EDITOR='vi'

## Default shell configuration
#
# set prompt
#

## VCS_INFO_GET_data_git slow... skipping once

# autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git 2> /dev/null

# function rprompt-git-current-branch {
#         local name st color gitdir action
#         if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
#                 return
#         fi
#         name=`git branch 2> /dev/null | grep '^\*' | cut -b 3-`
#         if [[ -z $name ]]; then
#                 return
#         fi

#         gitdir=`git rev-parse --git-dir 2> /dev/null`
#         action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

#         st=`git status 2> /dev/null`
#         if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
#                 color=%F{blue}
#         elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
#                 color=%F{orange}
#         elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
#                 color=%B%F{red}
#         else
#                  color=%F{red}
#          fi
              
#         echo "$color$name$action%f%b: "
# }

autoload -U add-zsh-hook
function preexec_hook {
    case "$2" in
        *git*)
            PR_GIT_UPDATE=1
            ;;
    esac    
}
add-zsh-hook preexec preexec_hook

function chpwd_hook {
    PR_GIT_UPDATE=1
}
add-zsh-hook chpwd chpwd_hook

function precmd_hook {
    if [[ -n "$PR_GIT_UPDATE" ]]; then
        update_git_branch_info
    fi
}
add-zsh-hook precmd precmd_hook


function update_git_branch_info {
    local name st color
    git status --porcelain >/dev/null 2>&1
    if [ $? -ne 0 ]; then
        PROMPT_GIT_BRANCH_INFO=""
        return
    fi

    st=`git status 2> /dev/null`
    if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
        color=%F{blue}
    elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
        color=%F{orange}
    elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
        color=%B%F{red}
    else
        color=%F{red}
    fi
    name=$(git branch --show-current 2>/dev/null)
    PROMPT_GIT_BRANCH_INFO="$color$name%f%b: "
}

setopt prompt_subst
## Update prompt as per accepting lines to make the time in the prompt at the accepting time
re-prompt() {
    zle .reset-prompt
    zle .accept-line
}

zle -N accept-line re-prompt

autoload colors
colors
case ${UID} in
0)
    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') %B%{${fg[red]}%}%/#%{${reset_color}%}%b "
    PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
    SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
    ;;
*)
    PROMPT='%{${fg[green]}%}[${USERNAME}@${HOST} %D{%y-%m-%d %H:%M:%S}]%{${reset_color}%} ${PROMPT_GIT_BRANCH_INFO}%{${fg[yellow]}%}%~%{${reset_color}%}
$ '
    #RPROMPT='%D{%Y-%m-%d %H:%M:%S}'
    PROMPT2="%_> "
    SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    ;;
esac

# auto change directory
setopt auto_cd
# auto directory pushd that you can get dirs list by cd -[tab]
setopt auto_pushd
# command correct edition before each completion attempt
setopt correct
# compacked complete list display
setopt list_packed
# no remove postfix slash of command line
setopt noautoremoveslash
# Disable beep
setopt no_beep
# share history between the multi terminal
setopt share_history
# completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
			     /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:default' menu select
zstyle ':completion:*' insert-tab false


## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a gets to line head and Ctrl-e gets
#   to end) and something additions
#
bindkey -e
bindkey "^[[1~" beginning-of-line # Home gets to line head
bindkey "^[[4~" end-of-line # End gets to line end
bindkey "^[[3~" delete-char # Del

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

# reverse menu completion binded to Shift-Tab
#
bindkey "\e[Z" reverse-menu-complete


## Command history configuration
#
HISTFILE=${HOME}/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt hist_ignore_dups     # ignore duplication command history list
#setopt share_history        # share command history data

## Completion configuration
#
#fpath=(${HOME}/.zsh/functions/Completion ${fpath})
autoload -U compinit
compinit

if [[ -n ${ZSH_VERSION-} ]]; then
	autoload -U +X bashcompinit && bashcompinit
fi

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

## zsh editor
#
autoload zed


## Prediction configuration
#
#autoload predict-on
#predict-off

## terminal configuration
#
case "${TERM}" in
screen)
    TERM=xterm
    ;;
esac

case "${TERM}" in
xterm|xterm-color)
    export LSCOLORS=exfxcxdxbxegedabagacad
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
    zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
    ;;
kterm-color)
    stty erase '^H'
    export LSCOLORS=exfxcxdxbxegedabagacad
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
    zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
    ;;
kterm)
    stty erase '^H'
    ;;
cons25)
    unset LANG
    export LSCOLORS=ExFxCxdxBxegedabagacad
    export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
    zstyle ':completion:*' list-colors 'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
    ;;
jfbterm-color)
    export LSCOLORS=gxFxCxdxBxegedabagacad
    export LS_COLORS='di=01;36:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
    zstyle ':completion:*' list-colors 'di=;36;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
    ;;
esac

# set terminal title including current directory
#
case "${TERM}" in
xterm|xterm-color|kterm|kterm-color)
    precmd() {
        echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
    }
    ;;
esac

## Alias configuration
#
# expand aliases before completing
#
setopt complete_aliases     # aliased ls needs if file/dir completions work

# aliases and functions
case "${OSTYPE}" in
freebsd*|darwin*)
        #alias ls="ls -G -w"
        alias ls='ls -F -GF --color --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
        ;;
linux*)
        #alias ls="ls --color"
        alias ls='ls -F -GF --color --hide="\$RECYCLE.BIN" --hide="System Volume Information" --hide="ntuser*" --hide="NTUSER*" --hide="Thumbs.db"'
    ;;
esac

alias where="command -v"
alias lsc='/bin/ls'
alias la='ls -a'  
alias lf="ls -F"
alias ll='ls -l'

alias du="du -h"
alias df="df -h"

alias javac='javac -J-Dfile.encoding=utf8'
alias bye='exit'
alias g++11='g++ --std=c++0x'
alias clang++11='clang++ --std=c++0x'

function ff { diff $1~ $1 ; }
function ffc { diff -c $1~ $1 ; }
function va { v -a "$@" | more ; }

if [ -x `which vim` ]; then
    alias vi='vim'
    alias view="vim -M"
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

# NVM
lazy_load_nvm() {
    unset -f npm node nvm
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
}

npm() {
    lazy_load_nvm
    npm $@
}

node() {
    lazy_load_nvm
    node $@
}

nvm() {
    lazy_load_nvm
    nvm $@
}


## Commandline tools
function command_exist_warning {
    cmd=$1
    if [ -x "`which ${cmd}`" ]; then
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
