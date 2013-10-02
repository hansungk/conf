if [ -f /etc/bash_completion ]; then
	    . /etc/bash_completion
fi
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto'
alias grep='grep --color=auto -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias v='vim'

# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

### USER EDITED AREA
# Input module setup
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

# Vim stuff
export EDITOR=vim
export VISUAL=vim
alias vi=vim

# PS1 customizing
GREEN=$'\e[1;32m'
RED=$'\e[1;31m'
BLUE=$'\e[1;34m'
PURPLE=$'\e[1;35m'
GRAY=$'\e[1;30m'
RS=$'\e[0m'

#OK=$'\342\234\224'		# Check sign
OK='>'
#ERROR=$'\342\234\227'	# X sign
ERROR='x'

get_exit_status() {
	EXIT=$?
	if [ ${EXIT} -eq 0 ] ; then
		echo ''
	else
		echo ${RED}[${EXIT}]' '
	fi
}

# If you use double quotes, all variables will be expanded at this stage and will not be updated (thus $? is always 0)"
# With $'~'(note the dollar sign), all escape sequences EXCEPT \e will not be expanded.
# Use \[ and \] around non-printing sequences (such as color-changers) to prevent the bash display from being garbled up.
export PS1='\[$(get_exit_status)\]\[${GREEN}\]\u \[${RS}\]:: \[${BLUE}\]\w \[${RS}\]:: \[${PURPLE}\]\t\[${GRAY}\]\n> \[${RS}\]'

# Startup scripts
stty -ixon # Disable C-S suspension
