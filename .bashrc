if [ -f /etc/bash.bashrc ]; then
	    . /etc/bash.bashrc
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

# Startup scripts
stty -ixon # Disable C-S suspension
