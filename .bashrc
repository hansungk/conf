# Source global rc file first.
if [ -f /etc/bash.bashrc ]; then
	    . /etc/bash.bashrc
fi

if [ -f /etc/bashrc ]; then
	    . /etc/bashrc
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

### USER EDITED AREA
# PATH manipulation
export PATH=/home/stephen/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Vim stuff
export EDITOR=vim
export VISUAL=vim

# Fancy prompt
. /Users/stephen/bin/fancyprompt
