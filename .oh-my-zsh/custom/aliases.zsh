# Aliases
# Just use .zshrc...
# alias cd..='cd ..'
# alias colors='~/bin/colors'
# alias cp="cp -i"                          # confirm before overwriting something
# alias df='df -h'
# alias free='free -h'
# alias g='git'
# #alias grep='grep --color=auto -d skip'
# alias l='ls'
# #alias ls='ls --color=auto --group-directories-first'
# #alias ll='ls -l -h'
# #alias la='ls -la'
# alias tmux="TERM=screen-256color-bce tmux"
# alias rm="rm -i"                          # confirm before overwriting something
# alias v='vim'
# alias vi='vim'
# alias view='vim -R'

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

# man - colored manpage
# source: ArchWiki "man page"
# man() {
# 	env LESS_TERMCAP_mb=$'\E[01;31m' \
# 		LESS_TERMCAP_md=$'\E[01;38;5;74m' \
# 		LESS_TERMCAP_me=$'\E[0m' \
# 		LESS_TERMCAP_se=$'\E[0m' \
# 		LESS_TERMCAP_so=$'\E[38;5;246m' \
# 		LESS_TERMCAP_ue=$'\E[0m' \
# 		LESS_TERMCAP_us=$'\E[04;38;5;146m' \
# 		man "$@"
# }

# vim:ft=sh
