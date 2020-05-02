if [ "$(uname)" != "Darwin" ]; then
    alias ls='ls --color=auto'
    alias open='xdg-open'
else
fi
alias l='exa -l'
alias ll='ls -l'
alias cdw='cd ~/src/compiler-base'
alias e='emacsclient --no-wait'
alias se='SUDO_EDITOR="emacsclient" sudo -e'
alias g='git'
alias vi='nvim'
alias vim='nvim'
alias svim='sudo -E nvim'
alias rm='rm -i'
alias cclean='rm -rf CMakeCache.txt CMakeFiles/'
alias xi='xbps-install'
alias xq='xbps-query'
alias sudo='sudo ' # to make aliases work
