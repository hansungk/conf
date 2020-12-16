if [ "$(uname)" != "Darwin" ]; then
    alias ls='ls --color=auto'
    alias open='xdg-open'
else
fi
# alias l='exa -l'
# alias ll='ls -lh'
# alias la='ls -alh'
alias cdw='cd ~/src/compiler-base'
alias cdwb='cd ~/src/compiler-base/build'
alias cdresearch='cd ~/berkeley/research'
alias cdasic='cd ~/berkeley/asic-lab'
alias cdsys='cd ~/berkeley/systems'
alias hh='cd /mnt/hardhome'
alias emacs='TERM=screen-24bit emacs -nw'
alias e='emacsclient --no-wait'
alias se='SUDO_EDITOR="emacsclient" sudo -e'
alias g='git'
alias vim='nvim'
alias vi='vim'
alias svim='sudo -E vim'
alias rm='rm -I'
alias cclean='rm -rf CMakeCache.txt CMakeFiles/'
alias xi='xbps-install'
alias xq='xbps-query'
alias sudo='sudo ' # to make aliases work
alias emacs='TERM=screen-24bit emacs -nw'
