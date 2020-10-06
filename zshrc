autoload -Uz compinit promptinit
compinit
promptinit
# Vi mode
# bindkey -v
bindkey -e
bindkey \^U backward-kill-line

if [ -d $HOME/.zshrc.d ]
then
    for file ($HOME/.zshrc.d/*) source $file
fi

# History
HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=10000
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_BEEP

# Bash-style alt-backspace bahavior
autoload -U select-word-style
select-word-style bash

export PATH="$HOME/bin:$PATH"

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

export EDITOR=vim

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
