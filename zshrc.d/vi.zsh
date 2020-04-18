bindkey -M viins 'jk' vi-cmd-mode
bindkey '^P' up-history
bindkey '^N' down-history

# backspace and ^h working even after
# returning from command mode
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char

# alt-backspace on vi mode
bindkey '^[^?' backward-kill-word

# prompt for normal mode
function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/[ NORMAL ]}/(main|viins)/}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
