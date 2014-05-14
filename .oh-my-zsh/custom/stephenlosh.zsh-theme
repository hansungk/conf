# /|/ Code by Stephen
# /|/ "Hansong" Kim
# 
# name in folder (github)
# ± if in github repo, or ≥ if otherwise Time in 24-hour format is on right.
function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function prompt_char {
    echo -n "%{$fg_bold[grey]%}"
    git branch >/dev/null 2>/dev/null && echo "±%{$reset_color%}" && return
    echo ">%{$reset_color%}"
}

PROMPT='%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}$(git_prompt_info)
$(prompt_char) '

RPROMPT='%{$fg[green]%}[%*]%{$reset_color%}'
RPROMPT=''

ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[magenta]%}!"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[magenta]%}√"
