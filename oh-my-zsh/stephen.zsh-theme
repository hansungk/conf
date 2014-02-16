# /|/ Code by Stephen
# /|/ "Hansong" Kim
function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function prompt_char {
    echo -n "%{$fg_bold[grey]%}"
    git branch >/dev/null 2>/dev/null && echo "±%{$reset_color%}" && return
    echo ">%{$reset_color%}"
}

PROMPT='%{$fg_bold[cyan]%}$(collapse_pwd)%{$reset_color%} $(prompt_char) '
RPROMPT='$(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg_bold[magenta]%}!"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg_bold[magenta]%}√"
