# Code
local ret_status="%(?:%{$fg_bold[red]%}%n:%{$fg[red]%}%? %{$fg_bold[red]%}%n%s)"
# PROMPT='${ret_status}%{$fg_bold[green]%}%p %{$fg_bold[yellow]%}%M %{$fg_bold[green]%}%c %{$fg_bold[blue]%}$(git_prompt_info)%{$reset_color%}$ '
PROMPT='%{$fg_bold[green]%}%n@%M %{$fg_bold[blue]%}%c %{$fg_bold[blue]%}$(git_prompt_info)%{$reset_color%}$ '

ZSH_THEME_GIT_PROMPT_PREFIX="git:(%{$fg[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗ %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}) "
