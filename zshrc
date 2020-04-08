autoload -Uz compinit promptinit
compinit
promptinit

prompt walters

if [ -d $HOME/.zshrc.d ]
then
    for file ($HOME/.zshrc.d/*) source $file
fi
