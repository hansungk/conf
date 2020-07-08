export PATH="$HOME/build/emacs-27.0.91/bin:$PATH"
if [[ "$(uname)" == "Darwin" ]]; then
    export PATH="$HOME/src/emacs/nextstep/Emacs.app/Contents/MacOS:$PATH"
fi

export PATH="$HOME/.emacs.d/bin:$PATH"
