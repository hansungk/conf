function yy {
    echo ${PWD} > /tmp/yy_${USER}
    chmod 600 /tmp/yy_${USER}
}

function pp {
    cd $(cat /tmp/yy_${USER})
}
