#!/bin/bash

DIR=`dirname $0`
DOT=$DIR/dot

(
    cat $DOT/gitconfig;
    if [ "x$EMAIL" != "x" ]; then
        echo "[user]"
        echo "        email = $EMAIL"
    fi
) > $DOT/gitconfig.local
