#!/bin/bash

DIR=`dirname $0`
DOT=$DIR/dot

(cat $DOT/gitconfig; echo "[user]"; echo "        email = $EMAIL") > $DOT/gitconfig.local

