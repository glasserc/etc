#!/usr/bin/python

# Script to filter output of 'mr status' to remove some noise.
# My intent is to have all my directories clean and on the master branch unless
# I'm working on something, and since I track 14 repositories, output scrolls 
# off the page if I don't filter it somehow.

# Remove lines that say we're on branch master and remove lines that say
# we're clean. If this leaves a pair of lines that are of the form 

# mr status: [blah]\n
# \n

# then we strip those too.

import re, sys

statusline = None
line = True
while line:
    line = sys.stdin.readline()
    if line == '# On branch master\n': continue
    if line == 'nothing to commit (working directory clean)\n': continue
    if line == '': break  # early break when we hit EOF

    if line.startswith('mr status: '):
        statusline = line
        continue
    elif statusline:
        if line == '\n':
            continue
        print statusline,
        statusline = None
    print line,


if statusline:
    print statusline,
