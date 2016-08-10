#!/bin/sh
# Changes last commit hash to start with d00d

DOOD=""
while [ -z "$DOOD" ]; do
    sleep 1;
    git commit --amend -C HEAD;
    DOOD=$(echo `git show --oneline` | head -c 4 | grep 'd00d');
done
