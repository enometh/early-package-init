#!/bin/bash
#
#   Time-stamp: <>
#   Touched: Thu Aug 19 15:56:37 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021-2022 Madhu.  All Rights Reserved.
#

# SYNOPSIS: After updating the `bar' package, run this script under
# ~/.emacs.d/elpa/bar-{version}/ to commit the files there on a branch
# named `bar' in ELPA_GIT_DIR where we track point updates to
# different packages on different branches.

: ${ELPA_GIT_DIR:=${HOME}/.emacs.d/elpa-GIT}
export ELPA_GIT_DIR

#export TZ=UTC
#set -x
set -e

# If it does not already exist create ELPA_GIT_DIR with an initial
# empty commit on branch master-foo
if [ ! -d ${ELPA_GIT_DIR} ]; then
    mkdir -pv ${ELPA_GIT_DIR}
    (
	export TZ=UTC;
	cd ${ELPA_GIT_DIR};
	git init;
	d="$(date -d @0)";
	export GIT_AUTHOR_DATE="$d" GIT_COMMITTER_DATE="$d";
	git commit --allow-empty --allow-empty-message -m "";
	git branch -M master-foo
    )
fi

# Get a list of FILES to commit in the current directory
declare -a FILES
#FILES=($(ls | egrep -v '.elc$' | egrep -v 'autoloads.el$'))
FILES=($(find . -path ./.git -prune -o  \! -type d -print | egrep -v '.elc$' | egrep -v 'autoloads.el$'))

i=0
nfiles=${#FILES[@]}
echo nfiles=$nfiles
mintime=0
minindex=-1
while (( $i < $nfiles )) ; do
    time=$( stat -c '%Y' ${FILES[$i]})
    if (( $time < $mintime )); then mintime=$time; minindex=$i; fi
    i=$(($i+1))
done


# Figure out the branch on which to commit. This would be the name of
# the package we are on, but let the user override it for testing.
dir=$(basename $(pwd))
if [ -z "$BRANCH" ]; then
    if [[ $dir =~ (.+)-([0-9._]+) ]]; then
	BRANCH=${BASH_REMATCH[1]}
    fi
fi
if [ -z "$BRANCH" ]; then echo supply branch; exit -1; fi

# If .git does not exist, use `git-new-workdir.sh' to check out a
# temporary empty workdir from ELPA_GIT_DIR with branch BRANCH and
# move the .git directory from the temporary workdir to the current
# directory.  If BRANCH does not exist, it is created at the (presumed
# empty) `master-foo' branch.
#
# If .git exists (we assume it has been created by git-new-workdir.sh)
# just make sure we are on the correct BRANCH.
if [ ! -d .git ]; then
    branch_found=false
    if git --git-dir=${ELPA_GIT_DIR}/.git branch | grep "${BRANCH}$" > /dev/null ; then branch_found=true; fi
    if $branch_found; then
	NOCHECKOUT=y git-new-workdir.sh $ELPA_GIT_DIR /dev/shm/${BRANCH}-tmp
	# NOCHECKOUT=y doesn't checkout out any files, git-update-ref
	# won't cut it.
	echo "ref: refs/heads/${BRANCH}" >  /dev/shm/${BRANCH}-tmp/.git/HEAD
	mv /dev/shm/${BRANCH}-tmp/.git .
    else
	NOCHECKOUT=y git-new-workdir.sh $ELPA_GIT_DIR /dev/shm/${BRANCH}-tmp master-foo
	# should be unnecessary but do it for the cringe:
	echo "ref: refs/heads/master-foo" >  /dev/shm/${BRANCH}-tmp/.git/HEAD
	mv /dev/shm/${BRANCH}-tmp/.git .
	git checkout -b $BRANCH
    fi
    git reset
    rmdir -v /dev/shm/${BRANCH}-tmp
else
    if !  git branch --show-current | grep "${BRANCH}$"; then
	echo "current branch isn't ${BRANCH}"
    fi
fi

# Add files and remove missing files if any
git add ${FILES[@]}
REMFILES="$(git status --short | grep '^ D' | cut -c 4-)"
if [ -n "$REMFILES" ]; then
    git rm $REMFILES
fi

# Commit the files with the date of the latest file
(
    d="$(stat -c '%y' ${FILES[$minindex]})";
    export GIT_AUTHOR_DATE="$d" GIT_COMMITTER_DATE="$d";
    git commit -m "$dir";
)
