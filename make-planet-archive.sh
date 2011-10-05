#!/bin/bash
MAJOR=1
MINOR=4
PROJNAME=whalesong


OLDDIR=`pwd`
mkdir -p tmp
rm -rf tmp/$PROJNAME
echo "Checking out clean repo"
git archive --format=tar --prefix=$PROJNAME/ HEAD | (cd tmp && tar xf -)

## Remove any directories that we don't want as part of the repository.
#cd $OLDDIR/tmp/$PROJNAME/private
#rm -rf sandbox


cd $OLDDIR/tmp

raco planet unlink dyoo $PROJNAME.plt $MAJOR $MINOR
raco planet link dyoo $PROJNAME.plt $MAJOR $MINOR $PROJNAME
echo "Making planet package"
raco planet create $PROJNAME

raco planet unlink dyoo $PROJNAME.plt $MAJOR $MINOR

cd $OLDDIR
cp tmp/$PROJNAME.plt .