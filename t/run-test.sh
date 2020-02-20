#!/bin/sh

if [ ! -e /usr/share/cl-quicklisp/quicklisp.lisp ]; then
    echo "Must install cl-quicklisp to run tests";
    exit 1;
fi

if [ ! -e t/ci.lisp ]; then
    echo "Must run this from top-level";
    exit 1;
fi

export QLDIR=`mktemp -d`/

sbcl --no-userinit \
     --non-interactive \
     --load /usr/share/cl-quicklisp/quicklisp.lisp \
     --eval "(require 'sb-posix)" \
     --eval "(quicklisp-quickstart:install :path (sb-posix:getenv \"QLDIR\"))"

ln -s $PWD $QLDIR/local-projects/

sbcl --load $QLDIR/setup.lisp \
     --script t/ci.lisp

rv=$?

rm -rf $QLDIR

exit $rv
