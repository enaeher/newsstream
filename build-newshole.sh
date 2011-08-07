#!/bin/sh

sbcl --no-sysinit <<EOF
	(require 'asdf)
	(load "newshole.asd")
	(require 'newshole)
	(save-lisp-and-die "newshole" :executable t :toplevel 'newshole:start)
EOF
