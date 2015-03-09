" Vim indent file
" Language:	Gherkin
" Maintainer:	Chris McCormick <chris@mccormick.cx>
" URL:		 
" Last Change:	2015 Mar 09
" Based on the Hylang indent file by Gregor Best (<gbe@unobtanium.de>), 
" Based on the Lisp indent file by Sergey Khorev (<sergey.khorev@gmail.com>), 
" http://sites.google.com/site/khorser/opensource/vim

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

setlocal ai nosi
setlocal et

setlocal lispwords+=split,getenv,gc,random,load-file
setlocal lisp

let b:undo_indent = "setl ai< si< et< lw< lisp<"
