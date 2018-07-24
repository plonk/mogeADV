(ql:quickload :ltk)

(defpackage ltk-hoge
  (:use :common-lisp :ltk))

(in-package ltk-hoge)

(load "def.lisp")
(load "text.lisp")
(load "hoge.lisp")

(sb-ext:save-lisp-and-die "mogeADV" :toplevel #'hoge :executable t)
