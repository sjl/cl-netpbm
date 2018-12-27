(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :symb
               :with-gensyms
               :transpose

               )
  :package "NETPBM.QUICKUTILS")
