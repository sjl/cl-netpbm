(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :symb
               :with-gensyms
               :transpose

               )
  :package "TRIVIAL-PPM.QUICKUTILS")
