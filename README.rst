Little Schemer in Racket
========================

This is the code from The Little Schemer written in Racket-flavored
Scheme.

The book is an excellent introduction to thinking recursively in
Scheme, but uses a few old Scheme idioms that are incompatible or
uncommon with modern Schemes. This can make it hard to follow along
with the book.

Some things that changed:

- Some function names can't be made in Racket (there's no mega-+
  symbol, which is used in the book for addition)

- Some functions the book asks to define are already defined in
  Racket. In this case, it is replaced by a bullet-prefix version.
  For example, since Racket already has an "even?" function, I
  used "•even?"

- The book always defines functions like::

    (define myfunc
      (lambda (arg1 arg2) 
        ...))
   
  This works in modern Scheme, but widely preferred is::

    (define myfunc (arg1 arg2)
      ...)

- Instead of using "lambda" for lambda functions, λ is used for brevity

- Tests are added to ensure that the code works 



