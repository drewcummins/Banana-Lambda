Banana Lambda is a prototype for a small language and a naive implementation of an interpreter. As it is now, it's possible to manipulate some expressions and define methods. Some control structures exist but are being added. Here's an example of some interaction with the interpreter:

    5 * (3 + 2)^2
    > 125
    x := 10
    > "Assigned AInt 10 to x"
    y := x + 5
    > "Assigned Binary Plus (Atomic \"x\") (AInt 5) to y"
    x * y
    > 150
    x + y + u
    > "25 + u"
    def add( a, b ): return a + b:
    > "Defined add"
    add( 2, add( x, y ) )
    > 27
    add( 5, 3 )!
    > 40320
    if add( 5, 5 ) == x then u := 1 else u := 0 endif
    > "Assigned AInt 1 to u"
