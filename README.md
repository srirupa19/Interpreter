# Interpreter in Haskell

A simple interpreter written in Haskell for a Python like language. Currently, only space seperated terminals work.

-----
</br>

### Examples of Valid Expressions
``` 
    1.  
        { 
            let num = 5 ; 

            if ( num > 3 ) 
                then 
                { 
                    let num = 6 ; 
                } 
                else 
                { 
                    print num ; 
                } 
        }

    Output : num 6


    2.  
        { 
            let i = 5 ;

            /* First_while! */
            while ( i != 0 ) 
            { 
                print i ; 
                let i = i - 1 ; 
            }

        }

    Output : i 0
             print 5 4 3 2 1 
```

Note that atleast one blank character is mandatory even after ; and braces, the text inside the comment must not be space seperated and indentations do not matter.

----
</br>

### Currently Unsupported Features
```
    1.  Logical operators like & | ^
    2.  Space seperated comments
    3.  Strings and characters
    4.  Space insensitive langauage
```

----
</br>

### Grammar used for Interpreter
</br>

    Block 
        : { Part }

    Part 
        : Statement Part
        | IfStatement Part
        | WhileLoop Part
        | Comment Part
        | epsilon

    Comment 
        : */ string */

    Statement 
        : let var = ArithExpr ;
        | print ArithExpr ;

    IfStatement
        : if ( BoolExpr ) then Block else Block

    WhileLoop
        : while ( BoolExpr ) Block 

    BoolExpr 
        : True
        | False
        | ArithBoolExpr

    ArithBoolExpr
        : ArithExpr > ArithExpr
        | ArithExpr < ArithExpr
        | ArithExpr == ArithExpr
        | ArithExpr != ArithExpr

    ArithExpr 
        : FactorExpr Expr

    Expr 
        : FactorExpr + Expr
        | FactorExpr - Expr
        | epsilon

    FactorExpr 
        : Numbers HiExpr

    HiExpr 
        : Numbers * HiExpr
        | Numbers / HiExpr
        | Numbers % HiExpr
        | epsilon 

    Numbers 
        : int
        | ( ArithExpr )
        | var

----
</br>

### How to Run
```
$ ghc -o a interpreter.hs
$ ./a
```
