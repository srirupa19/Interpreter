# Interpreter in Haskell

A monadic interpreter written in Haskell for a Python like language. 
-----

### Examples of Valid Expressions
``` 
    1.  
        { 
            num = 5; 
            str = "First Program"; 
            print(str);

            if (num > 3)  
                { 
                    num = 6; 
                } 
                else 
                { 
                    print(num); 
                } 
        }

    Output : print "First Program"
             num 6


    2.  
        { 
            i = 5;
            a = (4 < 3) || 6 != 7;
            print(a);

            # First While! #
            while(i != 0 && a) 
            { 
                print(i); 
                i = i - 1; 
            }

        }

    Output : a True
             i 0
             print True 5 4 3 2 1 
```

----

### Grammar used for Interpreter


    Block 
        : { Part }

    Part 
        : Statement Part
        | IfStatement Part
        | WhileLoop Part
        | Comment String Part
        | epsilon

    Statement 
        : var = AllExpr;
        | print( AllExpr );

    AllExpr 
        : Sentences ++ AllExpr
        | Sentences

    Sentenes
        : string
        | LogicExpr

    IfStatement
        : if ( LogicExpr ) Block else Block

    WhileLoop
        : while ( LogicExpr ) Block 

    LogicExpr
        : BoolExpr && LogicExpr
        | BoolExpr || LogicExpr
        | BoolExpr

    BoolExpr 
        : True
        | False
        | ArithBoolExpr

    ArithBoolExpr
        : Expr > Expr
        | Expr < Expr
        | Expr == Expr
        | Expr != Expr
        | Expr

    Expr 
        : HiExpr + Expr
        | HiExpr - Expr
        | HiExpr

    HiExpr 
        : SignExpr * HiExpr
        | SignExpr / HiExpr
        | SignExpr % HiExpr
        | SignExpr 

    SignExpr
        : int
        | ( AllExpr )
        | var

----

### How to Run
```
$ ghc -o a interpreter.hs
$ ./a
```
