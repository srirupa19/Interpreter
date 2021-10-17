# Interpreter in Haskell

A monadic interpreter written in Haskell for a Python like language. 
-----

### Examples of Valid Expressions
``` 
    1.  
        { 
            int num = 5; 
            string str = "First Program"; 
            print(%c, str);

            if (num > 3) 
                then 
                { 
                    int num = 6; 
                } 
                else 
                { 
                    print (%i, num); 
                } 
        }

    Output : print "First Program "
             num 6


    2.  
        { 
            int i = 5;
            bool a = (4 < 3) || 6 != 7;
            print(%b, a);

            # First While! #
            while(i != 0 && a) 
            { 
                print(%i, i); 
                int i = i - 1; 
            }

        }

    Output : a True
             i 0
             print True 5 4 3 2 1 
```

Note that the syntax for print is different based on if it is used to print an integer(print %i), string(print %c) or a boolean value(print %b).

----

### Currently Unsupported Features
```
    1.  Local scopes are not present, everything belongs to global scope
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
        : int var = Expr ;
        | bool var = LogicExpr ;
        | string var = StrExpt ;
        | print(%i, Expr) ;
        | print(%b, LogicExpr) ;
        | print(%c, StrExpr) ;

    StrExpr 
        : Sentences + StrExpr
        | Sentences

    Sentenes
        : string
        | var

    IfStatement
        : if ( LogicExpr ) then Block else Block

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
        | ( LogicExpr )
        | var

    ArithBoolExpr
        : Expr > Expr
        | Expr < Expr
        | Expr == Expr
        | Expr != Expr

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
        | ( ArithExpr )
        | var

----

### How to Run
```
$ ghc -o a interpreter.hs
$ ./a
```
