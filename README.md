# Interpreter in Haskell

A simple interpreter written in Haskell for a Python like language. Currently, only space seperated terminals work.

-----

### Examples of Valid Expressions
``` 
    1.  
        { 
            int num = 5 ; 
            string str = ' First Program ' ; 
            print %s str ;

            if ( num > 3 ) 
                then 
                { 
                    int num = 6 ; 
                } 
                else 
                { 
                    print %i num ; 
                } 
        }

    Output : num 6


    2.  
        { 
            int i = 5 ;
            bool a = 4 < 3 | 6 != 7 ;
            print %b bool:: a ;

            /* First_while! */
            while ( i != 0 & bool:: a ) 
            { 
                print %i i ; 
                int i = i - 1 ; 
            }

        }

    Output : i 0
             print 5 4 3 2 1 
```

Note that atleast one blank character is mandatory even after ; and braces, the text inside the comment must not be space seperated and indentations do not matter. All boolean variables should be referred as bool:: var_name except while assigning values. The syntax for print is different based on if it is used to print an integer(print %i), string(print %s) or a boolean value(print %b).

----

### Currently Unsupported Features
```
    1.  Space seperated comments
    2.  Space insensitive langauage
    3.  Local scopes are not present, everything belongs to global scope
```

----

### Possible Improvements
```
    1. Making the grammar more concise.
    2. Boolean variables without the bool:: prefix.
```
----

### Grammar used for Interpreter


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
        : int var = ArithExpr ;
        | bool var = LogicExpr ;
        | string var = StrExpt ;
        | print %i ArithExpr ;
        | print %b LogicExpr ;
        | print %s StrExpr ;

    StrExpr 
        : Sentences + StrExpr
        | epsilon

    Sentenes
        : "Phrases"
        | var

    Phrases
        : string Phrases
        | epsilon

    IfStatement
        : if ( BoolExpr ) then Block else Block

    WhileLoop
        : while ( BoolExpr ) Block 

    LogicExpr
        : BoolExpr LogicBool

    LogicBool
        : BoolExpr & LogicBool
        | BoolExpr | LogicBool
        | epsilon

    BoolExpr 
        : True
        | False
        | ArithBoolExpr
        | ( LogicExpr )
        | var

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
        : SignExpr HiExpr

    HiExpr 
        : SignExpr * HiExpr
        | SignExpr / HiExpr
        | SignExpr % HiExpr
        | epsilon 

    SignExpr
        : - Numbers
        | Numbers

    Numbers 
        : int
        | ( ArithExpr )
        | var

----

### How to Run
```
$ ghc -o a interpreter.hs
$ ./a
```
