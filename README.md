# What's SnailScript

SnailScript is a programing language similar to shell script.
The base syntax is taken from sh or rc shell.
To mix the concept of functional programing, several changes have been added. 

## Features of SnailScript

* Lexical scope
* First-class function
* Non-numeric and multiple return values
* Pipeline using return values
* Immutable data structure

## Comparison with sh
### Same syntax as sh or rc shell

* Execution of command or function
  ```
  @ mkdir tmp
  @ cd tmp
  @ touch a b c
  @ ls .
  a b c
  ```
  
* Short-circuit evaluation
  ```
  @ true && echo a
  a
  @ true || echo a
  @ true && echo a || echo b
  a
  @ false && echo a || echo b
  b
  ```

* Simple pipe
  ```
  @ echo a | grep a
  a
  @ yes | head -n1
  y
  @ ech a |[2] grep echo
  ```

* Simple redirection
  ```
  @ echo a > a
  @ cat - <a
  a
  @ echo a >> a
  @ cat - <a
  a
  a
  @ ech a >[2] a
  @ cat - <a
  ech: snale: runInteractiveProcess: exec: does not exist (No such file or directory)
  @ {echo a; cat - <[1]} > a
  ```

* Non-linear pipe
  ```
  @ diff <{echo a} <{echo b}
  1c1
  < a
  ---
  > b
  @ echo a | tee >{tr a b}
  a
  b
  ```
  
### Different syntax from both
　　
* Redirection with copying fd
  ```
  
  ```

* Pipe with copying fd
  ```
  
  ```

* Variable
  - Definition
    ```
    
    ```
  - Scope
    ```
    
    ```
  - Re-definition
    ```
    
    ```
    
  
* Arithmetic expression
  ```
  
  ```
  
* Command substitution
  ```
  
  ```
  
* Function
  - Definition
    ```
    
    ```
  - Return Values
    ```
    
    ```
  - Type
    ```
    
    ```

* Flow-Control Constructs
  - Conditional branch
    ```
    
    ```
  - Loop
    ```
    
    ```
  - Non-local Exits
    ```
    
    ```
    
* Globing
  ```
  
  ```
  
* Data Stuructures
  - List
    ```
    
    ```
  - Associative array
    ```
    
    ```

## Built-in functions
* echo
* print
* cd
* true
* false
* :
* exit
* loop
* break
* catch
* trap
* let
* letr
* def
* load
* tmpf
* tmpd
* check
* read
* glob
* shift
* bool
* ubool
* list
* ulist
* dict

  - Type
  
  ```
 
  ```
* map
* fold
* filter
* len
* lenc
* sep
* usep
* sub
* timeo
* fork
* getenv
* setenv
* type
* int
* +
* usage


## Combinators

## Combinators in arithmetic expression

## Licence
