# What's SnailScript

SnailScript is a programing language similar to shell script.
The base syntax is taken from sh or rc shell.
There are several new syntax to import the concept of functional programing. 

## Features of SnailScript

* Lexical scope
* First-class function
* Non-numeric and multiple return values
* Pipeline using return values
* Immutable data structure

## Comparison with sh and rc shell
### Same syntax as sh or rc shell

* Execution of command or function
  ```
  @ mkdir tmp
  @ cd tmp
  @ touch a b c
  @ ls .
  a b c
  ```
* Refer valiable, positional parameter and return value.
  ```
  @ echo $a
  a
  @ echo $1
  1
  @ echo $@
  1 2 3
  @ echo $*
  1 2 3
  @ echo $?
  0
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
* Quoting
  ```
  @ echo "hello world"
  hello world
  @ echo "$a"
  $a
  @ echo """hello world"""
  "hello world"
  ```
  Single, double, back quotes have same meaning.

### Different syntax from sh and rc shell

* Redirection with copying fd
  ```
  @ {echo a; ech} >[2=1] a
  @ cat a
  a
  ech: snale: runInteractiveProcess: exec: does not exist (No such file or directory)
  @ {echo a; ech} >[1=2] /dev/null
  @ {echo a; ech} >[1<>2] | grep echo
  a
  ```
  "<>" swaps output and error output.

* Pipe with copying fd
  ```
  @ {echo a; ech} |[2=1] grep -w a
  a
  ```

* Variable
  - Definition
    ```
    @ let a b
    @ echo $a
    b
    @ letr a {echo a | read}
    @ echo $a
    a
    ```
  - Scope  
    Variables in the closure refer the values at the time of the closure creation.
    ```
    @ let a b
    @ let f {echo $a}
    @ let a c
    @ $f
    b
    ```
  - Re-definition  
    The value of variable is immutable. So it can not be updated.
    But it can be defined again as another variable.
  
* Arithmetic expression  
  Arithmetic expression can be used in parens.
  ```
  @ (1+1)
  @ echo $?
  2
  ```
  To use positional parameter like functon,
  "@" has to be add at left paren.
  ```
  @ @($1+$2) 1 1
  @ echo $?
  2
  ```
  Arithmetic expression in the position of arguments
  expands like arithmetic expansion.
  ```
  @ echo (1+1)
  2
  ```
  String comparison can be use in arithmetic expansion.
  ```
  @ (a~a) && echo $?
  a
  @ (ab~[aA]) && echo $?
  a
  @ ([a b c]~[a b c]) && echo $@?
  a b c
  ```
  String formatting can be use in arithmetic expansion.
  ```
  @ echo (%8s Hello)
     Hello
  @ echo (%.2s Hello)
  He
  @ echo (%06d 1234)
  001234
  @ echo (%6.3f 3.14)
   3.140
  ```
  
* Command substitution  
  Snale does not have command substitution syntax.
  Instead, read builtin command returns string.
  ```
  @ echo a | read
  @ echo $?
  a
  ```
  
* Function
  - Definition
    ```
    @ def func1 @{echo $1}
    @ def func2 @($1+$2)
    @ def cont hoge
    ```
  - Return Values
    ```
    @ def multi-values @{: $@}
    @ multi-values 1 2 3
    @ echo $?1 $?2 $?3
    1 2 3
    ```
  - Type  
    There are only two type in function.
    - Pure function  
      It is defined by arithmetic expression or constant.
      Pure function can be used in arithmetic expression.
      ```
      @ def func2 @($1+$2)
      @ echo (func2 1 1)
      2
      ```
      To referring constant, "@" has to be add in begining of function name.
      ```
      @ def cont hoge
      @ (@cont==hoge) && echo a
      a
      @ echo @cont
      hoge
      ```
    - Non pure function  
      It is defined by brace.
      It can not be used in arithmetic expression.
      
* Flow-Control Constructs
  - Conditional branch
    ```
    @ {(a==a) &&& echo a; echo b}
    a
    @ {(a==b) ||| echo b; echo a}
    b
    ```
    When arguments are passed to arithmetic expression (not having "@"),
    it works like ternary operator.
    ```
    @ (! a==a) a b
    @ echo $?
    b
    ``` 
  - Loop
    ```
    @ loop @{($1>=5) >>> break; echo $1; ($1+1)} 1
    1
    2
    3
    4
    ```
  - Non-local Exits
    ```
    @ catch {
        let break {break}
        loop {
          echo a
          catch {$break}
        }
        echo b
      }
    a
    ```
    
* Globing
  ```
  @ mkdir tmp; cd tmp; touch a b c
  @ glob *
  @ echo $@?
  a b c
  ```
  
* Data Stuructures
  - List
    ```
    @ [1 2 3 4] 1; echo $?
    1
    @ [1 2 3 4] -2 -1; echo $@?
    3 4
    ```
  - Associative array
    ```
    @ {dict a 1 b 2 c 3} a; echo $?
    1
    @ {dict a 1 b 2 c 3} b c; echo $?
    b 2 c 3
    ```
    
* Pipeline with return values
  - Case: pass all values.
    ```
    @ : 1 2 3 $> echo
    1 2 3
    ```
    If no value, finish pipeline and do exception process.
    ```
    @ : 1 2 3 $$ echo !! echo b 
    1 2 3
    @ : $$ echo !! echo b
    b
    ```
    If no value or error status, finish pipeline and do exception process.
    ```
    @ true 1 2 3 && $$ echo |! echo b
    1 2 3
    @ false 1 2 3 && $$ echo |! echo b
    b
    @ true && $$ echo |! echo b
    b
    ```
  - Case: pass first values.  
    If no value, finish pipeline and do exception process.
    ```
    @ : 1 2 3 $ echo !! echo b
    1
    @ : $ echo !! echo b
    b
    ```
    If no value or error status, finish pipeline and do exception process.
    ```
    @ true 1 2 3 $& echo |! echo b
    1
    @ false 1 2 3 $& echo |! echo b
    b
    @ true $& echo |! echo b
    b
    ```
  - Case: process each value.
    ```
    @ : 1 2 3 $: echo
    1
    2
    3
    ```

## Built-in functions
* print
* true
* false
* :
* loop
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
* usage

