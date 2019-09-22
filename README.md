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
  To use positional parameter like function,
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
  String comparison can be use in arithmetic expression.
  ```
  @ (a~a) && echo $?
  a
  @ (ab~[aA]) && echo $?
  a
  @ ([a b c]~[a b c]) && echo $@?
  a b c
  ```
  String formatting can be use in arithmetic expression.
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
  "||" operator can use to specify default value.
  ```
  @ echo $not_bind
  Getting an unbound variable: "not_bind"
  @ echo ($not_bind || a)
  a
  @ echo (#[a 1 b 2] c || x)
  x
  ```
  
* Command substitution  
  Snale does not have command substitution syntax.
  Instead, read builtin command returns string.
  ```
  @ yes | head -n3 | read -a
  @ echo $?
  y
  y
  y
  ```
  
* Function
  - Definition
    ```
    @ def func1 @{echo $1}
    @ def func2 @($1+$2)
    @ def cont hoge
    ```
    In script (not interactive), multiple definition is not available. 
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
    @ #[a 1 b 2 c 3] a; echo $?
    1
    @ #[a 1 b 2 c 3] b c; echo $?
    b 2 c 3
    ```
  - Chaining process using the data Stuructures
    ```
    @ [[1 2] 3] 1 $. 2 $ echo
    2
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
* echo  
  Unlike sh, any option can not be specified. 
* print  
  It is almost same as echo, but not add newline at last of string. 
  ```
  @ print a
  a@
  ```
* true  
  Unlike sh, it set argument to return values.
  ```
  @ true 1 2 3 && echo T $@? || echo F $@?
  T 1 2 3
  ```
* false  
  Unlike sh, it set argument to return values.
  ``` 
  @ false 1 2 3 && echo T $@? || echo F $@?
  F 1 2 3
  ```
* :  
  This command is not short version of true command.
  It does not change return state.
  ```
  @ false; : 1 2 3 && echo T $@? || echo F $@?
  F 1 2 3
  ```
* ?  
  This command is same as *command $> true*.
  ```
  @ ? false 1 2 3 && echo $@?
  1 2 3
  ```
* loop  
  First argument is closure for iteration. Rest arguments are arguments for first iteration.
  Return values of each iteration are passed to next iteration.
  Returning closure is meaning "continue" in other languege.
  When -n option is specified, this command does not catch exit by break command.
  ```
  @ loop {echo a; loop -n {break}; echo b}
  a
  ```
* catch  
  This command makes tag to jump.
  break command can be used to Non-local Exits in the closure being passed to this command.
* trap  
  Unlike sh, this command take closure as signal handler.
  When -a option is specified, this command add argument closure to existing signal handler.
  ```
  @ trap {rm file2} EXIT
  @ trap -a {rm file2} EXIT
  ```
* let  
  It binds value to valiable. For List, it works recursively.
  When the shape is not same between variable name and value list,
  it retuens false state.
  ```
  @ let a b 1; echo $a $b
  1 1
  @ let [a b] [1 2] && {echo $a; echo $b}
  1
  2
  @ let [a b] 1 && {echo $a} 
  @
  ```
* letr  
  It binds return values of the argument closure to valiables.
  When number of variable names are more then return values,
  it retuens false state.
  ```
  @ letr a {: 1 2}; echo $a
  1
  @ letr a b {: 1 2}; echo $a; echo $b
  1
  2
  ```
* def  
  This command defines function or constant.
* load  
  This command executes the argument file in the corrent environment.
  ```
  @ cat a
  def f {echo a}
  @ load ./a
  @ f
  a
  ```
  ```
  @ {load ./a; : @{$@}} $ def name-space
  @ name-space f
  a
  ```
* tmpf  
  This command makes temporary files and pass the files to the closure as arguments.
  When exiting from closure, these files are removed.
  ```
  @ tmpf @{ls $1; : $1}
  /tmp/snale2371-1
  @ ls $?
  ls: cannot access '/tmp/snale2371-1': No such file or directory
  ```
* tmpd  
  This command makes temporary directory and pass the files to the closure as argument.
  When exiting from closure, the directory is removed.
  ```
  @ tmpd @{ls -ld $1; : $1}
  drwx------ 2 root root 4096 Sep 21 23:28 /tmp/snale-146e51aa74ac0b46
  @ ls $?
  ls: cannot access '/tmp/snale-146e51aa74ac0b46': No such file or directory
  ```
* check  
  This command has file check parts of test command functions.
* read  
  Unlike sh, this command does not bind input to variable, but put it to return value.
  ```
  @ ? yes | read $ echo
  y
  @ ? yes yes | read -n2 $ echo
  ye
  @ ? yes | read -n2 $ echo
  y
  @ ? yes | read -Nn2 $ echo
  y
  
  @ ? yes yes | read -ss -n3 $ echo
  ye
  ```
* bool  
  It puts the return status and argument values to return values.
  ```
  @ true 1 2 3 $> bool; echo $@?
  true 1 2 3
  ```
* ubool  
  This command is the opposite of bool command.
  ```
  @ true 1 2 3 $> bool $> ubool && echo $@?
  1 2 3
  ```
* list  
  It puts the List of argument values to return value.
  ```
  @ : 1 2 3 $> list $ echo
  1 2 3
  ```
* ulist  
  This command is the opposite of list command.
  ```
  @ [1 2 3] $ ulist $> echo
  1 2 3
  ```
* dict  
  It make the associative array from argument values.
  ```
  @ : a 1 b 2 c 3 $> dict $.b $ echo
  2
  ```
* udict  
  This command is the opposite of dict command.
  ```
  @ #[a 1 b 2 c 3] $ udict $> echo
  a 1 b 2 c 3
  ```
* ins  
  It inserts values to List or associative array specified to argument.
  ```
  @ ins 2 a [1 2 3] $ echo
  1 a 2 3
  @ ins aa b #[a 1 b 2 c 3] $ echo
  a b aa b b 2 c 3
  @ ins a b #[a 1 b 2 c 3] $ echo
  a b b 2 c 3
  ```
* del  
  It delete values to List or associative array specified to argument.
  ```
  @ del 2 [1 2 3] $ echo
  1 3
  @ del a #[a 1 b 2 c 3] $ echo
  b 2 c 3
  ```
* map  
  When -n option is specified, this command passes specified number values to the closure or command.
  When argument closure or command returns multiple values,
  the return value of this command is the concat of all those return values.
  ```
  @ map -n2 echo [1 2 3 4]
  1 2
  3 4
  ```
* fold  
  When the argument closure takes three or more argument,
  you specify multiple values to initial values. 
  ```
  @ fold @{: ($1+$2) ($2+$3)} 1 2 [3 4 5]
  17 14
  ```
* len  
  This command returns the length of list. 
  ```
  @ len [1 2 3] $ echo
  3
  @ @{len $*} 1 2 3 $ echo
  3
  ```
* lenc  
  This command returns the length of string.
  ```
  @ lenc "123" $ echo
  3
  ```
* sep  
  This commnad is same as split in other languege.
  ```
  @ sep " a b c " $: echo
  a
  b
  c
  @ sep , ",a,b,c" $: echo
  
  a
  b
  c
  ```
* usep  
  This command is the opposite of sep command.
  ```
  @ usep [1 2 3] $ echo
  1 2 3
  @ usep , [1 2 3] $ echo
  1,2,3
  ```
* sub  
  ```
  @ sub "[aA]*" @ snale_SNALE $ echo
  sn@le_SN@LE
  ```
* timeo  
  It works like timeout command.
  ```
  @ timeo 1 {sleep 2; echo a}
  @
  ```
* fork  
  This command executes argument command or closure in background.
  ```
  @ timeo 1 {fork {sleep 2}; echo a}
  a
  ```
* getenv  
  This command puts the environment variable specified at the argument to return value.
  ```
  @ getenv LANG $ echo
  C
  ```
* setenv  
  This command sets the value to the environment variable specified at the argument.
  ```
  @ setenv LANG C
  ```
* type  
  It returns type of argument.
  ```
  @ type [] $ echo
  LIST
  ```
* int  
  ```
  @ int 1.5 $ echo
  1
  @ int -r 1.5 $ echo 
  2
  @ int -r 1.4 $ echo 
  1
  @ int -c 1.4 $ echo
  2
  @ int -f 1.5 $ echo
  1
  ```
* usage  
  This command shows usage the command specified at the argument.
  When no argument is specified, it shows list of all built-in functions.

