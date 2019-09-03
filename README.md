# What's snail script

Snail script is a programing language similar to shell script.
The base syntax is taken from sh or rc shell.
To mix the concept of functional programing, several changes have been added. 

## Features of snail script

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
  ```

* Non-linear pipe
  ```
  
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
  
　関数
　　定義
　　戻り値
　　型
　　スコープ
　分岐
　ループ
　グロビング

その他の仕様
　データ構造
　　リスト
　　連想配列
　jump
　　スコープ
　　多重ループからの脱出

## Built-in functoins

## Combinators

## Combinators in arithmetic expression
