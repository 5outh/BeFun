BeFun
=====

A Befunge-93 Language Interpreter

How to use:
<pre>
> ./BeFun -f "befunge_file_name.bf"
</pre>

Example:
<pre>
> ./BeFun -f "../hello_world.bf"
Hello World!
</pre>

Language Specification from http://en.wikipedia.org/wiki/Befunge#Befunge-93_instruction_list :

* 0-9	Push this number on the stack
* +	Addition: Pop a and b, then push a+b
* -	Subtraction: Pop a and b, then push b-a
* \*	Multiplication: Pop a and b, then push a*b
* /	Integer division: Pop a and b, then push b/a, rounded down. If a is zero, ask the user what result they want.
* %	Modulo: Pop a and b, then push the remainder of the integer division of b/a. If a is zero, ask the user what result they want.
* !	Logical NOT: Pop a value. If the value is zero, push 1; otherwise, push zero.
* `	Greater than: Pop a and b, then push 1 if b>a, otherwise zero.
* >	Start moving right
* <	Start moving left
* ^	Start moving up
* v	Start moving down
* ?	Start moving in a random cardinal direction
* _	Pop a value; move right if value=0, left otherwise
* |	Pop a value; move down if value=0, up otherwise
* "	Start string mode: push each character's ASCII value all the way up to the next "
* :	Duplicate value on top of the stack
* \	Swap two values on top of the stack
* $	Pop value from the stack and discard it
* .	Pop value and output as an integer
* ,	Pop value and output as ASCII character
* #	Trampoline: Skip next cell
* p	A "put" call (a way to store a value for later use). Pop y, x and v, then change the character at the position (x,y) in the program to the character with ASCII value v
* g	A "get" call (a way to retrieve data in storage). Pop y and x, then push ASCII value of the character at that position in the program
* &	Ask user for a number and push it
* ~	Ask user for a character and push its ASCII value
* @	End program
* (space)	No-op. Does nothing
