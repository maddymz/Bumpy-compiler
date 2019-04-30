# SER502-Spring2019-Team11

This repository is for the group project of SER502.
Compiler and Virtual Machine for a Programming Language.

The language is called 'bumpy'. The link of the youtube video is

## System information 
The src (source folder) consists of the following folders - compiler and interpreter.

compiler/- consists of tokeniser.pl and parser.pl files.tokeniser.pl consists of the tokenizer which is used togenerate tokens for .bmy code.parser.pl defines the parser of our language which consumes the tokens to generate the parse tree. The parse tree is stored  which is output to the .bmic file (intermediate code). 
interpreter/- consists of interpreter.pl which takes the input as .bmic file and generates the output for the given .bmy code.


## Instructions for Installation

Requires SWI-Prolog 8.0.2 to be installed on the computer


## Directions to build and run 
1. Download the repository to your local system.
2. Open the SWI-Prolog runtime environment.
3. Consult filename.
4. Consult filename.
5. Run - compileBumpy('<FILEPATH/.bmy>')
6. This will generate an intermediate code file with .bmic extension in the data folder.
7. Run - bumpy('<FILEPATH/.bmic>').

## Tools Used
Tools used for development go here.

## Requirements Addressed
1. Implemented operators and primitive types boolean, var.
2. Support for Assigment statments and evaluation of expressions.
3. if-then-else construct to make decisions.
4. while statement to do iterative execution.
5. Generates intermediate code (parsetree) and saves it to a .bmic file.
6. Interpreter takes .bmic file as input and prints the ouput on the Prolog runtime environment.

## Extra Features Added
1. Single line comments can be made at the begining of the code using @.
2. Reading the user inputs from the console
3. Conditional operators 'and', 'or', 'not' are implemented.
4. Comparision operators ‘>’ | ‘<’ | ‘<=’ | ‘>=’ | ‘==’ are implemented.
5. Print statements can be used as a. "show *printthis*'" 
6. Conditional construct 'If then else' and looping construct 'while do' are implemented.
7. Nesting of multiple 'If then else', multiple 'while do', 'while do' within 'If then else' are implemented.
7. Evaluation of arithmetic and boolean expressions.
8. Boolean expressions can be used in the conditions.
9. Multiple conditions can be checked using and/or.
10. Support of assignment for boolean expressions.


## Restrictions on the code
1. User input needs to be followed by a '.'.
2. Cannot input any string (String functions are not supported).
3. Comments are added only at the top of the program.
4. Language is case sensitive and only supports lowercase identifiers.
5. Declaration of the variables can be done at the start of the block.

## Authors

* **Palak Chugh** - [Githubaccount](https://github.com/pchugh1)
* **Yuti Desai** - [Githubaccount](https://github.com/yrdesai)
* **Bharat Goel** - [Githubaccount](https://github.com/BharatG295)
* **Madhukar Raj** - [Githubaccount](https://github.com/maddymz)


## Link to youtube video

Link to youtube video goes here.
