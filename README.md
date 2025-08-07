# CMSC330 OCaml Interpreter Project

This repository contains a **lexer**, **parser**, and **evaluator** written in **OCaml**.  
It was developed as part of the **CMSC330: Organization of Programming Languages** course at the University of Maryland.

---

## Overview

The project implements a small functional language featuring:

- Let bindings and recursive functions
- Arithmetic, boolean, and string expressions
- Function definitions and applications
- Conditionals and records
- A simple top-level interpreter (mutop)

---

## Part 1: Lexer

The lexer converts a raw input string into a list of tokens.

- Uses regex to parse inputs.
- Supports:
  - Keywords: `let`, `fun`, `if`, `then`, `else`, `rec`, `def`, etc.
  - Operators: `+`, `-`, `*`, `/`, `=`, `<>`, `||`, `&&`, `->`, `^`
  - Values: integers, booleans, strings
  - Identifiers and punctuation: `(`, `)`, `{`, `}`, `;`, `;;`, `.`
  - Variable and Function IDs
- Ignores whitespace

---

## Part 2: Parser

The parser converts a list of tokens into an **Abstract Syntax Tree (AST)** using recursive descent.

### Supported constructs:
- **Let-expressions**:  
  ```ocaml
  let x = 3 in x + 1
  ```
- **Recursive functions**:  
  ```ocaml
  let rec f = fun x -> ...
  ```
- **If-expressions**:  
  ```ocaml
  if cond then exp1 else exp2
  ```
- **Function definitions and application**:  
  ```ocaml
  let f = fun x -> x + 1
  f 3
  ```
- **Records and field access**:  
  ```ocaml
  {a = 1; b = 2}.a
  ```

---

## Part 3: Evaluator:

The evaluator recursively traverses the Abstract Syntax Tree (AST) and computes the final value of an expression using a runtime environment. 
It handles expression evaluation and updates the environment as needed.

- Primitive values are self evaluating
- Identifiers are resolved using the environment
- Type checking for operations and if statements
- Let-bindings extend the environment with new variables and functions
- Function definitions create closures to ensure correct scoping
- Supports recursive functions
- Handles errors: Type errors, Divide by zero errors, Unbound variables, etc.

---

## Academic Integrity

> This project was developed as part of **CMSC330** at the University of Maryland.  
> **Do not copy, share, or submit this code** for any other course or academic assignment.


