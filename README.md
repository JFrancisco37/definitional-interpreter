# Interpreter for L2

This repository contains the implementation of an interpreter for the **L2 language**, which includes:

* **Expression evaluator**: Based on the big-step operational semantics defined for L2.
* **Type inference**: Implemented according to the specified type system.

## Supported Syntax

The L2 language extends a basic imperative language with features for memory management, loops, and I/O operations. It supports the following constructs:

* Literal values (`n`, `b`) - Integers and Booleans
* Binary operations (`e1 op e2`) - Arithmetic (`Sum`, `Sub`, `Mul`, `Div`), Relational (`Eq`, `Gt`, `Lt`, `Leq`, `Neq`), and Boolean (`And`, `Or`)
* Conditional structures (`if e1 then e2 else e3`)
* Variables (`x`)
* Declarations (`let x:T = e1 in e2`)
* Assignment (`e1 := e2`) - Requires `e1` to be a reference
* Dereference (`!e1`) - Retrieves the value stored at a reference
* New reference creation (`new e`) - Allocates memory for `e` and returns a reference
* Unit value (`unit`) - Represents the absence of a meaningful value
* While loops (`while e1 do e2`)
* Sequencing (`e1; e2`) - Executes `e1` then `e2`; `e1` must evaluate to `unit`
* Read input (`read`) - Reads an integer from the input stream
* Print output (`print e`) - Prints the integer value of `e` to the output stream
* For loops (`for i = e1 to e2 do e3`) - A syntactic sugar for a `while` loop with an implicit counter.

## Quick Setup Guide

These instructions assume an Ubuntu/Debian-based system.

### Install Dependencies

```bash
# Update package lists
sudo apt update

# Install essential build tools, make, and m4
sudo apt install build-essential make m4

# Install OPAM (OCaml Package Manager)
sudo apt install opam

# Initialize OPAM for your user
opam init

# Add OPAM environment variables to your shell session (add to .bashrc/.zshrc for persistence)
eval $(opam env)

# Create and switch to OCaml version 4.14.0
opam switch create 4.14.0

# Ensure OPAM environment variables are loaded for the new switch
eval $(opam env)

# Install OUnit2 for testing, ocaml-compiler-libs, and ocamlfind
opam install ounit2 ocaml-compiler-libs ocamlfind

# Ensure OPAM environment is loaded
eval $(opam env)

# Compile the interpreter and tests
# The -package and -linkpkg flags tell ocamlfind to include the ounit2 library.
ocamlfind ocamlc -package ounit2 -linkpkg -o tests interpreter.ml tests.ml

# Run the compiled tests
./tests
