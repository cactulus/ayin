# Ayin

Compiler for my own programming language

Inspired by Go, Jai and C++

## Installation
### Requirements
premake5 \
Visual Studio (on Windows) \
make (on MacOS and Linux)

### Step by step
#### Windows
> git clone https://github.com/cactulus/ayin.git \
> open terminal and enter command 'premake5 vs2022' \
> open visual studio project and build 'dist' \
> add path of exe to environment path variable (optionally)

#### MacOS & Linux
> git clone https://github.com/cactulus/ayin.git \
> cd ayin \
> sudo sh install.sh

## Compiler Stages
* CLI
* Lexer
* Parser
* AST Lowering / Name and Type Resolving
* IR Lowering / Code Generation

## Backends
* LLVM (main)

Possible backends in the future:
* x64
* ASM (unlikely)
