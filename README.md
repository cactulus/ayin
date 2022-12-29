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

## Examples

### Basic
```c++

/* include standard library */
#use "libc"

/* include other file */
#include "other-file.ay

/* conditionals at compile time */
/* #if <condition> <statement>
 * if platform is windows, then include the following,
 * else include the following */
#if windows ... #else ...
*/

/* extern functions, varags */
extern func printf(fmt: *u8, ...) s32;

/* void functions */
func test() void {}
func test2() {}

/* type alias */
// alias name type
alias float f32;

/* constants */
SEEK_SET :: 0;

func main() s32 {
    a: s8;
    b: s16 = 5;
    c: s32 = 61;
    d: s64;
    e := 51; // type inference
    f: u8 = 0xa;
    g: f32;
    h: f64 = 5125.124;
    i := true;
    j: bool;
    k: str; /* c-string */
    
    /* binary operators
     *  + - * % / << >> | & ^
     *  every operator can be put before 
     */
    a = 5 + 1;
    a += 56;
    
    /* unary operators
     * ++ -- ! ~ -
    */
    a++;
    ++a;
    
    m := nil; /* null pointer */
    
    /* sizeof variable or type */
    n := sizeof(s32);
    
    /* casting */
    o := cast(s32) a;
    
    return 0;
}
```

### Structs and Enums
```c++
struct Person {
    name: str,
    age: s32
}

emum Weekdays {
    Monday = 0,
    Tuesday = 5,
    Wednesday, // 6
    Thursday, // 7
    Friday = 51,
    Saturday = "Saturday" /* any literal accepted */
}

func main() s32 {
    p: Person;
    p.name = "Peter";
    p.age = 51;
    
    // or quick initialization
    
    q := {"Peter", 51} Person;
    
    t := cast(*Person) malloc(sizeof(Person));
    t.name = "Peter";
    t.age = 51;

    free(t);
    
    printf("%d\n", Weekdays.Monday);
    printf("%s\n", Weekdays.Saturday);
    
    return 0;
}
```

### Generic Functions
```c++
func array_reserve<T>(array: *[..] T, amount: s64) {
    if amount <= 0 {
        amount = 16;
    }

    mem := cast(*T) malloc(amount * sizeof(T));

    if array.data != nil {
        memcpy(mem, array.data, array.length * sizeof(T));
        free(array.data);
    }

    array.data = mem;
    array.capacity = amount;
}

func array_add<T>(array: *[..] T, item: T) {
    if array.length + 1 >= array.capacity {
        array_reserve(array, array.capacity * 2);
    }

    array.length++;
    (*array)[array.length - 1] = item;
}
```

### Static Array and Types of For Loops
```c++
func main() s32 {
    a := {7, 1, 2, 5}[]s32;
    b: [5]s32;

    for a {
        printf("value %d at %d\n", it, it_index);
    }

    for v := a {
        printf("value %d at %d\n", v, it_index);
    }
    
    for i := 0..a.length {
        printf("value %d at %d\n", a[i], i);
    }
    
    return 0;
}
```

### Dynamic Array
```c++
#use "libc";
#use "array";

func main() s32 {
	a: [..]s32;

	array_add(&a, 21);
	array_add(&a, 561);
	array_add(&a, 1);
	array_add(&a, 91);

	for v := a {
		printf("%d\n".data, v);
	}
    
	return 0;
}
```

### GLFW Hello World
```c++
#use "libc";
#use "glfw";

func main() s32 {
    if !glfwInit() {
        printf("Failed to initalize GLFW");
        return 0;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

    win := glfwCreateWindow(640, 480, "Test", nil, nil);

    if !win {
        printf("Failed to create window");
        glfwTerminate();
        return 0;
    }

    printf("Success");

    glfwMakeContextCurrent(win);

    while !glfwWindowShouldClose(win) {
        glfwPollEvents();

        glfwSwapBuffers(win);
    }

    glfwTerminate();

    return 0;
}
```