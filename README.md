4LW
===
![image](https://i.imgur.com/Saho9Vh.gif)
## What is 4LW?
4LW is a Base-27 virtual machine and corresponding stack (an Assembler, a compiler for a C-like language called _lang, and a simple Operating System). The VM is written in Haskell, the assembler and compiler are written in Python, and the OS is of course written in _lang.

The 4LW's equivalent of a bit is a Letter. A letter can have 27 possible values: a space (Usually represented as an Underscore '_'), or an uncased character from A to Z.

The 4LW has a word size of four letters (Hence the name, *4* *L*etter *W*ord), giving 531441 possible values per word.
This gives the 4LW a word size roughly eight times bigger than a 16 bit processor.

### But why?
4LW has no practical use at all. I started 4LW because I wanted to learn about low-level systems and the history of computing. It seems counterintuitive to learn about the past by making up something that has never existed before, but it's been a huge success for me. 4LW is different enough where you can't just copy-and-paste the real-life solution, you are forced to discover the problems and work out the motivations behind the solutions yourself.

## Three Hello World examples:
#### 4LW bytecode:
Here's what a standalone "Hello World" looks like in 4LW bytecode:
`FNE____C___X___C__EYHLA_PUE____S___P___R___AMVE____S___S___R___AJZE__NMR___A___C__EAMVE___MR___A___I____MVE___PR___A___R___AJPC____C__BJPLE____S___P___R___ARTA____H__AE__AL__AL__AO__A___AW__AO__AR__AL__ADZZZZ`

Oh dear. That's not very readable at all. Just like in real life, bytecode is best left to the machines to generate. Someone could go crazy trying to write this by hand (I would know...).

### 4LW Assembly:
The bytecode above was generated by this assembly code.

    FN [const :print] [const :hworld_str]
    HL

    function print preserving A {
        MV [stack S] [reg A]             # Move argument to register A.

        label print_loop
        JZ [reg neg mem A] [const :done] # Have we reached ZZZZ character?
        MV [reg mem A] [io]              # Write the character.
        MV [reg plusfour A] [reg A]      # Increment memory address.
        JP [const :print_loop]           # Loop

        label done
    }

    term_string hworld_str "Hello world"

That looks a lot more readable than the bytecode.
### _lang:
Here's what a standalone "Hello World" looks like in _lang:

    print("Hello world");
    halt;
    function print(str){
        while ((*str) != ZZZZ){
            [IO] := *str;
            str := str + 4;
        };
    };

There, that looks like something almost recognizable as a real language. This _lang code will generate something close to (but uglier than) the assembly above.
## How to use 4LW
### Build 4LW
The 4LW VM build process uses [The Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/). Simply go to the `4LW/` directory and run `stack build 4LW`.

### Run 4LW
4LW requires code (in raw Letter format) to be loaded into main memory at VM start.

To run a file such as `code.4lw`, run `stack exec 4LW -- code.4lw`.

Writing 4LW bytecode (lettercode?) by hand is tedious though, chances are we'll want to use the assembler.

### Assemble a 4LW assembly program
The assembler is located at `assembler/assembler.py`. Assembler imports are calculated relative to the present working directory, so if you're in (for example) the `programs/` directory, you'll want to run `../assembler/assembler.py myprogram.4lwa`.

The assembler spits out the bytecode on stdout, so you'll probably want to pipe it to a file.

After a few months writing assembly programs got tiring, so maybe you'll want to use the _lang compiler:

### Compile a _lang program
The process for compiling a program is similar to assembling one. The compiler is located at `_lang_compiler/compiler.py`. Includes are calculated relative to the present working directory as well, so keep that in mind when compiling.

The compiler spits the compiled 4LW assembly out to stdout, so you'll probably want to pipe it to a file or directly to the assembler.
