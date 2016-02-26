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
