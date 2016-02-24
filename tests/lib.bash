function compile_lang_and_run(){
    asm=$(../_lang_compiler/compiler.py $1);
    code=$(../assembler/assembler.py <(echo "$asm"));
    output=$(cd ../4LW; stack exec 4LW -- <(echo "$code") 2> /dev/null) ;
    echo "$output";
}

compile_lang_and_run "../programs/ctest._lang";
