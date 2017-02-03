function compile_lang_and_run(){
    asm=$(../_lang_compiler/compiler.py $1);
    code=$(../assembler/assembler.py <(echo "$asm"));
    output=$(run_bytecode "$code");
    echo "$output";
}

function run_bytecode(){
    output=$(cd ../4LW; stack exec 4LW -- -t 0 <(echo "$1") 2> /dev/null);
    echo "$output";
}
