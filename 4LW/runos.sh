#!/usr/bin/env bash
function compile_os() {
    # pushd
    cd ../programs/OS
    res=$(../../_lang_compiler/compiler.py os._lang)
    echo "$res"
    # popd
}

function assemble_os() {
    asm=$(compile_os)
    bytecode=$(../assembler/assembler.py <(echo "$asm"))
    echo "$bytecode" 

}
code=$(assemble_os)
echo -e "\n\n\n"
stack exec 4LW -- <(echo "$code") -T tape

