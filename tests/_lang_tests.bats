load lib
@test "ABC" {
    out=$(compile_lang_and_run _lang_test_files/abc._lang)
    [ "$out" == "ABC" ]
}

@test "Simple Fcall" {
    out=$(compile_lang_and_run _lang_test_files/fcall_simple._lang)
    [ "$out" == "A" ]
}

@test "Function Args" {
    out=$(compile_lang_and_run _lang_test_files/fargs._lang)
    [ "$out" == "WT" ]
}

@test "Function Return" {
    out=$(compile_lang_and_run _lang_test_files/func_return._lang)
    [ "$out" == "BE" ]
}

@test "While" {
    out=$(compile_lang_and_run _lang_test_files/while._lang)
    [ "$out" == "MMMMMMMMMM" ]
}

@test "If" {
    out=$(compile_lang_and_run _lang_test_files/if._lang)
    [ "$out" == "EOEOE" ]
}

@test "Bool" {
    out=$(compile_lang_and_run _lang_test_files/bool._lang)
    [ "$out" == "TFTF" ]
}

@test "References" {
    out=$(compile_lang_and_run _lang_test_files/references._lang)
    [ "$out" == "AAB" ]
}
