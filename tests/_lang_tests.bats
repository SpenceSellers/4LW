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
