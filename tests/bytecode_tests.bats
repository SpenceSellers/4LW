load lib
@test "Hello World" {
    out=$(run_bytecode "FNE____C___X___C__EYHLA_PUE____S___P___R___AMVE____S___S___R___AJZE__NMR___A___C__EAMVE___MR___A___I____MVE___PR___A___R___AJPC____C__BJPLE____S___P___R___ARTA____H__AE__AL__AL__AO__A___AW__AO__AR__AL__ADZZZZ")
    [ "$out" == "Hello world" ]
}
