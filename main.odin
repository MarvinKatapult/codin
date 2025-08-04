package main

import "cc"

main :: proc() {
    cc.compile_file("tests/parse/no_semicolon.c")
    /*
        no_semicolon.c
        not_expression.c
        space_in_keyword.c
        switched_parens.c
        unclosed_brace.c
        unclosed_paren.c
    */
}
