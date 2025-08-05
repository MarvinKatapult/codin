package cc

import "core:strings"

@(private="file")
append_fasm_header :: proc(str_b: ^strings.Builder) -> bool {
    strings.write_string(str_b,
        "format ELF64 executable 3\n"
    )
    strings.write_string(str_b,
        "entry start\n"
    )
    strings.write_string(str_b,
        "\n"
    )
    strings.write_string(str_b,
        "segment readable executable\n"
    )

    return true;
}

@(private="file")
calc_ret_value :: proc(statement_node: AstNode) -> (bool, string){
    // At the moment we can expect that there is just one expression in statement:
    expression_t := statement_node.childs[0].value.(AstExpression);
    return true, expression_t.value;
}

@(private="file")
generate_for_statement :: proc(str_b: ^strings.Builder, statement_node: AstNode, function_label: string) -> bool {

    statement_t := statement_node.value.(AstStatement);

    #partial switch statement_node.type {
        case .AST_RETURN_STATEMENT:
            ok, ret_value := calc_ret_value(statement_node);
            if !ok do return false;

            if function_label != "start" {
                strings.write_string(str_b, "\tmov rax, ");
                strings.write_string(str_b, ret_value);
                strings.write_string(str_b, "\t; Setting return code\n");
                strings.write_string(str_b, "\tret \t\t; Returning\n");
            } else {
                strings.write_string(str_b, "\tmov rdi, ");
                strings.write_string(str_b, ret_value);
                strings.write_string(str_b, "\t; Setting exit code\n");
                strings.write_string(str_b, "\tmov rax, 60\t; (sys_exit)\n");
                strings.write_string(str_b, "\tsyscall\t\t; Shutting down program\n");
            }
    }

    return true;
}

@(private="file")
generate_for_function :: proc(str_b: ^strings.Builder, function_node: AstNode) -> bool {

    function_t := function_node.value.(AstFunction);

    function_label := function_t.identifier;
    if function_label == "main" do function_label = "start";

    strings.write_string(str_b, function_label);
    strings.write_string(str_b, ":\n");

    for child in function_node.childs {
        if !generate_for_statement(str_b, child, function_label) do return false;
    }
    
    return true;
}

@(private="file")
generate_for_ast_node :: proc(str_b: ^strings.Builder, node: AstNode) -> bool {
    #partial switch node.type {
        case .AST_PROGRAM:
            for child in node.childs {
                if !generate_for_ast_node(str_b, child) do return false;
            }
        case .AST_FUNCTION:
            if !generate_for_function(str_b, node) do return false;
    }
    return true;
}

@(private="package")
generate_asm :: proc(ast: AstNode) -> string {
    str_b := strings.builder_make();
    defer strings.builder_destroy(&str_b);

    if !append_fasm_header(&str_b) do return "";
    if !generate_for_ast_node(&str_b, ast) do return "";

    return strings.clone(strings.to_string(str_b));
}
