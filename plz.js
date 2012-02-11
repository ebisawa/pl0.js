/*
  program = block "." .

  block = [ "const" ident "=" number {"," ident "=" number} ";"]
          [ "var" ident {"," ident} ";"]
          { "procedure" ident ";" block ";" } statement .
  statement = [ ident ":=" expression | "call" ident |
              "begin" statement {";" statement } "end" |
              "if" condition "then" statement |
              "while" condition "do" statement ].
  condition = "odd" expression |
              expression ("="|"#"|"<"|"<="|">"|">=") expression .
  expression = [ "+"|"-"] term { ("+"|"-") term}.
  term = factor {("*"|"/") factor}.
  factor = ident | number | "(" expression ")".
*/

function PLZ() {
    var IDENT = 1, NUMBER = 2, KEYWORD = 3;

    function Lexer(source) {
        var tokens = [];
        var unget_token;

        var REGEXP = /\s+|[a-zA-Z][a-zA-Z0-9]*|[0-9]+|:=|<=|>=|[+\-*=#<>(),:;!\.]/;
        var KEYWORDS = [
            "begin", "call", "const", "do", "end", "if",
            "odd", "procedure", "then", "var", "while",
            "+", "-", "*", "/", "=", ":=", "(", ")",
            "<", "<=", ">", ">=", ":", ";", ",", ".", "!",
        ];
        
        function Token(type, string) {
            this.type = type;
            this.string = string;
            this.is_keyword = function(keyword) {
                return (this.type == KEYWORD && this.string == keyword);
            };
        }

        while (source.length > 0) {
            source = source.replace(REGEXP, function (matched) {
                    if (!matched.match(/\s+/))
                        tokens.push(matched);
                    return "";
                });
        }

        return {
            getnext: function() {
                var k, t, r;

                if (unget_token != undefined) {
                    r = unget_token;
                    unget_token = undefined;
                    return r;
                }

                t = tokens.shift();
                if (t == undefined)
                    return null;
                else if (t.match(/^\d+$/))
                    return new Token(NUMBER, Number(t));
                else if (KEYWORDS.indexOf(t.toLowerCase()) > -1)
                    return new Token(KEYWORD, t.toLowerCase());
                else
                    return new Token(IDENT, t);
            },

            unget: function(token) {
                unget_token = token;
            },

            dump: function() {
                console.log("XXX dump = ");
                console.log(tokens);
            }
        };
    }

    function Parser(lexer) {
        function NodeBlock(consts, vars, procs, stmts) {
            return { ntype: "block", consts: consts, vars: vars, procedures: procs, statements: stmts };
        }

        function NodeProcedure(name, block) {
            return { ntype: "procedure", name: name, block: block };
        }

        function NodeIdent(token) {
            return { ntype: "ident", value: token.string };
        }

        function NodeNumber(value) {
            return { ntype: "number", value: Number(token.string) };
        }

        function NodeOperation(op, value1, value2) {
            return { ntype: "operation", opcode: op, value1: value1, value2: value2 };
        }

        function NodeAssign(lvalue, rnode) {
            return { ntype: "assign", lvalue: lvalue, rnode: rnode };
        }

        function NodeIf(cond, stmts) {
            return { ntype: "if", condition: cond, statements: stmts };
        }

        function NodeLoop(cond, stat) {
            return { ntype: "loop", condition: cond, statements: stat };
        }

        function NodeCall(callee) {
            return { ntype: "call", callee: callee };
        }

        function NodePrint(value) {
            return { ntype: "print", value: value };
        }

        function dump_node(indent, ast) {
            function dump_block(indent, block) {
                var v, i;

                console.log(indent + "+ [block]");

                if (block.consts.length > 0) {
                    console.log(indent + "  - consts");
                    for (i = 0; i < block.consts.length; i++) {
                        v = block.consts[i];
                        dump_node(indent + "    ", v);
                    }
                }

                if (block.vars.length > 0) {
                    console.log(indent + "  - vars");
                    for (i = 0; i < block.vars.length; i++) {
                        v = block.vars[i];
                        dump_node(indent + "    ", v);
                    }
                }

                if (block.procedures.length > 0) {
                    console.log(indent + "  - procedures");
                    for (i = 0; i < block.procedures.length; i++) {
                        v = block.procedures[i];
                        dump_node(indent + "    ", v);
                    }
                }

                if (block.statements.length > 0) {
                    console.log(indent + "  - statements");
                    for (i = 0; i < block.statements.length; i++) {
                        v = block.statements[i];
                        dump_node(indent + "    ", v);
                    }
                }
            }

            function dump_procedure(indent, proc) {
                console.log(indent + "+ [procedure]");
                console.log(indent + "  - name: " + proc.name);
                console.log(indent + "  - block");
                dump_node(indent + "    ", proc.block);
            }

            function dump_loop(ident, loop) {
                console.log(indent + "+ [loop]");
                console.log(indent + "  - condition");
                dump_node(indent + "    ", loop.condition);

                if (loop.statements.length > 0) {
                    console.log(indent + "  - statements");
                    for (i = 0; i < loop.statements.length; i++)
                        dump_node(indent + "    ", loop.statements[i]);
                }
            }

            function dump_call(ident, call) {
                console.log(indent + "+ [call] " + call.callee);
            }

            function dump_print(ident, print) {
                console.log(indent + "+ [print] " + print.value);
            }

            function dump_assign(indent, stmt) {
                console.log(indent + "+ [assign]");
                console.log(indent + "  - lvalue");
                dump_node(indent + "    ", stmt.lvalue);
                console.log(indent + "  - rnode");
                dump_node(indent + "    ", stmt.rnode);
            }

            function dump_operation(indent, stmt) {
                console.log(indent + "+ [operation]");
                console.log(indent + "  - opcode: " + stmt.opcode);
                console.log(indent + "  - value1");
                dump_node(indent + "    ", stmt.value1);
                console.log(indent + "  - value2");
                dump_node(indent + "    ", stmt.value2);
            }

            function dump_ident(indent, value) {
                console.log(indent + "+ [ident] " + value.value);
            }

            function dump_number(indent, value) {
                console.log(indent + "+ [number] " + value.value);
            }

            switch (ast.ntype) {
            case "assign":     dump_assign(indent, ast);       break;
            case "block":      dump_block(indent, ast);        break;
            case "call":       dump_call(indent, ast);         break;
            case "ident":      dump_ident(indent, ast);        break;
            case "loop":       dump_loop(indent, ast);         break;
            case "number":     dump_number(indent, ast);       break;
            case "operation":  dump_operation(indent, ast);    break;
            case "print":      dump_print(indent, ast);        break;
            case "procedure":  dump_procedure(indent, ast);    break;
            default:
                console.log(ast);
                break;
            }
        }

        // program = block "." .
        function program() {
            var r;

            r = block();
            check_next_keyword(".");

            return r;
        }

        // block = [ "const" ident "=" number {"," ident "=" number} ";"]
        //         [ "var" ident {"," ident} ";"]
        //         { "procedure" ident ";" block ";" } statement .
        function block() {
            var token, c, v, p, s;

            token = lexer.getnext();
            c = []; v = []; p = [];

            if (token.is_keyword("const")) {
                c = c.concat(block_const());
                token = lexer.getnext();
            }

            if (token.is_keyword("var")) {
                v = v.concat(block_var());
                token = lexer.getnext();
            }

            while (token.is_keyword("procedure")) {
                p.push(block_procedure());
                token = lexer.getnext();
            }

            lexer.unget(token);
            s = statement();

            return NodeBlock(c, v, p, s);
        }

        function block_const() {
            console.log("warning: block_const() has not implemeted yet");
            return [];
        }

        //         [ "var" ident {"," ident} ";"]
        function block_var() {
            var token, vars = [];

            for (;;) {
                token = lexer.getnext();
                if (token.type != IDENT)
                    error_unexpected(token);

                vars.push(NodeIdent(token));
                token = lexer.getnext();

                if (token.is_keyword(","))
                    continue;
                else if (token.is_keyword(";"))
                    return vars;
                else
                    error_unexpected(token);
            }
        }

        //         { "procedure" ident ";" block ";" }
        function block_procedure() {
            var token, n, b;

            token = lexer.getnext();
            if (token.type != IDENT)
                error_unexpected(token);

            n = token.string;
            check_next_keyword(";");
            b = block();
            check_next_keyword(";");

            return NodeProcedure(n, b);
        }

        // statement = [ ident ":=" expression | "call" ident |
        //             "begin" statement {";" statement } "end" |
        //             "if" condition "then" statement |
        //             "while" condition "do" statement ].
        function statement() {
            var token = lexer.getnext();

            if (token.is_keyword("begin"))
                return statement_begin();
            else if (token.is_keyword("call"))
                return statement_call();
            else if (token.is_keyword("if"))
                return statement_if();
            else if (token.is_keyword("!"))
                return statement_print();
            else if (token.is_keyword("while"))
                return statement_while();
            else if (token.type == IDENT)
                return NodeAssign(NodeIdent(token), statement_assign());
            else {
                lexer.unget(token);
                return null;
            }
        }

        function statement_assign() {
            check_next_keyword(":=");
            return expression();
        }

        function statement_call() {
            var token;

            token = lexer.getnext();
            if (token.type != IDENT)
                error_unexpected(token);

            return NodeCall(token.string);
        }

        function statement_begin() {
            var token, r, s = [];

            for (;;) {
                if ((r = statement()) != null)
                    s.push(r);

                token = lexer.getnext();
                if (token.is_keyword(";"))
                    continue;
                else if (token.is_keyword("end"))
                    return s;
                else
                    error_unexpected(token);
            }
        }

        //             "if" condition "then" statement |
        function statement_if() {
            var c, s;

            c = condition();
            check_next_keyword("then");
            s = statement();

            return NodeIf(c, s);
        }

        //             "while" condition "do" statement ].
        function statement_while() {
            var c, s;

            c = condition();
            check_next_keyword("do");
            s = statement();

            return NodeLoop(c, s);
        }

        function statement_print() {
            var token;

            token = lexer.getnext();
            if (token.type != IDENT)
                error_unexpected(token);

            return NodePrint(token.string);
        }

        // condition = "odd" expression |
        //             expression ("="|"#"|"<"|"<="|">"|">=") expression .
        function condition() {
            var v1, v2, token;

            token = lexer.getnext();
            if (token.is_keyword("odd")) {
                v1 = expression();
                return NodeOperation("odd", v1, nil);
            } else {
                lexer.unget(token);
                v1 = expression();

                token = lexer.getnext();
                switch (token.string) {
                case "=": case "<": case "<=": case ">": case ">=":
                    break;
                default:
                    error_unexpected(token);
                    break;
                }

                v2 = expression();
                return NodeOperation(token.string, v1, v2);
            }
        }

        // expression = ["+"|"-"] term { ("+"|"-") term}.
        function expression() {
            var v1, v2, node, token;

            /* XXX */
            token = lexer.getnext();
            switch (token.string) {
            case "+": case "-":
                break;
            default:
                lexer.unget(token);
                break;
            }

            v1 = term();

            for (;;) {
                token = lexer.getnext();
                if (!token.is_keyword("+") && !token.is_keyword("-")) {
                    lexer.unget(token);
                    return v1;
                }

                v2 = term();
                v1 = NodeOperation(token.string, v1, v2);
            }
        }

        // term = factor {("*"|"/") factor}.
        function term() {
            var v1, v2, node, token;

            v1 = factor();

            for (;;) {
                token = lexer.getnext();
                if (!token.is_keyword("*") && !token.is_keyword("/")) {
                    lexer.unget(token);
                    return v1;
                }

                v2 = factor();
                v1 = NodeOperation(token.string, v1, v2);
            }
        }

        // factor = ident | number | "(" expression ")".
        function factor() {
            var r;

            token = lexer.getnext();
            if (token.type == IDENT)
                return NodeIdent(token);
            else if (token.type == NUMBER)
                return NodeNumber(token);
            else if (token.is_keyword("(")) {
                r = expression();
                check_next_keyword(")");
                return r;
            } else {
                error_unexpected(token);
                return undefined;
            }
        }

        function check_next_keyword(keyword) {
            t = lexer.getnext();
            if (!t.is_keyword(keyword))
                error_unexpected(t);
        }

        function error_unexpected(token) {
            console.log("syntax erorr: unexpected token '" + token.string 
                        + "' [" + error_unexpected.caller.name
                        + " <- " + error_unexpected.caller.caller.name
                        + " <- " + error_unexpected.caller.caller.caller.name + "]");
        
            throw "unexpected token";
        }

        return {
            parse: function() {
                var r;

                r = program();
                r.dump = function() { dump_node('', r) };

                return r;
            }
        };
    }

    this.parse = function (source) {
        var lexer;

        lexer = Lexer(source);
        try {
            return Parser(lexer).parse();
        } catch (e) {
            console.log(e);
            lexer.dump();
        }
    }
}


source =
    "VAR x, squ;\n" +
    "PROCEDURE square;\n" +
    "BEGIN\n" +
    "  squ := x * x\n" +
    "END;\n" +
    "\n" +
    "BEGIN\n" +
    "  x := 1;\n" +
    "  WHILE x <= 10 DO\n" +
    "  BEGIN\n" +
    "    CALL square;\n" +
    "    ! squ;\n" +
    "    x := x + 1;\n" +
    "  END\n" +
    "END.\n";

pl0 = new PLZ();
r = pl0.parse(source);
r.dump();
