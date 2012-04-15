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
    function Syntax() {
        var IDENT = 1, NUMBER = 2, KEYWORD = 3;

        function Lexer(source) {
            var tokens = [];
            var unget_token;

            var REGEXP = /\s+|[a-zA-Z][a-zA-Z0-9]*|[0-9]+|:=|<=|>=|[+\-*\/=#<>(),:;!\.]/;
            var KEYWORDS = [
                "begin", "call", "const", "do", "end", "if",
                "odd", "print", "procedure", "then", "var", "while",
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

        function Parser(lexer, debug) {
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

                function dump_if(indent, ifo) {
                    console.log(indent + "+ [if]");
                    console.log(indent + "  - condition");
                    dump_node(indent + "    ", ifo.condition);

                    if (ifo.statements.length > 0) {
                        console.log(indent + "  - statements");
                        for (i = 0; i < ifo.statements.length; i++)
                            dump_node(indent + "    ", ifo.statements[i]);
                    }
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
                case "if":         dump_if(indent, ast);           break;
                case "loop":       dump_loop(indent, ast);         break;
                case "number":     dump_number(indent, ast);       break;
                case "operation":  dump_operation(indent, ast);    break;
                case "print":      dump_print(indent, ast);        break;
                case "procedure":  dump_procedure(indent, ast);    break;
                default:
                    console.log("unknown: " + ast.ntype);
                    console.log(ast);
                    break;
                }
            }

            // program = block "." .
            function program() {
                var r;

                if (debug)
                    console.log("-- parser: program --");

                r = block();
                check_next_keyword(".");

                return r;
            }

            // block = [ "const" ident "=" number {"," ident "=" number} ";"]
            //         [ "var" ident {"," ident} ";"]
            //         { "procedure" ident ";" block ";" } statement .
            function block() {
                var token, c, v, p, s;

                if (debug)
                    console.log("-- parser: block --");

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

                if (debug)
                    console.log("-- parser: block_var --");

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

                if (debug)
                    console.log("-- parser: block_procedure --");

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

                if (debug)
                    console.log("-- parser: statement --");

                if (token.is_keyword("begin"))
                    return statement_begin();
                else if (token.is_keyword("call"))
                    return statement_call();
                else if (token.is_keyword("if"))
                    return statement_if();
                else if (token.is_keyword("!"))
                    return statement_print();
                else if (token.is_keyword("print"))
                    return statement_print();
                else if (token.is_keyword("while"))
                    return statement_while();
                else if (token.type == IDENT)
                    return [ NodeAssign(NodeIdent(token), statement_assign()) ];
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
                        s = s.concat(r);

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

                if (debug)
                    console.log("-- parser: condition --");

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

                if (debug)
                    console.log("-- parser: expression --");

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

                if (debug)
                    console.log("-- parser: term --");

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

                if (debug)
                    console.log("-- parser: factor --");

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
                    r.dump = function() { dump_node("", r); };

                    return r;
                }
            };
        }

        return {
            parse: function(source) {
                var lexer = Lexer(source);

                try {
                    return Parser(lexer).parse();
                } catch (e) {
                    console.log(e);
                    lexer.dump();
                }
            }
        };
    }

    function CodeGen(ast) {
        var gregs = 0;

        function Environ(name, parent, vars, global) {
            this.name = name;
            this.parent = parent;
            this.regs = gregs;
            this.vars = {};
            this.global = global;
            this.label_index = 0;

            for (var i = 0; i < vars.length; i++) {
                this.vars[vars[i]] = "r" + this.regs;
                this.regs += 1;
            }

            if (global)
                gregs = this.regs;
        }

        Environ.prototype.getreg = function(value) {
            var reg;

            reg = this.vars[value];
            if (reg == undefined)
                return this.parent.getreg(value);
            else
                return reg;
        };

        Environ.prototype.gettmpreg = function() {
            return "r" + this.regs++;
        };

        Environ.prototype.getlocalregs = function() {
            var key, r = [];

            if (!this.global) {
                for (key in this.vars)
                    r.push(this.vars[key]);
            }

            return r;
        };

        Environ.prototype.genlabel = function() {
            var label;

            label = "__" + this.name + "_" + this.label_index;
            this.label_index += 1;

            return label;
        };

        function codegen_block(env, block) {
            var i, s, rcode = [];

            if (debug) {
                console.log("---- environ ----");
                console.log(env);
            }

            rcode = merge_code(rcode, codegen_statements(env, block.statements));
            rcode.push([ "ret" ]);

            return rcode;
        }

        function codegen_statements(env, statements) {
            var i, r, rcode = [];

            for (i = 0; i < statements.length; i++) {
                r = codegen_stmt(env, statements[i]);

                if (debug) {
                    console.log("==== code ====");
                    console.log(r);
                }

                if (typeof r[0] == "object")
                    rcode = merge_code(rcode, r);
                else
                    rcode.push(r);
            }

            return rcode;
        }

        function codegen_stmt(env, stmt, dst) {
            var reg;

            if (debug) {
                console.log("---- statement ----");
                console.log(stmt);
            }

            switch (stmt.ntype) {
            case "assign":
                return codegen_stmt_assign(env, stmt, dst);
            case "operation":
                return codegen_stmt_op(env, stmt, dst);
            case "if":
                return codegen_stmt_if(env, stmt, dst);
            case "loop":
                return codegen_stmt_loop(env, stmt, dst);
            case "call":
                return codegen_stmt_call(env, stmt, dst);
            case "print":
                return codegen_stmt_print(env, stmt, dst);
            default:
                throw "unknown statement type";
            }
        }

        function codegen_stmt_assign(env, stmt) {
            reg = env.getreg(stmt.lvalue.value);
            switch (stmt.rnode.ntype) {
            case "number":
                return [ "add", reg, stmt.rnode.value, 0 ];
            case "ident":
                return [ "add", reg, env.getreg(stmt.rnode.value), 0 ];
            case "operation":
                return codegen_stmt_op(env, stmt.rnode, reg);

            default:
                return undefined;
            }
        }

        function codegen_stmt_op(env, stmt, dst) {
            var v1, v2, vdst, rcode = [];

            if (debug) {
                console.log("---- statement: operation ----");
                console.log(stmt);
            }

            function stmt_value(value) {
                var rtmp;

                switch (value.ntype) {
                case "number":
                    return value.value;
                case "ident":
                    return env.getreg(value.value);
                case "operation":
                    rtmp = env.gettmpreg();
                    rcode = merge_code(rcode, codegen_stmt_op(env, value, rtmp));
                    return rtmp;

                default:
                    throw "unknown value type";
                }
            }

            v1 = stmt_value(stmt.value1, rcode);
            v2 = stmt_value(stmt.value2, rcode);
            vdst = (dst == undefined) ? v1 : dst;

            switch (stmt.opcode) {
            case "+":
                rcode.push([ "add", vdst, v1, v2 ]);
                break;
            case "-":
                rcode.push([ "sub", vdst, v1, v2 ]);
                break;
            case "*":
                rcode.push([ "mul", vdst, v1, v2 ]);
                break;
            case "/":
                rcode.push([ "div", vdst, v1, v2 ]);
                break;

            case "=":
                rcode.push([ "bne", vdst, v1, v2 ]);
                break;
            case "<":
                rcode.push([ "bge", vdst, v1, v2 ]);
                break;
            case "<=":
                rcode.push([ "bgt", vdst, v1, v2 ]);
                break;
            case ">":
                rcode.push([ "ble", vdst, v1, v2 ]);
                break;
            case ">=":
                rcode.push([ "blt", vdst, v1, v2 ]);
                break;

            default:
                throw "unknown opcode";
            }

            return rcode;
        }

        function codegen_stmt_if(env, stmt) {
            var rcode = [], else_label;

            if (debug) {
                console.log("------ if ------");
                console.log(stmt);
            }

            else_label = env.genlabel();
            rcode = merge_code(rcode, codegen_stmt(env, stmt.condition, else_label ));
            rcode = merge_code(rcode, codegen_statements(env, stmt.statements));
            rcode.push([ "label", else_label ]);

            return rcode;
        }

        function codegen_stmt_loop(env, stmt) {
            var r, rcode = [], start_label, end_label;

            if (debug)
                console.log("------ loop ------");

            start_label = env.genlabel();
            end_label = env.genlabel();

            rcode.push([ "label", start_label ]);
            rcode = merge_code(rcode, codegen_stmt(env, stmt.condition, end_label ));

            if (debug)
                console.log("-------- loop: statements --------");

            rcode = merge_code(rcode, codegen_statements(env, stmt.statements));
            rcode.push([ "jmp", start_label ]);
            rcode.push([ "label", end_label ]);

            return rcode;
        }

        function codegen_stmt_call(env, stmt) {
            var i, localregs, label, rcode = [];

            if (debug)
                console.log("------ call ------");

            localregs = env.getlocalregs();
            label = "_" + stmt.callee;

            for (i = 0; i < localregs.length; i++)
                rcode.push([ "push", localregs[i] ]);

            rcode.push([ "call", label ]);

            for (i = localregs.length - 1; i >= 0; i--)
                rcode.push([ "pop", localregs[i] ]);

            return rcode;
        }

        function codegen_stmt_print(env, stmt) {
            return [ "print", env.getreg(stmt.value) ];
        }

        function merge_code(basecode, newcode) {
            if (typeof newcode[0] == "object")
                return basecode.concat(newcode);
            else {
                basecode.push(newcode);
                return basecode;
            }
        }

        function print_code(code) {
            var i, c, operands;

            function strl(str, len) {
                var i, l, s;

                str = String(str);
                l = len - str.length;
                for (i = 0, s = ""; i < l; i++)
                    s += " ";

                return str + s;
            }

            for (i = 0; i < code.length; i++) {
                c = code[i];

                if (c[3] != undefined)
                    operands = 3;
                else if (c[2] != undefined)
                    operands = 2;
                else if (c[1] != undefined)
                    operands = 1;
                else
                    operands = 0;

                if (c[0] == "label")
                    console.log("| " + c[1] + ":");
                else {
                    switch (operands) {
                    case 0:
                        console.log("| \t" + c[0]);
                        break;
                    case 1:
                        console.log("| \t" + c[0] + "\t\t"
                                    + strl(c[1] + ",", 12));
                        break;
                    case 2:
                        console.log("| \t" + c[0] + "\t\t"
                                    + strl(c[1] + ",", 12) + "\t"
                                    + c[2]);
                        break;
                    case 3:
                        console.log("| \t" + c[0] + "\t\t"
                                    + strl(c[1] + ",", 12) + "\t"
                                    + strl(c[2] + ",", 12) + "\t"
                                    + c[3]);
                        break;
                    }
                }
            }
        }

        return {
            codegen: function(ast) {
                var i, r, code = [], root_env, proc_name, proc_block, proc_env;

                for (i = 0, r = []; i < ast.vars.length; i++)
                    r.push(ast.vars[i].value);

                root_env = new Environ("main", null, r, true);
                code.push([ "label", "_main" ]);
                code = code.concat(codegen_block(root_env, ast));

                for (i = 0; i < ast.procedures.length; i++) {
                    proc_name = ast.procedures[i].name;
                    proc_block = ast.procedures[i].block;

                    for (i = 0, r = []; i < proc_block.vars.length; i++)
                        r.push(proc_block.vars[i].value);

                    proc_env = new Environ(proc_name, root_env, r, false);
                    code.push([ "label", "_" + proc_name ]);
                    code = code.concat(codegen_block(proc_env, proc_block));
                }

                code.dump = function() { print_code(code); };

                return code;
            }
        };
    }

    this.parse = function(source) {
        return Syntax().parse(source);
    }

    this.generate_il = function(ast) {
        return CodeGen().codegen(ast);
    }
}

function PLZ_ILVM(code) {
    var regs = [], stack = [];

    function make_labelmap(code) {
        var i, lmap = {};

        for (i = 0; i < code.length; i++) {
            if (code[i][0] == "label")
                lmap[code[i][1]] = i;
        }

        return lmap
    }

    function loadopr(operand) {
        var r;

        if (typeof operand == "string" && operand[0] == "r") {
            if ((r = regs[Number(operand.slice(1))]) == undefined)
                r = 0;
        } else {
            r = operand;  // Number(operand);
        }

        return r;
    }

    function writeback(dst, value) {
        if (dst[0] != "r")
            throw "dst is not a register";

        regs[Number(dst.slice(1))] = value;

        if (debug) {
            console.log("\nregs =");
            console.log(regs);
            console.log("");
        }
    }

    this.run = function() {
        var pc, clock, opcode, dst, opr1, opr2;
        var labelmap = make_labelmap(code);

        for (pc = 0, clock = 1; pc < code.length; pc++, clock++) {
            if (debug)
                console.log(clock + ": pc[" + pc + "] " + code[pc]);

            opcode = code[pc][0];
            dst = code[pc][1];
            opr1 = loadopr(code[pc][2]);
            opr2 = loadopr(code[pc][3]);

            switch (opcode) {
            case "label":
                break;  // nop

            case "add":
                writeback(dst, opr1 + opr2);
                break;
            case "sub":
                writeback(dst, opr1 - opr2);
                break;
            case "mul":
                writeback(dst, opr1 * opr2);
                break;
            case "div":
                writeback(dst, parseInt(opr1 / opr2));
                break;

            case "bne":
                if (opr1 != opr2)
                    pc = labelmap[dst];
                break;
            case "bge":
                if (opr1 >= opr2)
                    pc = labelmap[dst];
                break;
            case "bgt":
                if (opr1 > opr2)
                    pc = labelmap[dst];
                break;
            case "ble":
                if (opr1 <= opr2)
                    pc = labelmap[dst];
                break;
            case "blt":
                if (opr1 < opr2)
                    pc = labelmap[dst];
                break;
            case "jmp":
                pc = labelmap[dst];
                break;

            case "call":
                stack.push(pc);
                pc = labelmap[dst];
                break;
            case "ret":
                pc = stack.pop();
                if (pc == undefined)
                    return;
                break;

            case "push":
                stack.push(loadopr(dst));
                break;
            case "pop":
                writeback(dst, stack.pop());
                break;
            case "print":
                console.log(loadopr(dst));
                break;

            default:
                throw "unknown opcode";
            }
        }
    }
}

var debug = process.argv[3];
var fs = require("fs");
var src = "" + fs.readFileSync(process.argv[2]);
var plz = new PLZ();

var ast = plz.parse(src);
if (debug)
    ast.dump();

var il = plz.generate_il(ast);
if (debug)
    il.dump();

var ilvm = new PLZ_ILVM(il);
ilvm.run();
