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

            var REGEXP = /\s+|[a-zA-Z][a-zA-Z0-9]*|[0-9]+|:=|<=|>=|[+\-*=#<>(),:;!\.]/;
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

        return {
            parse: function(source) {
                var lexer;

                lexer = Lexer(source);
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
        var regs = 0;

        function Environ(name, penv, vars, global) {
            var vr = {};
            var label_index = 0;

            for (var i = 0; i < vars.length; i++) {
                vr[vars[i]] = "r" + regs;
                regs += 1;
            }

            this.name = name;
            this.penv = penv;
            this.vars = vr;
            this.global = global;

            this.getreg = function(value) {
                var reg;

                reg = this.vars[value];
                if (reg == undefined)
                    return this.penv.getreg(value);
                else
                    return reg;
            };

            this.getlocalregs = function() {
                var key, r = [];

                if (!this.global) {
                    for (key in this.vars)
                        r.push(this.vars[key]);
                }

                return r;
            }

            this.genlabel = function() {
                var label;

                label = "__" + name + "_" + label_index;
                label_index += 1;

                return label;
            };
        }

        function codegen_block(env, block) {
            var i, s, rcode = [];

            console.log('---- environ ----');
            console.log(env);

            rcode = rcode.concat(codegen_statements(env, block.statements));
            rcode.push([ 'ret' ]);

            return rcode;
        }

        function codegen_statements(env, statements) {
            var i, r, rcode = [];

            for (i = 0; i < statements.length; i++) {
                r = codegen_stmt(env, statements[i]);

                console.log('==== code ====');
                console.log(r);

                if (typeof r[0] == "object")
                    rcode = rcode.concat(r);
                else
                    rcode.push(r);
            }

            return rcode;
        }

        function codegen_stmt(env, stmt, dst) {
            var reg;

            console.log('---- statement ----');
            console.log(stmt);

            switch (stmt.ntype) {
            case 'assign':
                return codegen_stmt_assign(env, stmt, dst);
            case 'operation':
                return codegen_stmt_op(env, stmt, dst);
            case 'if':
                return codegen_stmt_if(env, stmt, dst);
            case 'loop':
                return codegen_stmt_loop(env, stmt, dst);
            case 'call':
                return codegen_stmt_call(env, stmt, dst);
            case 'print':
                return codegen_stmt_print(env, stmt, dst);
            default:
                throw "unknown statement type";
            }
        }

        function codegen_stmt_assign(env, stmt) {
            reg = env.getreg(stmt.lvalue.value);
            switch (stmt.rnode.ntype) {
            case 'number':
                return [ 'add', stmt.rnode.value, '0', reg ];
            case 'ident':
                return [ 'add', env.getreg(stmt.rnode.value), '0', reg ];
            case 'operation':
                return codegen_stmt_op(env, stmt.rnode, reg);

            default:
                return undefined;
            }
        }

        function codegen_stmt_op(env, stmt, dst) {
            var v1, v2, vdst;

            console.log('------ stmt_op ------');

            function stmt_value(value) {
                switch (value.ntype) {
                case 'number':
                    return value.value;
                case 'ident':
                    return env.getreg(value.value);
                default:
                    throw "unknown value type";
                }
            }

            v1 = stmt_value(stmt.value1);
            v2 = stmt_value(stmt.value2);
            vdst = (dst == undefined) ? v1 : dst;

            switch (stmt.opcode) {
            case '+':
                return [ 'add', v1, v2, vdst ];
            case '-':
                return [ 'sub', v1, v2, vdst ];
            case '*':
                return [ 'mul', v1, v2, vdst ];
            case '/':
                return [ 'div', v1, v2, vdst ];
            case '=':
                return [ 'jmpneq', v1, v2, vdst ];
            case '<':
                return [ 'jmpnlt', v1, v2, vdst ];
            case '>':
                return [ 'jmpngt', v1, v2, vdst ];

            default:
                throw "unknown opcode";
            }
        }

        function codegen_stmt_if(env, stmt) {
            var rcode = [], else_label;

            console.log('------ if ------');
            console.log(stmt);

            else_label = env.genlabel();
            rcode.push(codegen_stmt(env, stmt.condition, else_label ));
            rcode = rcode.concat(codegen_statements(env, stmt.statements));
            rcode.push([ 'label', else_label ]);

            return rcode;
        }

        function codegen_stmt_loop(env, stmt) {
            var r, rcode = [], start_label, end_label;

            console.log('------ loop ------');

            start_label = env.genlabel();
            end_label = env.genlabel();

            rcode.push([ 'label', start_label ]);
            rcode.push(codegen_stmt(env, stmt.condition, end_label ));

            console.log('-------- loop: statements --------');

            rcode = rcode.concat(codegen_statements(env, stmt.statements));
            rcode.push([ 'jmp', start_label ]);
            rcode.push([ 'label', end_label ]);

            return rcode;
        }

        function codegen_stmt_call(env, stmt) {
            var i, localregs, label, rcode = [];

            console.log('------ call ------');

            localregs = env.getlocalregs();
            label = '_' + stmt.callee;

            for (i = 0; i < localregs.length; i++)
                rcode.push([ 'push', localregs[i] ]);

            rcode.push([ 'call', label ]);

            for (i = localregs.length - 1; i >= 0; i--)
                rcode.push([ 'pop', localregs[i] ]);

            return rcode;
        }

        function codegen_stmt_print(env, stmt) {
            var reg;

            console.log('------ print ------');

            console.log(stmt);

            console.log(stmt.value);

            reg = env.getreg(stmt.value);
            return [ 'print', reg ];
        }

        return {
            codegen: function(ast) {
                var i, r, code = [], root_env, proc_name, proc_block, proc_env;

                for (i = 0, r = []; i < ast.vars.length; i++)
                    r.push(ast.vars[i].value);

                root_env = new Environ('main', null, r, true);
                code.push([ 'label', '_main' ]);
                code = code.concat(codegen_block(root_env, ast));

                for (i = 0; i < ast.procedures.length; i++) {
                    proc_name = ast.procedures[i].name;
                    proc_block = ast.procedures[i].block;

                    for (i = 0, r = []; i < proc_block.vars.length; i++)
                        r.push(proc_block.vars[i].value);

                    proc_env = new Environ(proc_name, root_env, r, false);
                    code.push([ 'label', '_' + proc_name ]);
                    code = code.concat(codegen_block(proc_env, proc_block));
                }

                return code;
            }
        };
    }

    this.compile = function(source) {
        var ast = Syntax().parse(source);
        ast.dump();

        return CodeGen().codegen(ast);
    }
}

source =
"var	i, n, f; " +
    "" +
"procedure fib; " +
"var	m, save; " +
"begin " +
"	m := n; " +
"	if m = 0 then " +
"		f := 1; " +
"	if m = 1 then " +
"		f := 1; " +
"	if m > 1 then begin " +
"		n := m - 1; " +
"		call fib; " +
"		save :=	f; " +
"		n := m - 2; " +
"		call fib; " +
"		f := f + save " +
"	end " +
"end; " +
"" +
"begin " +
"	i := 0; " +
"	while i	< 20 do	begin " +
"		n := i; " +
"		call fib; " +
"		print f; " +
"		i := i + 1; " +
"	end " +
"end. "

function print_code(code) {
    var i, c, operands;

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

        if (c[0] == 'label')
            console.log('| ' + c[1] + ":");
        else {
            switch (operands) {
            case 0:
                console.log("| \t" + c[0]);
                break;
            case 1:
                console.log("| \t" + c[0] + "\t\t" + c[1]);
                break;
            case 2:
                console.log("| \t" + c[0] + "\t\t" + c[1] + ",\t" + c[2]);
                break;
            case 3:
                console.log("| \t" + c[0] + "\t\t" + c[1] + ",\t" + c[2] + ",\t" + c[3]);
                break;
            }
        }
    }
}

pl0 = new PLZ();
code = pl0.compile(source);

console.log(code);
print_code(code);
