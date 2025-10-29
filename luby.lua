#!/usr/bin/env luajit

--[[
Luby - A Ruby-esque language that transpiles to Lua
Now has basic error checking, but you should still be alert. It's not perfect.
Pass this script a filename to print the transpiled code to stdout
Pass it a folder to recursively transpile all *.rb files to the current working directory 
(the folder structures will remain in tact)
- poltergasm
]]

package.path = "/usr/local/luby/?.lua;" .. package.path

-- try to load lpeg, fall back to lulpeg if not available
local lpeg
local ok, mod = pcall(require, "lpeg")
if ok then
    lpeg = mod
else
    lpeg = require("lulpeg")
end

local P, R, S, V, C, Ct, Cc, Cg = 
    lpeg.P, lpeg.R, lpeg.S, lpeg.V, lpeg.C, lpeg.Ct, lpeg.Cc, lpeg.Cg

-- LEXICAL ELEMENTS

local ws = S(" \t\n\r")^1  -- at least one whitespace
local ws0 = S(" \t\n\r")^0  -- zero or more whitespace
-- Don't consume comments in skip - handle them separately
local skip = ws^0

-- Identifiers
local alpha = R("az", "AZ") + P("_")
local alnum = alpha + R("09")
local identifier = C(alpha * alnum^0)

-- Numbers
local number = C(R("09")^1 * (P(".") * R("09")^1)^-1)

-- Strings
local escape = P("\\") * P(1)
local string_sq = Ct(Cc("string") * Cg(C(P("'") * (escape + (1 - S("'\\")))^0 * P("'")), "value"))
local string_dq_literal = Ct(Cc("string") * Cg(C(P('"') * (escape + (1 - S('"\\')))^0 * P('"')), "value"))

-- Pattern for matching strings (for ternary parsing)
local any_string = (P('"') * (escape + (1 - S('"\\')))^0 * P('"')) + 
                   (P("'") * (escape + (1 - S("'\\")))^0 * P("'"))

-- Literals
local bool_lit = C(P("true") + P("false"))
local nil_lit = C(P("nil"))

-- GRAMMAR

local grammar = P{
    "chunk",
    
    chunk = skip * Ct(V("statement")^0) * skip,
    
    statement = (V("class_def") +
                 V("method_def") +
                 V("let_stmt") +
                 V("case_stmt") +
                 V("if_stmt") +
                 V("while_stmt") +
                 V("for_in_stmt") +
                 V("each_pair_stmt") +
                 V("each_stmt") +
                 V("instance_var_assign") +
                 V("super_stmt") +
                 V("unary_op_stmt") +
                 V("puts_stmt") +
                 V("return_stmt") +
                 V("ternary_stmt") +
                 V("assignment_stmt") +
                 V("other_stmt")) * skip,
    
    -- let statement: let var = value or let x, y = val1, val2
    let_stmt = Ct(
        Cc("let") *
        P("let") * ws * 
        Cg(C(identifier * (ws0 * P(",") * ws0 * identifier)^0), "vars") * ws0 *
        P("=") * ws0 * 
        Cg(V("let_values"), "values")
    ),
    
    -- values for let (try structured patterns that need full parsing, but use raw for comma-separated)
    let_values = V("ternary_expr") + V("lambda_expr") + V("hash_table") + V("array_table") + C((1 - S("\n"))^1),
    
    -- class: class Name < Parent ... end
    class_def = Ct(
        Cc("class") *
        P("class") * ws * Cg(identifier, "name") * ws0 *
        (P("<") * ws0 * Cg(identifier, "parent"))^-1 * skip *
        Cg(Ct(V("class_method")^0), "methods") *
        P("end")
    ),
    
    class_method = Ct(
        Cc("method") *
        P("def") * ws * Cg(identifier, "name") * ws0 *
        Cg(V("param_list"), "params") * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end") * skip
    ),
    
    -- method: def name(params) : Local ... end or def table.name(params) ... end
    method_def = Ct(
        Cc("def") *
        P("def") * ws * Cg(C(identifier * (P(".") * identifier)^0), "name") * ws0 *
        Cg(V("param_list"), "params") * ws0 *
        (P(":") * ws0 * Cg(C(P("Local")), "is_local"))^-1 * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    param_list = (P("(") * ws0 * Ct((identifier * ws0 * (P(",") * ws0 * identifier * ws0)^0)^-1) * P(")")) + Ct(Cc()),
    
    -- case: case expr when cond ... else ... end
    case_stmt = Ct(
        Cc("case") *
        P("case") * ws * Cg(V("simple_expr"), "expr") * skip *
        Cg(Ct(V("when_clause")^1), "whens") *
        (P("else") * skip * Cg(Ct(V("statement")^0), "else_body"))^-1 *
        P("end")
    ),
    
    when_clause = Ct(
        P("when") * ws * Cg(V("when_condition"), "condition") * skip *
        Cg(Ct(V("statement")^0), "body")
    ),
    
    when_condition = V("range_cond") + V("comp_cond") + V("pattern_cond") + V("simple_expr"),
    
    range_cond = Ct(Cc("range") * Cg(number, "from") * P("..") * Cg(number, "to")),
    comp_cond = Ct(Cc("comparison") * Cg(C(S("<>") * P("=")^-1), "op") * ws0 * Cg(V("simple_expr"), "value")),
    
    -- pPattern condition for partial string matching: "prefix%" or "%suffix"
    pattern_cond = Ct(
        Cc("pattern") *
        P('"') * 
        (Cg(C((1 - S('"%'))^1), "prefix") * Cg(Cc("prefix"), "type") * P('%"') +
         P('%') * Cg(C((1 - S('"%'))^1), "suffix") * Cg(Cc("suffix"), "type") * P('"'))
    ),
    
    -- if: if cond ... elsif cond ... else ... end
    if_stmt = Ct(
        Cc("if") *
        P("if") * ws * Cg(V("complex_expr"), "condition") * skip *
        Cg(Ct(V("statement")^0), "body") *
        Cg(Ct(V("elsif_clause")^0), "elsifs") *
        (P("else") * skip * Cg(Ct(V("statement")^0), "else_body"))^-1 *
        P("end")
    ),
    
    elsif_clause = Ct(
        P("elsif") * ws * Cg(V("complex_expr"), "condition") * skip *
        Cg(Ct(V("statement")^0), "body")
    ),
    
    -- each_pair: target.each_pair do |k, v| ... end
    each_pair_stmt = Ct(
        Cc("each_pair") *
        Cg(C((1 - (ws0 * P(".each_pair")) - P("\n"))^1), "target") * ws0 * P(".each_pair") * skip *
        P("do") * skip * P("|") * ws0 *
        Cg(identifier, "key") * ws0 * P(",") * ws0 * Cg(identifier, "value") * ws0 * P("|") * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    -- each: target.each do |i, v| ... end OR target.each do |v| ... end
    each_stmt = Ct(
        Cc("each") *
        Cg(C((1 - (ws0 * P(".each")) - P("\n"))^1), "target") * ws0 * P(".each") * skip *
        P("do") * skip * P("|") * ws0 *
        (
            -- Two parameters: |index, value|
            (Cg(identifier, "index") * ws0 * P(",") * ws0 * Cg(identifier, "value")) +
            -- One parameter: |value| (index will be nil)
            (Cg(identifier, "value"))
        ) * ws0 * P("|") * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    -- while: while cond ... end
    while_stmt = Ct(
        Cc("while") *
        P("while") * ws * Cg(V("complex_expr"), "condition") * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    -- generic for loop: for vars in expr ... end (no 'do' required, like while)
    for_in_stmt = Ct(
        Cc("for_in") *
        P("for") * ws * Cg(C((1 - P(" in "))^1), "vars") * ws * P("in") * ws *
        Cg(C((1 - S("\n"))^1), "iterator") * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    -- instance variable assignment: @var = value
    instance_var_assign = Ct(
        Cc("instance_var_assign") *
        P("@") * Cg(C(P("super") + P("__name") + identifier), "var") * ws0 *
        P("=") * ws0 * Cg(V("assignment_value"), "value")
    ),
    
    -- super(args)
    super_stmt = Ct(
        Cc("super_stmt") *
        P("super") * P("(") * ws0 * Cg(C((1 - P(")"))^0), "args") * ws0 * P(")")
    ),
    
    -- unary operators: x += 1 or @x += 1
    -- TODO: add /= maybe? I rarely use it, but you might
    unary_op_stmt = Ct(
        Cc("unary_op") *
        (P("@") * Cg(Cc(true), "is_instance") * Cg(C(identifier), "var") + 
         Cg(Cc(false), "is_instance") * Cg(identifier, "var")) * ws0 *
        Cg(C(P("+=") + P("-=") + P("*=")), "op") * ws0 *
        Cg(C((1 - S("\n"))^1), "value")
    ),
    
    -- puts: puts expr (just capture the rest of the line)
    puts_stmt = Ct(
        Cc("puts") *
        P("puts") * ws * Cg(C((1 - S("\n"))^0), "arg")
    ),
    
    -- return: return expr
    return_stmt = Ct(
        Cc("return") *
        P("return") * (ws * Cg(V("return_value"), "value"))^-1
    ),
    
    -- return values (similar to let_values, try structured then raw)
    return_value = V("ternary_expr") + V("lambda_expr") + V("hash_table") + V("array_table") + V("interpolated_string") + C((1 - S("\n"))^1),
    
    -- ternary statement (standalone): expr ? if_true : if_false OR expr ? if_true
    -- Match strings or non-delimiter chars to handle : inside strings
    ternary_stmt = Ct(
        Cc("ternary") *
        Cg(C((any_string + (1 - S("?\n")))^1), "condition") * ws0 * P("?") * ws0 *
        Cg(C((any_string + (1 - S(":\n")))^1), "if_true") * ws0 *
        (P(":") * ws0 * Cg(C((1 - S("\n"))^1), "if_false"))^-1
    ),
    
    -- ternary operator: expr ? if_true : if_false OR expr ? if_true (for use in expressions)
    ternary_expr = Ct(
        Cc("ternary") *
        Cg(C((any_string + (1 - S("?\n")))^1), "condition") * ws0 * P("?") * ws0 *
        Cg(C((any_string + (1 - S(":\n")))^1), "if_true") * ws0 *
        (P(":") * ws0 * Cg(C((1 - S("\n"))^1), "if_false"))^-1
    ),
    
    -- assignment: var = value or array[index] = value or table.field = value
    -- This catches assignments with lambda expressions that other_stmt would miss
    assignment_stmt = Ct(
        Cc("assignment") *
        Cg(C((1 - S("=\n"))^1 * P("=") * ws0), "target") *
        Cg(V("assignment_value"), "value")
    ),
    
    -- values that can be assigned (structured values that need parsing)
    assignment_value = V("ternary_expr") + V("lambda_expr") + V("hash_table") + V("array_table") + V("interpolated_string") + C((1 - S("\n"))^1),
    
    -- other Lua code (pass through) - only matches plain Lua statements  
    -- don't match lines that start with block-ending keywords!
    other_stmt = -((P("end") + P("when") + P("elsif") + P("else")) * (ws + -P(1))) * Ct(
        Cc("lua") *
        Cg(C((1 - S("\n"))^1), "code")
    ),
    
    -- value expressions
    value_expr = V("lambda_expr") + V("hash_table") + V("array_table") + V("interpolated_string") + 
                 V("operator_expr") + V("backslash_call") + V("simple_expr"),
    
    -- expression containing operators (captured as raw Lua)
    -- lookahead: some non-operator chars, then an operator, then anything
    operator_expr = #((1 - S("\n+-*/<>="))^1 * S("+-*/<>=")) * C((1 - S("\n"))^1),
    
    -- backslash method call: \method(args) -> self:method(args)
    backslash_call = Ct(
        Cc("backslash_call") *
        P("\\") * Cg(identifier, "method") * ws0 *
        P("(") * ws0 * Cg(C((1 - P(")"))^0), "args") * P(")")
    ),
    
    -- lambda: lambda do |params| ... end
    lambda_expr = Ct(
        Cc("lambda") *
        P("lambda") * ws0 * P("do") * ws0 *
        (P("|") * ws0 * Cg(Ct((identifier * ws0 * (P(",") * ws0 * identifier * ws0)^0)^-1), "params") * P("|") * ws0)^-1 *
        skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    -- hash: { :key => value, ... }
    hash_table = Ct(
        Cc("hash") *
        P("{") * skip *
        Cg(Ct((V("hash_pair") * skip * (P(",") * skip * V("hash_pair") * skip)^0)^-1), "pairs") *
        P("}")
    ),
    
    hash_pair = Ct(
        P(":") * Cg(identifier, "key") * skip * P("=>") * skip * Cg(V("value_expr"), "value")
    ),
    
    -- array: [ item, ... ] or { item, ... } (backwards compatible)
    array_table = Ct(
        Cc("array") *
        (P("[") + P("{")) * skip *
        Cg(Ct((V("value_expr") * skip * (P(",") * skip * V("value_expr") * skip)^0)^-1), "items") *
        (P("]") + P("}"))
    ),
    
    -- complex expressions (for conditions) - captures until newline, 'then', or 'do'
    complex_expr = V("interpolated_string") + C((1 - S("\n") - P("then") - P("do"))^1),
    
    -- simple expressions  
    simple_expr = V("interpolated_string") +
                  string_dq_literal +
                  V("super_call") +
                  V("instance_var") +
                  V("function_call") +
                  V("method_call") +
                  bool_lit +
                  nil_lit +
                  string_sq +
                  number +
                  identifier,
    
    -- string interpolation: "text #{expr} more"
    interpolated_string = Ct(
        Cc("interpolated") *
        P('"') * Cg(Ct(V("interp_part")^0), "parts") * P('"')
    ),
    
    interp_part = V("interp_expr") + V("interp_literal"),
    
    interp_expr = Ct(
        Cc("interp") *
        P("#{") * Cg(C((1 - P("}"))^1), "expr") * P("}")
    ),
    
    interp_literal = Ct(
        Cc("literal") *
        Cg(C((1 - S('"#') + (P("#") * (1 - P("{"))))^1), "text")
    ),
    
    -- @variable
    instance_var = Ct(
        Cc("instance_var") *
        P("@") * Cg(C(P("super") + P("__name") + identifier), "name")
    ),
    
    -- super(args)
    super_call = Ct(
        Cc("super") *
        P("super") * P("(") * ws0 * Cg(C((1 - P(")"))^0), "args") * ws0 * P(")")
    ),
    
    -- obj.method(args) or obj:method(args)
    method_call = Ct(
        Cc("method_call") *
        Cg(identifier, "object") * Cg(C(S(".:")), "sep") * Cg(identifier, "method") * ws0 *
        P("(") * ws0 * Cg(Ct((V("simple_expr") * ws0 * (P(",") * ws0 * V("simple_expr") * ws0)^0)^-1), "args") * P(")")
    ),
    
    -- func(args)
    function_call = Ct(
        Cc("call") *
        Cg(identifier, "name") * ws0 *
        P("(") * ws0 * Cg(Ct((V("simple_expr") * ws0 * (P(",") * ws0 * V("simple_expr") * ws0)^0)^-1), "args") * P(")")
    ),
}

-- CODE GENERATOR
-- the meat and potatoes

local CodeGen = {}
CodeGen.__index = CodeGen

function CodeGen:new()
    local obj = {
        output = {},
        indent = 0,
        current_class = nil,
        current_method = nil,
        class_seen = false  -- track if we've seen any classes (used for class injection)
    }
    setmetatable(obj, self)
    return obj
end

function CodeGen:indent_str()
    return string.rep("    ", self.indent)
end

function CodeGen:inc_indent()
    self.indent = self.indent + 1
end

function CodeGen:dec_indent()
    self.indent = self.indent - 1
end

function CodeGen:emit_line(line)
    table.insert(self.output, self:indent_str() .. line .. "\n")
end

function CodeGen:emit_class_boilerplate()
    if self.class_seen then
        return  -- Only emit once
    end
    self.class_seen = true
    
    self:emit_line("-- Class implementation")
    self:emit_line("local class = {}")
    self:emit_line("class.__index = class")
    self:emit_line("function class:initialize() end")
    self:emit_line("function class:extend_as(name)")
    self:inc_indent()
    self:emit_line("local cls = {}")
    self:emit_line('cls["__call"] = class.__call')
    self:emit_line("cls.__index = cls")
    self:emit_line("cls.super = self")
    self:emit_line('cls.__name = name or "Anonymoose"')
    self:emit_line("setmetatable(cls, self)")
    self:emit_line("return cls")
    self:dec_indent()
    self:emit_line("end")
    self:emit_line("function class:is(name)")
    self:inc_indent()
    self:emit_line("return self.__name == name")
    self:dec_indent()
    self:emit_line("end")
    self:emit_line("function class:__call(...)")
    self:inc_indent()
    self:emit_line("local inst = setmetatable({}, self)")
    self:emit_line("inst:initialize(...)")
    self:emit_line("return inst")
    self:dec_indent()
    self:emit_line("end")
    self:emit_line("")
end

function CodeGen:generate(ast)
    -- Always emit puts alias (puts -> print)
    self:emit_line("local puts = print")
    self:emit_line("")
    
    -- first pass: check if we have any classes
    for _, node in ipairs(ast) do
        if node[1] == "class" then
            self:emit_class_boilerplate()
            break
        end
    end
    
    -- generate code for all statements
    for _, node in ipairs(ast) do
        self:gen_node(node)
    end
    
    return table.concat(self.output)
end

function CodeGen:gen_node(node)
    local ntype = node[1]
    
    if ntype == "let" then
        self:gen_let(node)
    elseif ntype == "class" then
        self:gen_class(node)
    elseif ntype == "def" then
        self:gen_def(node)
    elseif ntype == "case" then
        self:gen_case(node)
    elseif ntype == "if" then
        self:gen_if(node)
    elseif ntype == "each_pair" then
        self:gen_each_pair(node)
    elseif ntype == "each" then
        self:gen_each(node)
    elseif ntype == "for_in" then
        self:gen_for_in(node)
    elseif ntype == "instance_var_assign" then
        self:gen_instance_var_assign(node)
    elseif ntype == "super_stmt" then
        self:gen_super_stmt(node)
    elseif ntype == "while" then
        self:gen_while(node)
    elseif ntype == "unary_op" then
        self:gen_unary_op(node)
    elseif ntype == "puts" then
        self:gen_puts(node)
    elseif ntype == "return" then
        self:gen_return(node)
    elseif ntype == "ternary" then
        self:gen_ternary(node)
    elseif ntype == "assignment" then
        self:gen_assignment(node)
    elseif ntype == "lua" then
        local code = self:convert_interpolated_strings(node.code)
        code = self:convert_instance_vars(code)
        self:emit_line(code)
    else
        error("Unknown node type: " .. tostring(ntype))
    end
end

function CodeGen:gen_let(node)
    -- convert instance variables in vars
    local vars = self:convert_instance_vars(node.vars)
    
    -- handle values - could be a table (value_expr) or a string (raw)
    local values
    if type(node.values) == "table" then
        values = self:gen_expr(node.values)
    else
        values = self:convert_instance_vars(tostring(node.values))
    end
    
    self:emit_line("local " .. vars .. " = " .. values)
end

function CodeGen:gen_class(node)
    self.current_class = node.name
    
    if node.parent then
        self:emit_line("local " .. node.name .. " = " .. node.parent .. ':extend_as("' .. node.name .. '")')
    else
        self:emit_line("local " .. node.name .. ' = class:extend_as("' .. node.name .. '")')
    end
    
    for _, method in ipairs(node.methods) do
        self:gen_class_method(node.name, method)
    end
    
    self.current_class = nil
end

function CodeGen:gen_class_method(class_name, method)
    self.current_method = method.name
    local params = table.concat(method.params or {}, ", ")
    
    self:emit_line("function " .. class_name .. ":" .. method.name .. "(" .. params .. ")")
    self:inc_indent()
    for _, stmt in ipairs(method.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
    
    self.current_method = nil
end

function CodeGen:gen_def(node)
    local params = table.concat(node.params or {}, ", ")
    local prefix = node.is_local and "local " or ""
    
    self:emit_line(prefix .. "function " .. node.name .. "(" .. params .. ")")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
end

function CodeGen:gen_case(node)
    local case_var = self:gen_expr(node.expr)
    local first = true
    
    for _, when_clause in ipairs(node.whens) do
        local condition = when_clause.condition
        local cond_str
        
        if type(condition) == "table" then
            local ctype = condition[1]
            
            if ctype == "range" then
                cond_str = case_var .. " >= " .. condition.from .. " and " .. case_var .. " <= " .. condition.to
            elseif ctype == "comparison" then
                cond_str = case_var .. " " .. condition.op .. " " .. self:gen_expr(condition.value)
            elseif ctype == "pattern" then
                -- Handle partial string matching
                if condition.type == "prefix" then
                    local len = #condition.prefix
                    cond_str = 'string.sub(' .. case_var .. ', 1, ' .. len .. ') == "' .. condition.prefix .. '"'
                elseif condition.type == "suffix" then
                    local len = #condition.suffix
                    cond_str = 'string.sub(' .. case_var .. ', -' .. len .. ') == "' .. condition.suffix .. '"'
                end
            else
                local expr_val = self:gen_expr(condition)
                cond_str = case_var .. " == " .. expr_val
            end
        else
            cond_str = case_var .. " == " .. tostring(condition)
        end
        
        if first then
            self:emit_line("if " .. cond_str .. " then")
            first = false
        else
            self:emit_line("elseif " .. cond_str .. " then")
        end
        
        self:inc_indent()
        for _, stmt in ipairs(when_clause.body) do
            self:gen_node(stmt)
        end
        self:dec_indent()
    end
    
    if node.else_body then
        self:emit_line("else")
        self:inc_indent()
        for _, stmt in ipairs(node.else_body) do
            self:gen_node(stmt)
        end
        self:dec_indent()
    end
    
    self:emit_line("end")
end

function CodeGen:gen_if(node)
    self:emit_line("if " .. self:gen_expr(node.condition) .. " then")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    
    for _, elsif in ipairs(node.elsifs or {}) do
        self:emit_line("elseif " .. self:gen_expr(elsif.condition) .. " then")
        self:inc_indent()
        for _, stmt in ipairs(elsif.body) do
            self:gen_node(stmt)
        end
        self:dec_indent()
    end
    
    if node.else_body then
        self:emit_line("else")
        self:inc_indent()
        for _, stmt in ipairs(node.else_body) do
            self:gen_node(stmt)
        end
        self:dec_indent()
    end
    
    self:emit_line("end")
end

function CodeGen:gen_each_pair(node)
    self:emit_line("for " .. node.key .. ", " .. node.value .. " in pairs(" .. node.target .. ") do")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
end

function CodeGen:gen_each(node)
    local index = node.index or "_"
    self:emit_line("for " .. index .. ", " .. node.value .. " in ipairs(" .. node.target .. ") do")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
end

function CodeGen:gen_for_in(node)
    local vars = self:convert_instance_vars(node.vars)
    local iterator = self:convert_instance_vars(node.iterator)
    -- comments already stripped in preprocessing
    self:emit_line("for " .. vars .. " in " .. iterator .. " do")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
end

function CodeGen:gen_instance_var_assign(node)
    local var_name = "self." .. node.var
    local value
    
    -- Handle structured values (lambda, hash, array)
    if type(node.value) == "table" then
        value = self:gen_expr(node.value)
    else
        -- strip any leading/trailing whitespace from value and convert instance vars
        value = node.value:match("^%s*(.-)%s*$")
        value = self:convert_instance_vars(value)
    end
    
    self:emit_line(var_name .. " = " .. value)
end

function CodeGen:gen_super_stmt(node)
    if self.current_class and self.current_method then
        self:emit_line(self.current_class .. ".super." .. self.current_method .. "(" .. node.args .. ")")
    else
        error("super() called outside of class method")
    end
end

function CodeGen:gen_while(node)
    self:emit_line("while " .. self:gen_expr(node.condition) .. " do")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
end

function CodeGen:gen_unary_op(node)
    local op_map = {["+="] = "+", ["-="] = "-", ["*="] = "*"}
    local lua_op = op_map[node.op]
    
    local var_name = node.is_instance and ("self." .. node.var) or node.var
    local value = self:convert_instance_vars(node.value)
    -- comments already stripped in preprocessing
    
    self:emit_line(var_name .. " = " .. var_name .. " " .. lua_op .. " " .. value)
end

function CodeGen:gen_puts(node)
    -- Convert interpolation and instance vars, then wrap in parens
    local arg = self:convert_interpolated_strings(node.arg)
    arg = self:convert_instance_vars(arg)
    self:emit_line("puts(" .. arg .. ")")
end

function CodeGen:gen_return(node)
    if node.value then
        local value
        if type(node.value) == "table" then
            value = self:gen_expr(node.value)
        else
            value = self:gen_expr(node.value)
            -- comments already stripped in preprocessing
        end
        self:emit_line("return " .. value)
    else
        self:emit_line("return")
    end
end

function CodeGen:gen_ternary(node)
    -- Ternary: expr ? if_true : if_false OR expr ? if_true
    -- Transpiles to: if expr then if_true else if_false end OR if expr then if_true end
    
    -- Helper to trim whitespace
    local function trim(s)
        return s:match("^%s*(.-)%s*$")
    end
    
    -- Convert interpolation first, then instance vars
    local condition = self:convert_interpolated_strings(trim(node.condition))
    condition = self:convert_instance_vars(condition)
    
    local if_true = self:convert_interpolated_strings(trim(node.if_true))
    if_true = self:convert_instance_vars(if_true)
    
    -- Add parentheses to puts calls for consistency
    if_true = self:wrap_puts_with_parens(if_true)
    
    -- For standalone ternary statements, emit as multi-line if
    self:emit_line("if " .. condition .. " then")
    self:inc_indent()
    self:emit_line(if_true)
    self:dec_indent()
    
    -- Only emit else block if if_false exists
    if node.if_false then
        local if_false = self:convert_interpolated_strings(trim(node.if_false))
        if_false = self:convert_instance_vars(if_false)
        if_false = self:wrap_puts_with_parens(if_false)
        
        self:emit_line("else")
        self:inc_indent()
        self:emit_line(if_false)
        self:dec_indent()
    end
    
    self:emit_line("end")
end

function CodeGen:wrap_puts_with_parens(code)
    -- If code starts with 'puts ' and doesn't already have parens, add them
    -- This ensures consistency in ternary operators
    local trimmed = code:match("^%s*(.-)%s*$")
    if trimmed:match("^puts%s+") and not trimmed:match("^puts%s*%(") then
        -- Extract the argument after 'puts '
        local arg = trimmed:match("^puts%s+(.+)$")
        if arg then
            return "puts(" .. arg .. ")"
        end
    end
    return code
end

function CodeGen:gen_ternary_expr(node)
    -- Ternary expression for use in assignments, returns, etc.
    -- expr ? if_true : if_false -> (expr and if_true or if_false)
    -- expr ? if_true -> (expr and if_true or nil)
    -- Note: This uses Lua's "and/or" idiom which works like a ternary
    
    -- Helper to trim whitespace
    local function trim(s)
        return s:match("^%s*(.-)%s*$")
    end
    
    -- Convert interpolation first, then instance vars
    local condition = self:convert_interpolated_strings(trim(node.condition))
    condition = self:convert_instance_vars(condition)
    
    local if_true = self:convert_interpolated_strings(trim(node.if_true))
    if_true = self:convert_instance_vars(if_true)
    
    local if_false
    if node.if_false then
        if_false = self:convert_interpolated_strings(trim(node.if_false))
        if_false = self:convert_instance_vars(if_false)
    else
        if_false = "nil"
    end
    
    return "(" .. condition .. " and " .. if_true .. " or " .. if_false .. ")"
end

function CodeGen:gen_assignment(node)
    local target = node.target
    local value
    
    -- Handle structured values (lambda, hash, array)
    if type(node.value) == "table" then
        value = self:gen_expr(node.value)
    else
        value = self:convert_instance_vars(tostring(node.value))
            -- comments already stripped in preprocessing
    end
    
    -- Convert instance variables in target
    target = self:convert_instance_vars(target)
    
    self:emit_line(target .. value)
end

function CodeGen:gen_expr(expr)
    if type(expr) ~= "table" then
        -- check if it's a string value captured from grammar
        local str = tostring(expr)
        -- convert instance variables in raw expressions
        str = self:convert_instance_vars(str)
        -- convert comments in raw expressions
        -- comments already stripped in preprocessing
        return str
    end
    
    local etype = expr[1]
    
    if etype == "string" then
        return expr.value
    elseif etype == "ternary" then
        return self:gen_ternary_expr(expr)
    elseif etype == "lambda" then
        return self:gen_lambda(expr)
    elseif etype == "hash" then
        return self:gen_hash(expr)
    elseif etype == "array" then
        return self:gen_array(expr)
    elseif etype == "interpolated" then
        return self:gen_interpolated(expr)
    elseif etype == "instance_var" then
        if expr.name == "__name" then
            return "self.__name"
        elseif expr.name == "super" then
            return "self.super"
        else
            return "self." .. expr.name
        end
    elseif etype == "backslash_call" then
        -- \method(args) -> self:method(args)
        local args = self:convert_instance_vars(expr.args)
        return "self:" .. expr.method .. "(" .. args .. ")"
    elseif etype == "super" then
        -- super(args) - use current class and method context
        if self.current_class and self.current_method then
            return self.current_class .. ".super." .. self.current_method .. "(" .. expr.args .. ")"
        else
            error("super() called outside of class method")
        end
    elseif etype == "method_call" then
        return self:gen_method_call(expr)
    elseif etype == "call" then
        return self:gen_call(expr)
    else
        return tostring(expr)
    end
end

function CodeGen:gen_lambda(expr)
    local params = expr.params or {}
    local result = "function(" .. table.concat(params, ", ") .. ")\n"
    self:inc_indent()
    for _, stmt in ipairs(expr.body) do
        result = result .. self:indent_str()
        local old_output = self.output
        self.output = {}
        self:gen_node(stmt)
        result = result .. table.concat(self.output)
        self.output = old_output
    end
    self:dec_indent()
    result = result .. self:indent_str() .. "end"
    return result
end

function CodeGen:gen_hash(expr)
    if not expr.pairs or #expr.pairs == 0 then
        return "{}"
    end
    
    local parts = {}
    for _, pair in ipairs(expr.pairs) do
        table.insert(parts, pair.key .. " = " .. self:gen_expr(pair.value))
    end
    
    return "{\n" .. self:indent_str() .. "    " .. table.concat(parts, ",\n" .. self:indent_str() .. "    ") .. "\n" .. self:indent_str() .. "}"
end

function CodeGen:gen_array(expr)
    if not expr.items or #expr.items == 0 then
        return "{}"
    end
    
    local parts = {}
    for _, item in ipairs(expr.items) do
        table.insert(parts, self:gen_expr(item))
    end
    
    return "{ " .. table.concat(parts, ", ") .. " }"
end

function CodeGen:gen_interpolated(expr)
    local parts = {}
    for _, part in ipairs(expr.parts) do
        if part[1] == "literal" then
            if part.text and part.text ~= "" then
                table.insert(parts, '"' .. part.text .. '"')
            end
        elseif part[1] == "interp" then
            -- convert @var to self.var in the expression
            local converted = self:convert_instance_vars(part.expr)
            table.insert(parts, converted)
        end
    end
    
    if #parts == 0 then
        return '""'
    elseif #parts == 1 then
        return parts[1]
    else
        return table.concat(parts, " .. ")
    end
end

function CodeGen:convert_operators(expr)
    -- Convert Ruby-style operators to Lua equivalents
    -- != becomes ~=
    return expr:gsub("!=", "~=")
end

function CodeGen:convert_instance_vars(expr)
    -- convert @variable to self.variable
    -- handle @__name and @super specially
    -- FIXME:
    -- thinking about it, I don't think __name and @super need to be 
    -- handled specially. It should just resolve as long as @var -> self.var
    
    -- First convert operators
    expr = self:convert_operators(expr)
    
    -- Then convert \method to self:method
    expr = expr:gsub("\\([%w_]+)", "self:%1")
    
    -- Then convert @variable to self.variable
    return expr:gsub("@(__name)", "self.%1")
               :gsub("@(super)", "self.%1")
               :gsub("@([%w_]+)", "self.%1")
end

function CodeGen:convert_interpolated_strings(code)
    -- convert "text #{expr} more" to "text " .. expr .. " more"
    -- this handles interpolated strings in raw Lua code
    local result = code
    
    -- find all interpolated strings
    result = result:gsub('"([^"]*#%{[^}]+%}[^"]*)"', function(content)
        local parts = {}
        local pos = 1
        
        while pos <= #content do
            -- find next interpolation
            local start_interp, end_interp = content:find("#%{", pos)
            
            if start_interp then
                -- add literal text before interpolation
                if start_interp > pos then
                    local literal = content:sub(pos, start_interp - 1)
                    if literal ~= "" then
                        table.insert(parts, '"' .. literal .. '"')
                    end
                end
                
                -- find the closing }
                local brace_count = 1
                local expr_start = end_interp + 1
                local expr_end = expr_start
                
                while expr_end <= #content and brace_count > 0 do
                    if content:sub(expr_end, expr_end) == '{' then
                        brace_count = brace_count + 1
                    elseif content:sub(expr_end, expr_end) == '}' then
                        brace_count = brace_count - 1
                    end
                    if brace_count > 0 then
                        expr_end = expr_end + 1
                    end
                end
                
                -- extract and convert the expression
                local expr = content:sub(expr_start, expr_end - 1)
                expr = self:convert_instance_vars(expr)
                table.insert(parts, expr)
                
                pos = expr_end + 1
            else
                -- no more interpolations, add remaining literal
                local literal = content:sub(pos)
                if literal ~= "" then
                    table.insert(parts, '"' .. literal .. '"')
                end
                break
            end
        end
        
        if #parts == 0 then
            return '""'
        elseif #parts == 1 then
            return parts[1]
        else
            return table.concat(parts, " .. ")
        end
    end)
    
    return result
end

function CodeGen:gen_method_call(expr)
    local args = {}
    for _, arg in ipairs(expr.args or {}) do
        table.insert(args, self:gen_expr(arg))
    end
    return expr.object .. expr.sep .. expr.method .. "(" .. table.concat(args, ", ") .. ")"
end

function CodeGen:gen_call(expr)
    local args = {}
    for _, arg in ipairs(expr.args or {}) do
        table.insert(args, self:gen_expr(arg))
    end
    return expr.name .. "(" .. table.concat(args, ", ") .. ")"
end

-- TRANSPILING

-- ERROR CHECKING

local ErrorChecker = {}
ErrorChecker.__index = ErrorChecker

function ErrorChecker:new()
    local obj = {
        errors = {},
        state = {
            in_case = false,
            in_class = false,
            in_class_method = false,
            block_stack = {}  -- track blocks with {type, line_num}
        }
    }
    setmetatable(obj, self)
    return obj
end

function ErrorChecker:add_error(line_num, message)
    table.insert(self.errors, {line = line_num, msg = message})
end

function ErrorChecker:push_block(block_type, line_num)
    table.insert(self.state.block_stack, {type = block_type, line = line_num})
end

function ErrorChecker:pop_block(expected_type, line_num)
    if #self.state.block_stack == 0 then
        self:add_error(line_num, "Unexpected 'end' - no matching block to close")
        return false
    end
    
    local block = table.remove(self.state.block_stack)
    if expected_type and block.type ~= expected_type then
        self:add_error(line_num, 
            string.format("Mismatched 'end' - expected to close '%s' from line %d, but context suggests '%s'",
                block.type, block.line, expected_type))
        return false
    end
    
    return true
end

function ErrorChecker:check_line(line, line_num)
    -- First check if this is a full-line comment (starts with #)
    local trimmed_full = line:match("^%s*(.-)%s*$")
    if trimmed_full:match("^#") then
        return  -- Skip full-line comments
    end
    
    -- For non-comment lines, remove inline comments (everything after #)
    local code_part = line:match("^([^#]-)%s*#") or line
    local trimmed = code_part:match("^%s*(.-)%s*$")
    
    -- Skip empty lines
    if trimmed == "" then
        return
    end
    
    -- Check for 'when' without 'case'
    if trimmed:match("^when%s+") then
        if not self.state.in_case then
            self:add_error(line_num, "Used 'when' outside of a 'case' statement")
        end
    end
    
    -- Check for 'else' without proper context
    if trimmed:match("^else%s*$") or trimmed:match("^else%s*#") then
        if not self.state.in_case and #self.state.block_stack > 0 then
            local top = self.state.block_stack[#self.state.block_stack]
            if top.type ~= "if" and top.type ~= "elsif" then
                self:add_error(line_num, "Used 'else' outside of 'if' or 'case' statement")
            end
        elseif #self.state.block_stack == 0 then
            self:add_error(line_num, "Used 'else' outside of any control structure")
        end
    end
    
    -- Check for 'super' outside class method
    if trimmed:match("super%s*%(") then
        if not self.state.in_class_method then
            self:add_error(line_num, "Used 'super' outside of a class method")
        end
    end
    
    -- Check for instance variables outside class
    local instance_var = trimmed:match("@([%w_]+)")
    if instance_var then
        -- Allow @super and @__name as special cases
        if instance_var ~= "super" and instance_var ~= "__name" then
            if not self.state.in_class then
                self:add_error(line_num, 
                    string.format("Used instance variable '@%s' outside of a class", instance_var))
            end
        end
    end
    
    -- Check for malformed hash table syntax (key => value without :)
    -- Look for "word =>" pattern where word is NOT preceded by :
    if trimmed:match("=>") then
        -- Check if there's a bare word (not preceded by :) before =>
        local before_arrow = trimmed:match("([^,{]+)%s*=>")
        if before_arrow then
            -- Check if this segment has a word but no leading colon
            if before_arrow:match("%w") and not before_arrow:match(":") then
                self:add_error(line_num, 
                    "Invalid hash table syntax - keys must be symbols (use ':key => value', not 'key => value')")
            end
        end
    end
    
    -- Check for :key = instead of :key =>
    if trimmed:match(":%s*%w+%s*=") and not trimmed:match(":%s*%w+%s*=>") then
        self:add_error(line_num,
            "Invalid hash table syntax - use '=>' for hash pairs (not '=')")
    end
    
    -- Track block starts
    if trimmed:match("^case%s+") then
        self.state.in_case = true
        self:push_block("case", line_num)
    elseif trimmed:match("^if%s+") then
        self:push_block("if", line_num)
    elseif trimmed:match("^elsif%s+") then
        -- Replace 'if' with 'elsif' on stack
        if #self.state.block_stack > 0 and self.state.block_stack[#self.state.block_stack].type == "if" then
            self.state.block_stack[#self.state.block_stack].type = "elsif"
        end
    elseif trimmed:match("^while%s+") then
        self:push_block("while", line_num)
    elseif trimmed:match("^for%s+") and trimmed:match("%s+in%s+") then
        self:push_block("for_in", line_num)
    elseif trimmed:match("^class%s+") then
        self.state.in_class = true
        self:push_block("class", line_num)
    elseif trimmed:match("^def%s+") then
        if self.state.in_class then
            self.state.in_class_method = true
            self:push_block("class_method", line_num)
        else
            self:push_block("def", line_num)
        end
    elseif trimmed:match("lambda%s+do") then
        self:push_block("lambda", line_num)
    elseif trimmed:match("%.each_pair%s+do") then
        self:push_block("each_pair", line_num)
    elseif trimmed:match("%.each%s+do") then
        self:push_block("each", line_num)
    end
    
    -- Track block ends
    if trimmed:match("^end%s*$") or trimmed:match("^end%s*#") then
        if #self.state.block_stack > 0 then
            local block = self.state.block_stack[#self.state.block_stack]
            
            -- Update state when closing blocks
            if block.type == "case" then
                self.state.in_case = false
            elseif block.type == "class" then
                self.state.in_class = false
                self.state.in_class_method = false
            elseif block.type == "class_method" then
                self.state.in_class_method = false
            end
            
            self:pop_block(nil, line_num)
        else
            self:add_error(line_num, "Unexpected 'end' - no matching block to close")
        end
    end
end

function ErrorChecker:check_source(source)
    local line_num = 1
    -- Split source into lines properly
    for line in (source .. "\n"):gmatch("([^\n]*)\n") do
        self:check_line(line, line_num)
        line_num = line_num + 1
    end
    
    -- Check for unclosed blocks
    if #self.state.block_stack > 0 then
        for _, block in ipairs(self.state.block_stack) do
            self:add_error(block.line, 
                string.format("Unclosed '%s' block - missing 'end'", block.type))
        end
    end
    
    return #self.errors == 0
end

function ErrorChecker:get_error_report()
    if #self.errors == 0 then
        return nil
    end
    
    -- Sort errors by line number
    table.sort(self.errors, function(a, b) return a.line < b.line end)
    
    local report = {"Luby syntax errors found:\n"}
    for _, err in ipairs(self.errors) do
        table.insert(report, string.format("  Line %d: %s", err.line, err.msg))
    end
    
    return table.concat(report, "\n")
end

function transpile(source)
    -- Preprocessing: Strip all # comments before parsing
    -- This is simpler and more reliable than trying to convert them
    source = strip_comments(source)
    
    -- Error checking
    local checker = ErrorChecker:new()
    local valid = checker:check_source(source)
    
    if not valid then
        local error_report = checker:get_error_report()
        error(error_report)
    end
    
    -- Original transpilation
    local ast = grammar:match(source)
    
    if not ast then
        error("Failed to parse source")
    end
    
    local codegen = CodeGen:new()
    return codegen:generate(ast)
end

function strip_comments(source)
    local result = {}
    local in_string = false
    local string_char = nil
    local in_interp = false
    local i = 1
    
    while i <= #source do
        local char = source:sub(i, i)
        local next_char = source:sub(i+1, i+1)
        
        -- Track string boundaries
        if (char == '"' or char == "'") and (i == 1 or source:sub(i-1, i-1) ~= "\\") then
            if not in_string then
                in_string = true
                string_char = char
            elseif char == string_char and not in_interp then
                in_string = false
                string_char = nil
            end
        end
        
        -- Track interpolation inside double-quoted strings
        if in_string and string_char == '"' then
            if char == '#' and next_char == '{' then
                in_interp = true
            elseif char == '}' and in_interp then
                in_interp = false
            end
        end
        
        -- Remove comments outside of strings and interpolations
        if char == '#' and not in_string and not in_interp then
            -- Skip the rest of the line (the comment)
            while i <= #source and source:sub(i, i) ~= '\n' do
                i = i + 1
            end
            -- Keep the newline
            if i <= #source then
                table.insert(result, '\n')
            end
        else
            table.insert(result, char)
        end
        
        i = i + 1
    end
    
    return table.concat(result)
end

-- my super sketch "lfs" implementation
local lfs = require("llfs")
local SOURCE_EXT = ".rb"  -- using rb for now as it plays nicely with syntax highlighting

-- get all files recursively from directory
local function get_files_recursive(dir, files)
    files = files or {}
    local f = lfs.dir(dir)
    for _,entry in ipairs(f) do
        if entry ~= "." and entry ~= ".." then
            local filepath = dir .. "/" .. entry
            if lfs.is_dir(filepath) then
                get_files_recursive(filepath, files)
            elseif filepath:match(SOURCE_EXT .. "$") then
                table.insert(files, filepath)
            end
        end
    end
    return files
end

-- get relative path from source directory
local function get_relative_path(filepath, basedir)
    -- normalize paths
    basedir = basedir:gsub("/$", "")
    -- remove basedir prefix and leading slash
    local relative = filepath:gsub("^" .. basedir:gsub("([^%w])", "%%%1") .. "/?", "")
    -- remove any leading slashes
    relative = relative:gsub("^/+", "")
    return relative
end

-- ensure directory exists
local function ensure_directory(filepath)
    local parts = {}
    for part in filepath:gmatch("[^/]+") do
        table.insert(parts, part)
    end
    
    -- remove filename (last part)
    table.remove(parts)
    
    -- create directories incrementally
    local path = ""
    for _, part in ipairs(parts) do
        path = path == "" and part or (path .. "/" .. part)
        if not lfs.path_exists(path) then
            lfs.mkdir(path)
        end
    end
end

-- MAIN EXECUTION

if #arg < 1 then
    io.stderr:write("Usage: lua luby.lua <source_file_or_directory> [run]\n")
    io.stderr:write("  If source is a file and 'run' is specified, execute the transpiled code\n")
    io.stderr:write("  If source is a directory, recursively transpile all *" .. SOURCE_EXT .. " files\n")
    os.exit(1)
end

local source_path = arg[1]
local run = arg[2]

if lfs.is_dir(source_path) then
    -- directory mode: recursively transpile all files
    local files = get_files_recursive(source_path)
    
    if #files == 0 then
        io.stderr:write("No *" .. SOURCE_EXT .. " files found in directory: " .. source_path .. "\n")
        os.exit(1)
    end
    
    io.stderr:write("Transpiling " .. #files .. " file(s)...\n")
    
    for _, filepath in ipairs(files) do
        local file = io.open(filepath, "r")
        if not file then
            io.stderr:write("Error: cannot open file '" .. filepath .. "'\n")
            goto continue
        end
        
        local source = file:read("*all")
        file:close()
        
        local success, result = pcall(transpile, source)
        if not success then
            io.stderr:write("Transpilation error in " .. filepath .. ": " .. result .. "\n")
            goto continue
        end
        
        -- determine output path
        local relative = get_relative_path(filepath, source_path)
        local output = relative:gsub(SOURCE_EXT .. "$", ".lua")
        
        -- ensure output directory exists
        ensure_directory(output)
        
        -- Write output file
        local outfile = io.open(output, "w")
        if not outfile then
            io.stderr:write("Error: cannot write to '" .. output .. "'\n")
            goto continue
        end
        
        outfile:write(result)
        outfile:write("\n")
        outfile:close()
        
        io.stderr:write("  " .. filepath .. " -> " .. output .. "\n")
        
        ::continue::
    end
    
    io.stderr:write("Done!\n")
else
    -- single file mode
    local file = io.open(source_path, "r")
    if not file then
        io.stderr:write("Error: cannot open file '" .. source_path .. "'\n")
        os.exit(1)
    end

    local source = file:read("*all")
    file:close()

    local success, result = pcall(transpile, source)
    if success then
        if run then
            -- execute mode
            local s = load(result)
            s()
        else
            -- output mode
            io.write(result)
            io.write("\n")
        end
    else
        io.stderr:write("Transpilation error: " .. result .. "\n")
        os.exit(1)
    end
end