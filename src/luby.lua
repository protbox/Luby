--[[
Luby - A Ruby-esque language that transpiles to Lua
Not a lot/any error checking currently, so it will just convert anything 
you write, but parsing is pretty solid at least.
Pass this script a filename to print the transpiled code to stdout
Pass it a folder to recursively transpile all *.rb files to the current working directory 
(the folder structures will remain in tact)
- poltergasm
]]

-- try to load lpeg, fall back to lulpeg if not available
local lpeg
local ok, mod = pcall(require, "lpeg")
if ok then
    lpeg = mod
else
    lpeg = require("../deps/lulpeg/lulpeg")
end

local P, R, S, V, C, Ct, Cc, Cg = 
    lpeg.P, lpeg.R, lpeg.S, lpeg.V, lpeg.C, lpeg.Ct, lpeg.Cc, lpeg.Cg

-- LEXICAL ELEMENTS

local ws = S(" \t\n\r")^1  -- at least one whitespace
local ws0 = S(" \t\n\r")^0  -- zero or more whitespace
local comment = P("#") * (1 - P("\n"))^0
local skip = (ws + comment)^0

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
                 V("each_pair_stmt") +
                 V("each_stmt") +
                 V("instance_var_assign") +
                 V("super_stmt") +
                 V("unary_op_stmt") +
                 V("puts_stmt") +
                 V("return_stmt") +
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
    let_values = V("lambda_expr") + V("hash_table") + V("array_table") + C((1 - S("\n#"))^1),
    
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
        Cg(identifier, "target") * ws0 * P(".each_pair") * skip *
        P("do") * skip * P("|") * ws0 *
        Cg(identifier, "key") * ws0 * P(",") * ws0 * Cg(identifier, "value") * ws0 * P("|") * skip *
        Cg(Ct(V("statement")^0), "body") *
        P("end")
    ),
    
    -- each: target.each do |i, v| ... end
    each_stmt = Ct(
        Cc("each") *
        Cg(identifier, "target") * ws0 * P(".each") * skip *
        P("do") * skip * P("|") * ws0 *
        Cg(identifier, "index") * ws0 * P(",") * ws0 * Cg(identifier, "value") * ws0 * P("|") * skip *
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
    
    -- instance variable assignment: @var = value
    instance_var_assign = Ct(
        Cc("instance_var_assign") *
        P("@") * Cg(C(P("super") + P("__name") + identifier), "var") * ws0 *
        P("=") * ws0 * Cg(C((1 - S("\n#"))^1), "value")
    ),
    
    -- super(self)
    super_stmt = Ct(
        Cc("super_stmt") *
        P("super") * P("(") * ws0 * Cg(C(P("self")), "arg") * ws0 * P(")")
    ),
    
    -- unary operators: x += 1 or @x += 1
    -- TODO: add /= maybe? I rarely use it, but you might
    unary_op_stmt = Ct(
        Cc("unary_op") *
        (P("@") * Cg(Cc(true), "is_instance") * Cg(C(identifier), "var") + 
         Cg(Cc(false), "is_instance") * Cg(identifier, "var")) * ws0 *
        Cg(C(P("+=") + P("-=") + P("*=")), "op") * ws0 *
        Cg(C((1 - S("\n#"))^1), "value")
    ),
    
    -- puts: puts expr
    puts_stmt = Ct(
        Cc("puts") *
        P("puts") * ws * Cg(V("value_expr"), "arg")
    ),
    
    -- return: return expr
    return_stmt = Ct(
        Cc("return") *
        P("return") * (ws * Cg(V("return_value"), "value"))^-1
    ),
    
    -- return values (similar to let_values, try structured then raw)
    return_value = V("lambda_expr") + V("hash_table") + V("array_table") + V("interpolated_string") + C((1 - S("\n#"))^1),
    
    -- other Lua code (pass through) - only matches plain Lua statements  
    -- don't match lines that start with block-ending keywords!
    other_stmt = -((P("end") + P("when") + P("elsif") + P("else")) * (ws + -P(1))) * Ct(
        Cc("lua") *
        Cg(C((1 - S("\n"))^1), "code")
    ),
    
    -- value expressions
    value_expr = V("lambda_expr") + V("hash_table") + V("array_table") + V("interpolated_string") + 
                 V("operator_expr") + V("simple_expr"),
    
    -- expression containing operators (captured as raw Lua)
    -- lookahead: some non-operator chars, then an operator, then anything
    operator_expr = #((1 - S("\n#+-*/<>="))^1 * S("+-*/<>=")) * C((1 - S("\n#"))^1),
    
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
        P("{") * ws0 *
        Cg(Ct((V("hash_pair") * ws0 * (P(",") * ws0 * V("hash_pair") * ws0)^0)^-1), "pairs") *
        P("}")
    ),
    
    hash_pair = Ct(
        P(":") * Cg(identifier, "key") * ws0 * P("=>") * ws0 * Cg(V("value_expr"), "value")
    ),
    
    -- array: { item, ... }
    -- TODO: potentially change this to [] to differentiate it from hashes
    array_table = Ct(
        Cc("array") *
        P("{") * ws0 *
        Cg(Ct((V("value_expr") * ws0 * (P(",") * ws0 * V("value_expr") * ws0)^0)^-1), "items") *
        P("}")
    ),
    
    -- complex expressions (for conditions) - captures until newline, 'then', 'do', or comment
    complex_expr = V("interpolated_string") + C((1 - S("\n#") - P("then") - P("do"))^1),
    
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
    
    -- super(self)
    super_call = Ct(
        Cc("super") *
        P("super") * P("(") * ws0 * Cg(C(P("self")), "arg") * ws0 * P(")")
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
    self:emit_line("for " .. node.index .. ", " .. node.value .. " in ipairs(" .. node.target .. ") do")
    self:inc_indent()
    for _, stmt in ipairs(node.body) do
        self:gen_node(stmt)
    end
    self:dec_indent()
    self:emit_line("end")
end

function CodeGen:gen_instance_var_assign(node)
    local var_name = "self." .. node.var
    -- strip any leading/trailing whitespace from value and convert instance vars
    local value = node.value:match("^%s*(.-)%s*$")
    value = self:convert_instance_vars(value)
    self:emit_line(var_name .. " = " .. value)
end

function CodeGen:gen_super_stmt(node)
    if self.current_class and self.current_method then
        self:emit_line(self.current_class .. ".super." .. self.current_method .. "(" .. node.arg .. ")")
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
    
    self:emit_line(var_name .. " = " .. var_name .. " " .. lua_op .. " " .. value)
end

function CodeGen:gen_puts(node)
    self:emit_line("print(" .. self:gen_expr(node.arg) .. ")")
end

function CodeGen:gen_return(node)
    if node.value then
        self:emit_line("return " .. self:gen_expr(node.value))
    else
        self:emit_line("return")
    end
end

function CodeGen:gen_expr(expr)
    if type(expr) ~= "table" then
        -- check if it's a string value captured from grammar
        local str = tostring(expr)
        -- convert instance variables in raw expressions
        str = self:convert_instance_vars(str)
        return str
    end
    
    local etype = expr[1]
    
    if etype == "string" then
        return expr.value
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
    elseif etype == "super" then
        -- super(self) - use current class and method context
        if self.current_class and self.current_method then
            return self.current_class .. ".super." .. self.current_method .. "(" .. expr.arg .. ")"
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

function CodeGen:convert_instance_vars(expr)
    -- convert @variable to self.variable
    -- handle @__name and @super specially
    -- FIXME:
    -- thinking about it, I don't think __name and @super need to be 
    -- handled specially. It should just resolve as long as @var -> self.var
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

function transpile(source)
    local ast = grammar:match(source)
    
    if not ast then
        error("Failed to parse source")
    end
    
    local codegen = CodeGen:new()
    return codegen:generate(ast)
end

-- lfs is necessary for cross-platform file/directory operations
local lfs = require("lfs")
local SOURCE_EXT = ".rb"  -- file extension to look for

-- check if path is a directory
local function is_directory(path)
    local attr = lfs.attributes(path)
    return attr and attr.mode == "directory"
end

-- get all files recursively from directory
local function get_files_recursive(dir, files)
    files = files or {}
    for entry in lfs.dir(dir) do
        if entry ~= "." and entry ~= ".." then
            local filepath = dir .. "/" .. entry
            local attr = lfs.attributes(filepath)
            if attr then
                if attr.mode == "directory" then
                    get_files_recursive(filepath, files)
                elseif attr.mode == "file" and filepath:match(SOURCE_EXT .. "$") then
                    table.insert(files, filepath)
                end
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
        local attr = lfs.attributes(path)
        if not attr then
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

if is_directory(source_path) then
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