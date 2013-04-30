#!/usr/bin/lua

-- TODO: remove debugging stuff:
-- local inspect = require'inspect' 
-- local function dump(x) print(inspect(x)) end

local Parser = require'ParseLua'

-- Read entire source file into memory.
local file = io.open(arg[1], 'r')
local source = file:read('*all')
file:close()

-- Parse it.
local success, ast = Parser.ParseLua(source)
if not success then
	print('Parse error.')
	os.exit(1)
end

-- Forward-declaration of functions.
local formatString
local just
local nothing
local handleExpr
local handleExprList
local handleNameList
local handleStatement
local handleBlock
local formatString
local stringify

-- Adds quotes and escapes them to get a formatted string literal.
formatString = function(str)
	return "'" .. string.gsub(str, "'", "''") .. "'"
end

-- Helpers for denoting optional values.
just = function(arg)
	return {ctor = 'Just', args = arg}
end

nothing = function()
	return {ctor = 'Nothing', args = {}}
end

handleExprList = function(list)
	if not list then
		return {}
	end
	for k,v in pairs(list) do
		list[k] = handleExpr(v)
	end
	return {ctor = 'List', args = list}
end

handleNameList = function(list)
	if not list then
		return {}
	end
	for k,v in pairs(list) do
		list[k] = v.Name
	end
	return {ctor = 'List', args = list}
end

-- Functions converting given AST => constructor-arguments pair.
-- In some cases they may mutate their argument, so do not re-use elements provided as arguments to
-- these functions. That should not be neccessary anyway.
handleOperator = function(op)
	return {ctor = 'Operator', args = {formatString(op)}}
end

handleExpr = function(expr)
	local ctor = expr.AstType
	local args

	if expr.AstType == 'VarExpr' then
		args = {(expr.Variable and expr.Variable.Name) or expr.Name}

	elseif expr.AstType == 'NumberExpr' then
		args = {expr.Value.Data}

	elseif expr.AstType == 'StringExpr' then
		args = {formatString(expr.Value.Data)}

	elseif expr.AstType == 'BooleanExpr' then
		args = {(expr.Value and 'True') or 'False'}

	elseif expr.AstType == 'NilExpr' then
		args = {}

	elseif expr.AstType == 'BinopExpr' then
		args = {handleExpr(expr.Lhs), handleOperator(expr.Op), handleExpr(expr.Rhs)}

	elseif expr.AstType == 'UnopExpr' then
		args = {handleOperator(expr.Op), handleExpr(expr.Rhs)}

	elseif expr.AstType == 'DotsExpr' then
		args = {}

	elseif expr.AstType == 'CallExpr' then
		args = {handleExpr(expr.Base), handleExprList(expr.Arguments)}

	elseif expr.AstType == 'TableCallExpr' then
		-- Merely syntactic sugar. Treat as CallExpr.
		ctor = 'CallExpr'
		args = {handleExpr(expr.Base), handleExprList(expr.Arguments)}

	elseif expr.AstType == 'StringCallExpr' then
		-- Merely syntactic sugar. Treat as CallExpr.
		ctor = 'CallExpr'
		args = {handleExpr(expr.Base), handleExprList(expr.Arguments)}

	elseif expr.AstType == 'IndexExpr' then
		args = {handleExpr(expr.Base), handleExpr(expr.Index)}

	elseif expr.AstType == 'MemberExpr' then
		args = {handleExpr(expr.Base), expr.Ident.Data}

	elseif expr.AstType == 'Function' then
		args = {handleNameList(expr.Arguments), handleBlock(expr.Body)}

	elseif expr.AstType == 'ConstructorExpr' then
		local content = {}
		local arrayIndex = 1
		for entry in Expr.EntryList do
			if entry.Type == 'Key' or entry.Type == 'KeyString' then
				table.insert(content, {formatString(entry.Key), handleExpr(entry.Value)})
			elseif entry.Type == 'Value' then
				table.insert(content, {arrayIndex, handleExpr(entry.Value)})
				arrayIndex = arrayIndex + 1
			end
		end

		listArgs = {}
		for k,v in pairs(content) do
			listArgs[k] = {ctor = 'Pair', args = v}
		end
		args = {ctor = 'List', args = listArgs}
	end

	return {ctor = ctor, args = args}
end

handleStatement = function(stat)
	local ctor = stat.AstType
	local args

	if stat.AstType == 'AssignmentStatement' then
		args = {lhs = stat.Lhs, rhs = handleExprList(stat.Rhs)}

	elseif stat.AstType == 'CallStatement' then
		args = {exp = handleExpr(stat.Expression)}

	elseif stat.AstType == 'LocalStatement' then
		args = {locals = handleNameList(stat.LocalList), inits = handleExprList(stat.InitList)}

	elseif stat.AstType == 'IfStatement' then
		args = {condition = handleExpr(stat.Clauses[1].Condition), 
				thenBody  = handleBlock(stat.Clauses[1].Body),
				elseBody = nothing()}

		if #stat.Clauses == 2  and not stat.Clauses[2].Condition then
			args.elseBody = just(handleBlock(stat.Clauses[2].Body))
		elseif #stat.Clauses >= 2 then
			table.remove(stat.Clauses, 1)
			args.elseBody = just(handleStatement(stat))
		end

	elseif stat.AstType == 'WhileStatement' then
		args = {condition = stat.Condition, body = handleBlock(stat.Body)}

	elseif stat.AstType == 'DoStatement' then
		args = {body = handleBlock(stat.Body)}

	elseif stat.AstType == 'ReturnStatement' then
		args = {args = handleExprList(stat.Arguments)}

	elseif stat.AstType == 'BreakStatement' then
		args = {}

	elseif stat.AstType == 'RepeatStatement' then
		args = {body = handleBlock(stat.Body), condition = handleExpr(stat.Condition)}

	elseif stat.AstType == 'Function' then
		args = {
			isLocal = (stat.IsLocal and 'True') or 'False',
			name    = stat.Name.Name,
			argList = handleNameList(stat.Arguments), --TODO?: varargs
			body    = handleBlock(stat.Body)
		}

	elseif stat.AstType == 'GenericForStatement' then
		args = {
			vars       = handleNameList(stat.VariableList),
			generators = handleExprList(stat.Generators),
			body       = handleBlock(stat.Body)
		}

	elseif stat.AstType == 'NumericForStatement' then
		args = {
			var     = stat.Variable.Name,
			start   = handleExpr(stat.Start),
			['end'] = handleExpr(stat.End),
			step    = (stat.Step and just(handleExpr(stat.Step))) or nothing(),
			body    = handleBlock(stat.Body)
		}

	end

	return {ctor = ctor, args = args}
end

handleBlock = function(statList)
	local stats = {}
	for _,stat in pairs(statList.Body) do
		table.insert(stats, handleStatement(stat))
	end

	return {ctor =  'StatList', args = {{ctor = 'List', args = stats}}}
end


-- Stringifies a formatted AST.
stringify = function(root)
	-- dump(root)
	if type(root) == 'string' then
		return root
	elseif type(root) == 'number' then
		return tostring(root)
	elseif type(root) == 'table' then
		local argStr = '('
		local first = true
		for k,v in pairs(root.args) do
			if not first then argStr = argStr .. ',' end
			first = false
			argStr = argStr .. stringify(root.args[k])
		end
		argStr = argStr .. ')'

		return root.ctor .. argStr
	else
		error('Invalid AST type.')
	end
end

-- Do the actual serialisation on the AST and print the result to stdout.
local formatted = handleBlock(ast)
-- dump(formatted)
print(stringify(formatted))
