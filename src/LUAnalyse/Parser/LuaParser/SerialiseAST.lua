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
	return {ctor = 'Just', args = {arg}}
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
	    ctor = 'FunctionExpr'
		args = {handleNameList(expr.Arguments), handleBlock(expr.Body)}

	elseif expr.AstType == 'ConstructorExpr' then
		local content = {}
		local arrayIndex = 1
		for k,entry in pairs(expr.EntryList) do
			if entry.Type == 'KeyString' then
			    local str = {ctor = "StringExpr", args = {formatString(entry.Key)}}
			    
				table.insert(content, {str, handleExpr(entry.Value)})
			elseif entry.Type == 'Key' then
				table.insert(content, {handleExpr(entry.Key), handleExpr(entry.Value)})
			elseif entry.Type == 'Value' then
			    local num = {ctor = "NumberExpr", args = {arrayIndex}}
			    
				table.insert(content, {num, handleExpr(entry.Value)})
				arrayIndex = arrayIndex + 1
			end
		end

		local listArgs = {}
		for k,v in pairs(content) do
			listArgs[k] = {ctor = 'Pair', args = v}
		end
		
		args = {{ctor = 'List', args = listArgs}}
	end

	return {ctor = ctor, args = args}
end

handleStatement = function(stat)
	local ctor = stat.AstType
	local args

	if stat.AstType == 'AssignmentStatement' then
		args = {handleExprList(stat.Lhs), handleExprList(stat.Rhs)}

	elseif stat.AstType == 'CallStatement' then
		args = {handleExpr(stat.Expression)}

	elseif stat.AstType == 'LocalStatement' then
		args = {handleNameList(stat.LocalList), handleExprList(stat.InitList)}

	elseif stat.AstType == 'IfStatement' then
		args = {handleExpr(stat.Clauses[1].Condition),
				handleBlock(stat.Clauses[1].Body),
				nothing()}

		if #stat.Clauses == 2  and not stat.Clauses[2].Condition then
			args[3] = just(handleBlock(stat.Clauses[2].Body))
		elseif #stat.Clauses >= 2 then
			table.remove(stat.Clauses, 1)
			args[3] = just(handleStatement(stat))
		end

	elseif stat.AstType == 'WhileStatement' then
		args = {handleExpr(stat.Condition), handleBlock(stat.Body)}

	elseif stat.AstType == 'DoStatement' then
		args = {handleBlock(stat.Body)}

	elseif stat.AstType == 'ReturnStatement' then
		args = {handleExprList(stat.Arguments)}

	elseif stat.AstType == 'BreakStatement' then
		args = {}

	elseif stat.AstType == 'RepeatStatement' then
		args = {handleBlock(stat.Body), handleExpr(stat.Condition)}

	elseif stat.AstType == 'Function' then
		args = {
			(stat.IsLocal and 'True') or 'False',
			stat.Name.Name,
			handleNameList(stat.Arguments), --TODO?: varargs
			handleBlock(stat.Body)
		}

	elseif stat.AstType == 'GenericForStatement' then
		args = {
			handleNameList(stat.VariableList),
			handleExprList(stat.Generators),
			handleBlock(stat.Body)
		}

	elseif stat.AstType == 'NumericForStatement' then
		args = {
			stat.Variable.Name,
			handleExpr(stat.Start),
			handleExpr(stat.End),
			(stat.Step and just(handleExpr(stat.Step))) or nothing(),
			handleBlock(stat.Body)
		}

	end

	return {ctor = ctor, args = args}
end

handleBlock = function(statList)
	local stats = {}
	for _,stat in pairs(statList.Body) do
		table.insert(stats, handleStatement(stat))
	end

	return {ctor = 'StatList', args = {{ctor = 'List', args = stats}}}
end


-- Stringifies a formatted AST.
stringify = function(root)
	--dump(root)
	if type(root) == 'string' then
		return root
	elseif type(root) == 'number' then
		return tostring(root)
	elseif type(root) == 'table' then
		local argStr = '('
		local first = true
		
		if root.args == nil then
		    return 'nil'
		else
		    for k,v in pairs(root.args) do
			    if not first then argStr = argStr .. ',' end
			    first = false
			    
			    argStr = argStr .. stringify(root.args[k])
		    end
		    argStr = argStr .. ')'

		    return root.ctor .. argStr
		end
	else
		error('Invalid AST type.')
	end
end

-- Do the actual serialisation on the AST and print the result to stdout.
local formatted = handleBlock(ast)

print(stringify(formatted))
