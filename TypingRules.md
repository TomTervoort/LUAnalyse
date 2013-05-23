# Typing rules #

Note: `{a}` means "type of `a`".

Note: when a "given" predicate is violated, an error occurs.
Of course, we ignore the existance of metatables.

## Miscellaneous ##

    Assign      {a} = {b}
    Const       {a} = {constant}
    Call        {a} = {return-of b}         given {b} < function
    Length      {a} = number (nat!) (fin?)  given {b} sequence-coercible
    Concat      {a} = string                given {b},{c} string-coercible

## Table operations ##

Given: `{b} < table`

    Member      {a} = {b[name]} \/ nil
    Index       {a} = {b[name]} \/ nil
    NewMember   {a} = {a >>= \x -> x[name] is-a {b}}
    NewIndex    {a} = {a >>= \x -> x[name] is-a {b}}

## Arithmetic ##

Given: `{b}` and `{c}` are number-coercible

    Add         {a} = number
    Sub         {a} = number
    Mul         {a} = number
    Div         {a} = number
    Pow         {a} = number
    Mod         {a} = number
    Minus       {a} = number

## Equality ##

Note: operands of inequal types are inequal.

Note: tables are always inequal; this breaks (anti)reflexivity.

    Eq          {a} = boolean
    NotEq       {a} = boolean

## Comparison ##

Given: both operand types are equal, and either a number or a string.

    Less        {a} = boolean    
    Greater     {a} = boolean
    LessEq      {a} = boolean
    GreaterEq   {a} = boolean

## Logic ##

Note: operands can be of any type; only false and nil are considered false.

    Not         {a} = boolean
    And         {a} = {b} \/ {c}
    Or          {a} = {b} \/ {c}
