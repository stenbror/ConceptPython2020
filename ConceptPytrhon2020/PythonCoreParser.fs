
namespace PythonCoreFramework.Runtime

module PythonCoreParser =

    type Trivia =
        |   Empty

    
    type Token =
        |   Invalid
        |   Newline of uint32 * uint32 * Trivia array
        |   Indent
        |   Dedent
        |   PyFalse of uint32 * uint32 * Trivia array
        |   PyNone of uint32 * uint32 * Trivia array
        |   PyTrue of uint32 * uint32 * Trivia array
        |   PyAnd of uint32 * uint32 * Trivia array
        |   PyAs of uint32 * uint32 * Trivia array
        |   PyAssert of uint32 * uint32 * Trivia array
        |   PyAsync of uint32 * uint32 * Trivia array
        |   PyAwait of uint32 * uint32 * Trivia array
        |   PyBreak of uint32 * uint32 * Trivia array
        |   PyClass of uint32 * uint32 * Trivia array
        |   PyContinue of uint32 * uint32 * Trivia array
        |   PyDef of uint32 * uint32 * Trivia array
        |   PyDel of uint32 * uint32 * Trivia array
        |   PyElif of uint32 * uint32 * Trivia array
        |   PyElse of uint32 * uint32 * Trivia array
        |   PyExcept of uint32 * uint32 * Trivia array
        |   PyFinally of uint32 * uint32 * Trivia array
        |   PyFor of uint32 * uint32 * Trivia array
        |   PyFrom of uint32 * uint32 * Trivia array
        |   PyGlobal of uint32 * uint32 * Trivia array
        |   PyIf of uint32 * uint32 * Trivia array
        |   PyImport of uint32 * uint32 * Trivia array
        |   PyIn of uint32 * uint32 * Trivia array
        |   PyIs of uint32 * uint32 * Trivia array
        |   PyLambda of uint32 * uint32 * Trivia array
        |   PyNonlocal of uint32 * uint32 * Trivia array
        |   PyNot of uint32 * uint32 * Trivia array
        |   PyOr of uint32 * uint32 * Trivia array
        |   PyPass of uint32 * uint32 * Trivia array
        |   PyRaise of uint32 * uint32 * Trivia array
        |   PyReturn of uint32 * uint32 * Trivia array
        |   PyTry of uint32 * uint32 * Trivia array
        |   PyWhile of uint32 * uint32 * Trivia array
        |   PyWith of uint32 * uint32 * Trivia array
        |   PyYield of uint32 * uint32 * Trivia array
        |   PyPlus of uint32 * uint32 * Trivia array
        |   PyPlusAssign of uint32 * uint32 * Trivia array
        |   PyMinus of uint32 * uint32 * Trivia array
        |   PyMinusAssign of uint32 * uint32 * Trivia array
        |   PyMul of uint32 * uint32 * Trivia array
        |   PyMulAssign of uint32 * uint32 * Trivia array
        |   PyPower of uint32 * uint32 * Trivia array
        |   PyPowerAssign of uint32 * uint32 * Trivia array
        |   PyDiv of uint32 * uint32 * Trivia array
        |   PyDivAssign of uint32 * uint32 * Trivia array
        |   PyFloorDiv of uint32 * uint32 * Trivia array
        |   PyFlooeDivAssign of uint32 * uint32 * Trivia array
        |   PyModulo of uint32 * uint32 * Trivia array
        |   PyModuloAssign of uint32 * uint32 * Trivia array
        |   PyMatrice of uint32 * uint32 * Trivia array
        |   PyMatriceAssign of uint32 * uint32 * Trivia array
        |   PyBitAnd of uint32 * uint32 * Trivia array
        |   PyBitAndAssign of uint32 * uint32 * Trivia array
        |   PyBitOr of uint32 * uint32 * Trivia array
        |   PyBitOrAssign of uint32 * uint32 * Trivia array
        |   PyBitXor of uint32 * uint32 * Trivia array
        |   PyBitXorAssign of uint32 * uint32 * Trivia array
        |   PyShiftLeft of uint32 * uint32 * Trivia array
        |   PyShiftLeftAssign of uint32 * uint32 * Trivia array
        |   PyShiftRight of uint32 * uint32 * Trivia array
        |   PyShiftRightAssign of uint32 * uint32 * Trivia array
        |   PyAssign of uint32 * uint32 * Trivia array
        |   PyEqual of uint32 * uint32 * Trivia array
        |   PyNotEqual of uint32 * uint32 * Trivia array
        |   PyLess of uint32 * uint32 * Trivia array
        |   PyLessEqual of uint32 * uint32 * Trivia array
        |   PyGreater of uint32 * uint32 * Trivia array
        |   PyGreaterEqual of uint32 * uint32 * Trivia array
        |   PyArrow of uint32 * uint32 * Trivia array
        |   PyColon of uint32 * uint32 * Trivia array
        |   PyColonAssign of uint32 * uint32 * Trivia array
        |   PySemiColon of uint32 * uint32 * Trivia array
        |   PyComma of uint32 * uint32 * Trivia array
        |   PyDot of uint32 * uint32 * Trivia array
        |   PyElipsis of uint32 * uint32 * Trivia array
        |   PyBitInvert of uint32 * uint32 * Trivia array
        |   PyLeftParen of uint32 * uint32 * Trivia array
        |   PyRightParen of uint32 * uint32 * Trivia array
        |   PyLeftBracket of uint32 * uint32 * Trivia array
        |   PyRightBracket of uint32 * uint32 * Trivia array
        |   PyLeftCurly of uint32 * uint32 * Trivia array
        |   PyRightCurly of uint32 * uint32 * Trivia array
        |   Name of uint32 * uint32 * Trivia array * string
        |   Number of uint32 * uint32 * Trivia array * string
        |   String of uint32 * uint32 * Trivia array * string array
        |   EOF of uint32
        |   Empty


    type TokenStream = Token list

    type Node =
        |   NamedExpr of uint32 * uint32 * Node * Token * Node
        |   Test of uint32 * uint32 * Node * Token * Node * Token * Node
        |   Lambda of uint32 * uint32 * Token * Node * Token * Node
        |   OrTest of uint32 * uint32 * Node * Token * Node
        |   AndTest of uint32 * uint32 * Node * Token * Node
        |   NotTest of uint32 * uint32 * Token * Node
        |   Less of uint32 * uint32 * Node * Token * Node
        |   LessEqual of uint32 * uint32 * Node * Token * Node
        |   Equal of uint32 * uint32 * Node * Token * Node
        |   GreaterEqual of uint32 * uint32 * Node * Token * Node
        |   Greater of uint32 * uint32 * Node * Token * Node
        |   NotEqual of uint32 * uint32 * Node * Token * Node
        |   NotIn of uint32 * uint32 * Node * Token * Token * Node
        |   In of uint32 * uint32 * Node * Token * Node
        |   Is of uint32 * uint32 * Node * Token * Node
        |   IsNot of uint32 * uint32 * Node * Token * Token * Node
        |   StarExpr of uint32 * uint32 * Token * Node
        |   OrExpr of uint32 * uint32 * Node * Token * Node
        |   XorExpr of uint32 * uint32 * Node * Token * Node
        |   AndExpr of uint32 * uint32 * Node * Token * Node
        |   ShiftLeft of uint32 * uint32 * Node * Token * Node
        |   ShiftRight of uint32 * uint32 * Node * Token * Node
        |   Plus of uint32 * uint32 * Node * Token * Node
        |   Minus of uint32 * uint32 * Node * Token * Node
        |   Mul of uint32 * uint32 * Node * Token * Node
        |   Matrice of uint32 * uint32 * Node * Token * Node
        |   Div of uint32 * uint32 * Node * Token * Node
        |   FloorDiv of uint32 * uint32 * Node * Token * Node
        |   Modulo of uint32 * uint32 * Node * Token * Node
        |   UnaryPlus of uint32 * uint32 * Token * Node
        |   UnaryMinus of uint32 * uint32 * Token * Node
        |   UnaryInvert of uint32 * uint32 * Token * Node
        |   Power of uint32 * uint32 * Node * Token * Node
        |   AtomExpr of uint32 * uint32 * Token * Node * Node array
        |   Name of uint32 * uint32 * Token
        |   Number of uint32 * uint32 * Token
        |   String of uint32 * uint32 * Token array
        |   None of uint32 * uint32 * Token
        |   True of uint32 * uint32 * Token
        |   False of uint32 * uint32 * Token
        |   Elipsis of uint32 * uint32 * Token
        |   TestListComp of uint32 * uint32 * Node array * Token array
        |   Call of uint32 * uint32 * Token * Node * Token
        |   Index of uint32 * uint32 * Token * Node * Token
        |   DotName of uint32 * uint32 * Token * Token
        |   SubscriptList of uint32 * uint32 * Node array * Token array
        |   Subscript of uint32 * uint32 * Node * Token * Node * Token * Node
        |   ExprList of uint32 * uint32 * Node array * Token array
        |   TestList of uint32 * uint32 * Node array * Token array
        |   Dictionary of uint32 * uint32 * Token * Node * Token
        |   DictionaryContainer of uint32 * uint32 * Node array * Token array
        |   DictionaryKW of uint32 * uint32 * Token * Node
        |   DictionaryEntry of uint32 * uint32 * Node * Token * Node
        |   Set of uint32 * uint32 * Token * Node * Token
        |   SetContainer of uint32 * uint32 * Node array * Token array
        |   List of uint32 * uint32 * Token * Node * Token
        |   Tuple of uint32 * uint32 * Token * Node * Token
        |   ClassDef of uint32 * uint32 * Token * Token * Token * Node * Token * Token * Node
        |   ArgList of uint32 * uint32 * Node array * Token array
        |   Argument of uint32 * uint32 * Node * Token * Node
        |   SyncCompFor of uint32 * uint32 * Token * Node * Token * Node * Node
        |   CompFor of uint32 * uint32 * Token * Node
        |   CompIf of uint32 * uint32 * Token * Node * Node
        |   Yield of uint32 * uint32 * Token * Node
        |   YieldFrom of uint32 * uint32 * Token * Token * Node
        |   FuncBodySuite of uint32 * uint32 * Token * Node * Token * Node array * Token
        |   FuncTypeInput of uint32 * uint32 * Node * Token array * Token
        |   FuncType of uint32 * uint32 * Token * Node * Token * Token * Node
        |   TypeList of uint32 * uint32 * Node array * Token array
        |   TypeStar of uint32 * uint32 * Token * Node
        |   TypeKW of uint32 * uint32 * Token * Node
        |   StmtList of uint32 * uint32 * Node array * Token array
        |   SimpleStmtList of uint32 * uint32 * Node array * Token array * Token
        |   PlusAssign of uint32 * uint32 * Node * Token * Node
        |   MinusAssign of uint32 * uint32 * Node * Token * Node
        |   MulAssign of uint32 * uint32 * Node * Token * Node
        |   DivAssign of uint32 * uint32 * Node * Token * Node
        |   PowerAssign of uint32 * uint32 * Node * Token * Node
        |   FloorDivAssign of uint32 * uint32 * Node * Token * Node
        |   AndAssign of uint32 * uint32 * Node * Token * Node
        |   OrAssign of uint32 * uint32 * Node * Token * Node
        |   XorAssign of uint32 * uint32 * Node * Token * Node
        |   ShiftLeftAssign of uint32 * uint32 * Node * Token * Node
        |   ShiftRightAssign of uint32 * uint32 * Node * Token * Node
        |   MatriceAssign of uint32 * uint32 * Node * Token * Node
        |   ModuloAssign of uint32 * uint32 * Node * Token * Node
        |   Assign of uint32 * uint32 * Node * Token * Node
        |   DelStmt of uint32 * uint32 * Token * Node
        |   PassStmt of uint32 * uint32 * Token
        |   BreakStmt of uint32 * uint32 * Token
        |   ContinueStmt of uint32 * uint32 * Token
        |   ReturnStmt of uint32 * uint32 * Token * Node
        |   RaiseStmt of uint32 * uint32 * Token * Node * Token * Node
        |   ImportNameStmt of uint32 * uint32 * Token * Node
        |   ImportFromStmt of uint32 * uint32 * Token * Token array * Node * Token * Token * Node * Token
        |   ImportAsName of uint32 * uint32 * Token * Token * Token
        |   DottedAsName of uint32 * uint32 * Node * Token * Token
        |   ImportAsNames of uint32 * uint32 * Node array * Token array
        |   DottedAsNames of uint32 * uint32 * Node array * Token array
        |   DottedName of uint32 * uint32 * Token array * Token array
        |   GlobalStmt of uint32 * uint32 * Token * Token array * Token array
        |   NonlocalStmt of uint32 * uint32 * Token * Token array * Token array
        |   AssertStmt of uint32 * uint32 * Token * Node * Token * Node
        |   AsyncStmt of uint32 * uint32 * Token * Node
        |   IfStmt of uint32 * uint32 * Token * Node * Token * Node * Node array * Node
        |   ElifStmt of uint32 * uint32 * Token * Node * Token * Node
        |   ElseStmt of uint32 * uint32 * Token * Token * Node
        |   WhileStmt of uint32 * uint32 * Token * Node * Token * Node * Node
        |   ForStmt of uint32 * uint32 * Token * Node * Token * Node * Token * Node * Node * Node
        |   TryStmt of uint32 * uint32 * Token * Token * Node * Node array * Node * Node
        |   ExceptStmt of uint32 * uint32 * Node * Token * Node
        |   ExceptClause of uint32 * uint32 * Token * Node * Token * Node
        |   Finally of uint32 * uint32 * Token * Token * Node
        |   WithStmt of uint32 * uint32 * Token * Node array * Token array * Token * Node * Node
        |   WithItem of uint32 * uint32 * Node * Token * Node
        |   Suite of uint32 * uint32 * Token * Token * Node * Token
        |   Decorator of uint32 * uint32 * Token * Node * Token * Node * Token * Token
        |   Decorators of uint32 * uint32 * Node array
        |   Decorated of uint32 * uint32 * Node * Node
        |   AsyncFuncDef of uint32 * uint32 * Token * Node
        |   FuncDef of uint32 * uint32 * Token * Token * Node * Token * Node * Token * Node * Node
        |   Parameters of uint32 * uint32 * Token * Node * Token
        |   TypedArgsList of uint32 * uint32 * Node array * Node array * Token * Node * Token * Node * Token
        |   TFPAssign of uint32 * uint32 * Node * Token * Node
        |   TFPDef of uint32 * uint32 * Token * Token * Node
        |   VarArgsList of uint32 * uint32 * Node array * Token * Node * Token * Node * Token
        |   VarArgAssign of uint32 * uint32 * Node * Token * Node
        |   VFPDef of uint32 * uint32 * Token
        |   SingleInput of uint32 * uint32 * Node * Token
        |   FileInput of uint32 * uint32 * Token array * Node array
        |   EvalInput of uint32 * uint32 * Node * Token
        |   Empty

    exception SyntaxError of Token * string

    // Support functions //////////////////////////////////////////////////////////////////////////

    let tryToken ( stream : TokenStream ) =
        match stream with
        |   tok :: rest -> Some(tok, rest)
        |   _ -> Option.None

    let getPosition (stream : TokenStream) : uint32 =
        match stream with
        |   tok :: rest ->
                match tok with
                |   Token.PyColonAssign(a, _ , _ ) ->   a

                |   Token.Name( a, _ , _ , _ ) -> a
                |   Token.Number( a, _ , _ , _ ) -> a
                |   Token.EOF( a ) -> a
                |   _ ->
                        0ul
        |   _ ->
                0ul


    // Expression rules in Python 3.9 grammar /////////////////////////////////////////////////////

    let rec parseNamedExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let left, rest = parseTest stream
        match tryToken rest with
        |   Some(Token.PyColonAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest
                let right, rest3 = parseTest rest2
                (Node.NamedExpr(spanStart, getPosition(rest3), left, op, right), rest3)
        |   _ ->
                (left, rest )

    and parseTest (stream : TokenStream) =
        let spanStart = getPosition stream
        let left, rest = parseOrTest stream
        match tryToken rest with
        |   Some(Token.PyIf( _ , _ , _ ), rest2) ->
                let op1 = List.head rest
                let right, rest3 = parseOrTest rest2
                match tryToken rest3 with
                |   Some(Token.PyElse( _ , _ , _ ), rest4) ->
                        let op2 = List.head rest3
                        let next, rest5  = parseTest rest4
                        (Node.Test(spanStart, getPosition(rest5), left, op1, right, op2, next), rest5)
                |   _ ->
                        raise (SyntaxError(List.head rest3, "Expecting ëlse' in test expression!"))
        |   _ ->
                (left, rest )

    and parseTestNoCond (stream : TokenStream) =
        match tryToken stream with
        |   Some(Token.PyLambda( _ , _ , _ ), _ ) ->
                parseLambda (stream, false)
        |   _ ->
                parseOrTest stream

    and parseLambda (stream : TokenStream, isCond : bool) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyLambda( _ , _ , _ ), rest) ->
                let op = List.head stream
                let left, rest2 =   match tryToken rest with
                                    |   Some(Token.PyColon( _ , _ , _ ), _ ) ->
                                            Node.Empty, rest
                                    |   _ ->
                                            parseVarArgsList rest
                match tryToken rest2 with
                |   Some(Token.PyColon( _ , _ , _ ), rest3) ->
                        let op2 = List.head rest2
                        let right, rest3 =  match isCond with
                                            |   true ->
                                                    parseTest rest2
                                            |   _ ->
                                                    parseTestNoCond rest2
                        (Node.Lambda(spanStart, getPosition(rest3), op, left, op2, right), rest3)                      
                |   _ ->
                        raise (SyntaxError(List.head stream, "Expecting ':' in lambda expression!"))
        |   _ ->
                raise (SyntaxError(List.head stream, "Expecting 'lambda' in lambda expression!"))

    and parseOrTest (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseAndTest stream
        while   match tryToken rest with
                |   Some(Token.PyOr( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseAndTest rest2
                        left <- Node.OrTest(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseAndTest (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseNotTest stream
        while   match tryToken rest with
                |   Some(Token.PyAnd( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseNotTest rest2
                        left <- Node.OrTest(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseNotTest (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyNot( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = parseNotTest rest
                (Node.NotTest(spanStart, getPosition(rest2), op, right), rest2)
        |   _ ->
                parseComparison stream

    and parseComparison (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseExpr stream
        while   match tryToken rest with
                |   Some(Token.PyLess( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseExpr rest2
                        left <- Node.Less(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyLessEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseExpr rest2
                        left <- Node.LessEqual(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseExpr rest2
                        left <- Node.Equal(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyGreaterEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseExpr rest2
                        left <- Node.GreaterEqual(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyGreater( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseExpr rest2
                        left <- Node.Greater(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyNotEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseExpr rest2
                        left <- Node.NotEqual(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyIs( _ , _ , _), rest2) ->
                        let op = List.head rest
                        match tryToken rest with
                        |   Some(Token.PyNot( _ , _ , _ ), rest2) ->
                                let op2 = List.head rest
                                let right, rest3 = parseExpr rest2
                                left <- Node.IsNot(spanStart, getPosition(rest3), left, op, op2, right )
                                rest <- rest3
                        |   _ ->
                                let right, rest3 = parseExpr rest2
                                left <- Node.Is(spanStart, getPosition(rest3), left, op, right )
                                rest <- rest3
                        true
                |   Some(Token.PyNot( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        match tryToken rest with
                        |   Some(Token.PyIn( _ , _ , _ ), rest2) ->
                                let op2 = List.head rest
                                let right, rest3 = parseExpr rest2
                                left <- Node.NotIn(spanStart, getPosition(rest3), left, op, op2, right )
                                rest <- rest3
                        |   _ ->
                                raise (SyntaxError(List.head rest2, "Missing 'in' in 'not' 'in' operator!"))
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseStarExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyMul( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = parseExpr rest
                (Node.StarExpr(spanStart, getPosition(rest2), op, right), rest2)
        |   _ ->
                raise (SyntaxError(List.head stream, "Missing '*' in star expression!"))

    and parseExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseXorExpr stream
        while   match tryToken rest with
                |   Some(Token.PyBitOr( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseXorExpr rest2
                        left <- Node.OrTest(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseXorExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseAndExpr stream
        while   match tryToken rest with
                |   Some(Token.PyBitXor( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseAndExpr rest2
                        left <- Node.OrTest(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseAndExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseShiftExpr stream
        while   match tryToken rest with
                |   Some(Token.PyBitAnd( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseShiftExpr rest2
                        left <- Node.AndTest(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseShiftExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseArithExpr stream
        while   match tryToken rest with
                |   Some(Token.PyShiftLeft( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseArithExpr rest2
                        left <- Node.ShiftLeft(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyShiftRight( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseArithExpr rest2
                        left <- Node.ShiftRight(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseArithExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseTerm stream
        while   match tryToken rest with
                |   Some(Token.PyPlus( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseTerm rest2
                        left <- Node.Plus(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyMinus( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseTerm rest2
                        left <- Node.Minus(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseTerm (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable left, rest = parseFactor stream
        while   match tryToken rest with
                |   Some(Token.PyMul( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseFactor rest2
                        left <- Node.Mul(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyDiv( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseFactor rest2
                        left <- Node.Div(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyFloorDiv( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseFactor rest2
                        left <- Node.FloorDiv(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyMatrice( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseFactor rest2
                        left <- Node.Matrice(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   Some(Token.PyModulo( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = parseFactor rest2
                        left <- Node.Modulo(spanStart, getPosition(rest3), left, op, right )
                        rest <- rest3
                        true
                |   _ ->    false
            do ()
        (left, rest)

    and parseFactor (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyPlus( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let right, rest2 = parseFactor rest
                (Node.UnaryPlus(spanStart, getPosition(rest2), op, right), rest2)
        |   Some(Token.PyMinus( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let right, rest2 = parseFactor rest
                (Node.UnaryMinus(spanStart, getPosition(rest2), op, right), rest2)
        |   Some(Token.PyBitInvert( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let right, rest2 = parseFactor rest
                (Node.UnaryInvert(spanStart, getPosition(rest2), op, right), rest2)
        |   _ ->
                parsePower stream

    and parsePower (stream : TokenStream) =
        let spanStart = getPosition stream
        let left, rest = parseAtomExpr stream
        match tryToken rest with
        |   Some(Token.PyPower( _ , _ , _ ), rest2 ) ->
                let op = List.head rest
                let right, rest3 = parseFactor rest2
                (Node.Power(spanStart, getPosition(rest3), left, op, right), rest3)
        |   _ ->
                (left, rest)

    and parseAtomExpr (stream : TokenStream) =
        let spanStart = getPosition stream
        let op, rest =  match tryToken stream with
                        |   Some(Token.PyAwait( _ , _ , _ ), rest) ->
                                let op = List.head stream
                                op, rest
                        |   _ ->
                                Token.Empty, stream
        let left, rest2 = parseAtom rest
        match tryToken rest2 with
        |   Some(Token.PyLeftParen( _ , _ , _ ), _ )
        |   Some(Token.PyLeftBracket( _ , _ , _ ), _ )
        |   Some(Token.PyDot( _ , _ , _ ), _ )
        |   Some( _ , _ ) when ( match op with | Token.PyAwait( _ , _ , _ ) -> true | _ -> false ) ->
                let mutable nodes : Node list = []
                let mutable restRep = rest2
                while   match tryToken restRep with
                        |   Some(Token.PyLeftParen( _ , _ , _ ), _ )
                        |   Some(Token.PyLeftBracket( _ , _ , _ ), _ )
                        |   Some(Token.PyDot( _ , _ , _ ), _ ) ->
                                let node, restMore = parseTrailer restRep
                                nodes <- node :: nodes
                                restRep <- restMore
                                true
                        |   _ ->    false
                    do ()
                (Node.AtomExpr(spanStart, getPosition(restRep), op, left, List.toArray(List.rev nodes)), restRep)
        |   _ ->
                left, rest2

    and parseAtom (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.Name( _ , _ , _ , _ ), rest) ->
                let op = List.head stream
                (Node.Name(spanStart, getPosition(rest), op), rest)
        |   Some(Token.Number( _ , _ , _ , _ ), rest) ->
                let op = List.head stream
                (Node.Number(spanStart, getPosition(rest), op), rest)
        |   Some(Token.String( _ , _ , _ , _ ), _ ) ->
                let mutable restAgain = stream
                let mutable nodes : Token list = []
                while   match tryToken restAgain with
                        |   Some(Token.String( _ , _ , _ , _ ), restNow) ->
                                nodes <- List.head restAgain :: nodes
                                restAgain <- restNow
                                true
                        |   _ -> false
                    do ()
                (Node.String(spanStart, getPosition(restAgain), List.toArray(List.rev nodes)), restAgain)
        |   Some(Token.PyNone( _ , _ , _ ), rest) ->
                let op = List.head stream
                (Node.None(spanStart, getPosition(rest), op), rest)
        |   Some(Token.PyTrue( _ , _ , _ ), rest) ->
                let op = List.head stream
                (Node.True(spanStart, getPosition(rest), op), rest)
        |   Some(Token.PyFalse( _ , _ , _ ), rest) ->
                let op = List.head stream
                (Node.False(spanStart, getPosition(rest), op), rest)
        |   Some(Token.PyElipsis( _ , _ , _ ), rest) ->
                let op = List.head stream
                (Node.Elipsis(spanStart, getPosition(rest), op), rest)
        |   _ ->
                raise (SyntaxError(List.head stream, "Expecting literal name, number, string etc!"))

    and parseTestListComp (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTrailer (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSubscriptList (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSubscript (stream : TokenStream) =
        (Node.Empty, stream )

    and parseExprList (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTestList (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDictorSetMaker (stream : TokenStream) =
        (Node.Empty, stream )

    and parseArgList (stream : TokenStream) =
        (Node.Empty, stream )

    and parseArgument (stream : TokenStream) =
        (Node.Empty, stream )

    and parseCompIter (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSyncCompFor (stream : TokenStream) =
        (Node.Empty, stream )

    and parseCompFor (stream : TokenStream) =
        (Node.Empty, stream )

    and parseCompIf (stream : TokenStream) =
        (Node.Empty, stream )

    and parseYield (stream : TokenStream) =
        (Node.Empty, stream )

    // Statement rules in Python 3.9 //////////////////////////////////////////////////////////////

    and parseStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSimpleStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSmallStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseExprStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseAnnAssign (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTestListStarExpr (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDelStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseBreakStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseContinueStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseReturnStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseRaiseStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseImportStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseImportNameStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseImportFromStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseImportAsNameStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDottedAsNameStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseImportAsNamesStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDottedAsNamesStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDottedNameStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseGlobalStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseNonlocalStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseAssertStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseCompoundStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseAsyncStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseIfStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseElseStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseWhileStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseForStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTryStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseExceptClauseStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseFinallyStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseWithtmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseWithItem (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSuite (stream : TokenStream) =
        (Node.Empty, stream )

    and parseSingleInput (stream : TokenStream) =
        (Node.Empty, stream )

    and parseFileInput (stream : TokenStream) =
        (Node.Empty, stream )

    and parseEvalInput (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDecorator (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDecorators (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDecorated (stream : TokenStream) =
        (Node.Empty, stream )

    and parseAsyncFuncDef (stream : TokenStream) =
        (Node.Empty, stream )

    and parseFuncDef (stream : TokenStream) =
        (Node.Empty, stream )

    and parseParameters (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTypedArgsList (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTFPDef (stream : TokenStream) =
        (Node.Empty, stream )

    and parseVarArgsList (stream : TokenStream) =
        (Node.Empty, stream )

    and parseClassDef (stream : TokenStream) =
        (Node.Empty, stream )

    and parseFuncBodySuite (stream : TokenStream) =
        (Node.Empty, stream )

    and parseFuncTypeInput (stream : TokenStream) =
        (Node.Empty, stream )

    and parseFuncType (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTypeList (stream : TokenStream) =
        (Node.Empty, stream )




    // Temporary code below ///////////////////////////////////////////////////////////////////////

    [<EntryPoint>]
    let main argv =
        printfn "Hello World from F#!"

        let a, b = [ Token.Name(0ul, 2ul, [||], "a"); Token.PyColonAssign(3ul, 5ul, [||]); Token.Name(6ul, 8ul, [| |], "b"); Token.EOF(8ul) ] |> parseNamedExpr
        printfn "%O" a
        System.Console.ReadKey() |> ignore
        0 // return an integer exit code
