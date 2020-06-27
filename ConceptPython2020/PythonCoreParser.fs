﻿
namespace PythonCoreFramework.Runtime

module PythonCoreParser =

    type Trivia =
        |   WhiteSpace of uint32 * uint32 * string
        |   Newline of uint32 * uint32 * char * char
        |   Comment of uint32 * uint32 * string
        |   LineContinuation of uint32 * uint32 * char * char * char
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
        |   PyFloorDivAssign of uint32 * uint32 * Trivia array
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
        |   StarArgument of uint32 * uint32 * Token * Node
        |   PowerArgument of uint32 * uint32 * Token * Node
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
                |   Token.PyPlus(a, _, _ ) -> a 
                |   Token.PyMinus(a, _, _ ) -> a 
                |   Token.PyMul(a, _, _ ) -> a 
                |   Token.PyPower(a, _, _ ) -> a 
                |   Token.PyDiv(a, _, _ ) -> a 
                |   Token.PyFloorDiv(a, _, _ ) -> a 
                |   Token.PyModulo(a, _, _ ) -> a 
                |   Token.PyMatrice(a, _, _ ) -> a 
                |   Token.PyShiftLeft(a, _, _ ) -> a 
                |   Token.PyShiftRight(a, _, _ ) -> a 
                |   Token.PyBitAnd(a, _, _ ) -> a 
                |   Token.PyBitOr(a, _, _ ) -> a 
                |   Token.PyBitXor(a, _, _ ) -> a 
                |   Token.PyBitInvert(a, _, _ ) -> a 
                |   Token.PyLess(a, _, _ ) -> a 
                |   Token.PyGreater(a, _, _ ) -> a 
                |   Token.PyLessEqual(a, _, _ ) -> a 
                |   Token.PyGreaterEqual(a, _, _ ) -> a 
                |   Token.PyEqual(a, _, _ ) -> a 
                |   Token.PyNotEqual(a, _, _ ) -> a 
                |   Token.PyLeftParen(a, _, _ ) -> a 
                |   Token.PyLeftBracket(a, _, _ ) -> a 
                |   Token.PyLeftCurly(a, _, _ ) -> a 
                |   Token.PyRightParen(a, _, _ ) -> a 
                |   Token.PyRightBracket(a, _, _ ) -> a 
                |   Token.PyRightCurly(a, _, _ ) -> a 
                |   Token.PyComma(a, _, _ ) -> a 
                |   Token.PyColon(a, _, _ ) -> a 
                |   Token.PyDot(a, _, _ ) -> a 
                |   Token.PySemiColon(a, _, _ ) -> a 
                |   Token.PyAssign(a, _, _ ) -> a 
                |   Token.PyArrow(a, _, _ ) -> a 
                |   Token.PyPlusAssign(a, _, _ ) -> a 
                |   Token.PyMinusAssign(a, _, _ ) -> a 
                |   Token.PyMulAssign(a, _, _ ) -> a 
                |   Token.PyDivAssign(a, _, _ ) -> a 
                |   Token.PyFloorDivAssign(a, _, _ ) -> a 
                |   Token.PyModuloAssign(a, _, _ ) -> a 
                |   Token.PyMatriceAssign(a, _, _ ) -> a 
                |   Token.PyBitAndAssign(a, _, _ ) -> a 
                |   Token.PyBitOrAssign(a, _, _ ) -> a 
                |   Token.PyBitXorAssign(a, _, _ ) -> a 
                |   Token.PyShiftLeftAssign(a, _, _ ) -> a 
                |   Token.PyShiftRightAssign(a, _, _ ) -> a 
                |   Token.PyPowerAssign(a, _, _ ) -> a 
                |   Token.PyElipsis(a, _, _ ) -> a 
                |   Token.PyFalse(a, _, _ ) -> a 
                |   Token.PyNone(a, _, _ ) -> a 
                |   Token.PyTrue(a, _, _ ) -> a 
                |   Token.PyAs(a, _, _ ) -> a 
                |   Token.PyAnd(a, _, _ ) -> a 
                |   Token.PyAssert(a, _, _ ) -> a 
                |   Token.PyAwait(a, _, _ ) -> a 
                |   Token.PyAsync(a, _, _ ) -> a 
                |   Token.PyBreak(a, _, _ ) -> a 
                |   Token.PyClass(a, _, _ ) -> a 
                |   Token.PyContinue(a, _, _ ) -> a 
                |   Token.PyDef(a, _, _ ) -> a 
                |   Token.PyDel(a, _, _ ) -> a 
                |   Token.PyElif(a, _, _ ) -> a 
                |   Token.PyElse(a, _, _ ) -> a 
                |   Token.PyExcept(a, _, _ ) -> a 
                |   Token.PyFinally(a, _, _ ) -> a 
                |   Token.PyFor(a, _, _ ) -> a 
                |   Token.PyFrom(a, _, _ ) -> a 
                |   Token.PyGlobal(a, _, _ ) -> a 
                |   Token.PyIf(a, _, _ ) -> a 
                |   Token.PyImport(a, _, _ ) -> a 
                |   Token.PyIn(a, _, _ ) -> a 
                |   Token.PyIs(a, _, _ ) -> a 
                |   Token.PyLambda(a, _, _ ) -> a 
                |   Token.PyNonlocal(a, _, _ ) -> a 
                |   Token.PyNot(a, _, _ ) -> a 
                |   Token.PyOr(a, _, _ ) -> a 
                |   Token.PyPass(a, _, _ ) -> a 
                |   Token.PyRaise(a, _, _ ) -> a 
                |   Token.PyReturn(a, _, _ ) -> a 
                |   Token.PyTry(a, _, _ ) -> a 
                |   Token.PyWhile(a, _, _ ) -> a 
                |   Token.PyWith(a, _, _ ) -> a 
                |   Token.PyYield(a, _, _ ) -> a 
                |   Token.Name(a, _ , _ , _ ) -> a
                |   Token.Number(a, _ , _ , _ ) -> a
                |   Token.String(a , _ , _ , _ ) -> a
                |   Token.EOF(a) -> a
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
        |   Some(Token.PyLeftParen( _ , _ , _ ), rest) ->
                let op1 = List.head stream
                match tryToken rest with
                |   Some(Token.PyRightParen( _ , _ , _ ), rest2 ) ->
                        let op2 = List.head rest
                        (Node.Tuple(spanStart, getPosition(rest2), op1, Node.Empty, op2), rest2)
                |   Some(Token.PyYield( _ , _ , _ ), rest2 ) ->
                        let node, rest3 = parseYield rest
                        match tryToken rest3 with
                        |   Some(Token.PyRightParen( _ , _ , _ ), rest4 ) ->
                                let op2 = List.head rest3
                                (Node.Tuple(spanStart, getPosition(rest2), op1, node, op2), rest4 )
                        |   Some ( _ , _ ) ->
                                raise (SyntaxError(List.head rest3, "Missing ')' in atomic value!"))
                        |   _ ->
                                raise (SyntaxError(List.head rest3, "Empty token stream!"))
                |   Some( _ , _ ) ->
                        let node, rest5 = parseTestListComp rest
                        match tryToken rest5 with
                        |   Some(Token.PyRightParen( _ , _ , _ ), rest4 ) ->
                            let op2 = List.head rest5
                            (Node.Tuple(spanStart, getPosition(rest), op1, node, op2), rest4 )
                        |   Some ( _ , _ ) ->
                            raise (SyntaxError(List.head rest, "Missing ')' in atomic value!"))
                        |   _ ->
                            raise (SyntaxError(List.head rest, "Empty token stream!"))
                |   _ ->
                        raise (SyntaxError(List.head rest, "Empty token stream!"))
        |   Some(Token.PyLeftBracket( _ , _ , _ ), rest) ->
                let op1 = List.head stream
                match tryToken rest with
                |   Some(Token.PyRightBracket( _ , _ , _ ), rest2 ) ->
                        let op2 = List.head rest
                        (Node.Tuple(spanStart, getPosition(rest2), op1, Node.Empty, op2), rest2)
                |   Some( _ , _ ) ->
                        let node, rest5 = parseTestListComp rest
                        match tryToken rest5 with
                        |   Some(Token.PyRightBracket( _ , _ , _ ), rest4 ) ->
                            let op2 = List.head rest5
                            (Node.Tuple(spanStart, getPosition(rest), op1, node, op2), rest4 )
                        |   Some ( _ , _ ) ->
                            raise (SyntaxError(List.head rest, "Missing ']' in atomic value!"))
                        |   _ ->
                            raise (SyntaxError(List.head rest, "Empty token stream!"))
                |   _ ->
                        raise (SyntaxError(List.head rest, "Empty token stream!"))
        |   Some(Token.PyLeftCurly( _ , _ , _ ), rest) ->
                let op1 = List.head stream
                match tryToken rest with
                |   Some(Token.PyRightCurly( _ , _ , _ ), rest2) ->
                        let op2 = List.head rest
                        (Node.Dictionary(spanStart, getPosition(rest2), op1, Node.Empty, op2), rest2)
                |   Some( _ , _ ) ->
                        let node, rest2 = parseDictorSetMaker rest
                        match tryToken rest2 with
                        |   Some(Token.PyRightCurly( _ , _ , _ ), rest3 ) ->
                                let op2 = List.head rest2
                                match node with
                                |   Node.DictionaryContainer( _ , _ , _ , _ ) ->
                                        (Node.Dictionary(spanStart, getPosition(rest3), op1, node, op2), rest3)
                                |   _ ->
                                        (Node.Set(spanStart, getPosition(rest3), op1, node, op2), rest3)
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head rest, "Missing '}' in dictionary or set!"))
                        |   _ ->
                                raise (SyntaxError(List.head rest, "Empty token stream!"))
                |   _ ->
                        raise (SyntaxError(List.head rest, "Missing '}' in dictionary or set!"))
        |   Some( _ , _ ) ->
                raise (SyntaxError(List.head stream, "Expecting literal name, number, string etc!"))
        |   _ ->
                raise (SyntaxError(List.head stream, "Empty token stream!"))

    and parseTestListComp (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable nodes : Node List = []
        let mutable separators : Token List = []
        let mutable rest = stream
        match tryToken rest with
        |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                let node, rest2 = parseStarExpr rest
                nodes <- node :: nodes
                rest <- rest2
        |   Some( _ , _ ) ->
                let node, rest2 = parseNamedExpr rest
                nodes <- node :: nodes
                rest <- rest2
        |   _ ->
                raise ( SyntaxError(List.head rest, "Expression list needs ar least one expression!") )
        match tryToken rest with
        |   Some(Token.PyFor( _ , _ , _ ), _ ) ->
                let node2, rest2 = parseCompFor rest
                nodes <- node2 :: nodes
                rest <- rest2
        |   _ ->
               while   match tryToken rest with
                       |   Some(Token.PyComma( _ , _ , _ ), rest2) ->
                               separators <- List.head rest :: separators
                               rest <- rest2
                               match tryToken rest with
                               |   Some(Token.PyIn( _ , _ , _ ), _ ) ->
                                       false
                               |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                                       let node2, rest3 = parseStarExpr rest
                                       nodes <- node2 :: nodes
                                       rest <- rest3
                                       true
                               |   _ ->
                                       let node2, rest3 = parseNamedExpr rest
                                       nodes <- node2 :: nodes
                                       rest <- rest3
                                       true
                       |   _ -> false
                   do ()
        (Node.TestListComp(spanStart, getPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest )

    and parseTrailer (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyLeftParen( _ , _ , _ ), rest ) ->
                let op1 = List.head stream
                let node, rest2 =   match tryToken rest with
                                    |   Some(Token.PyRightParen( _ , _ , _ ), _ ) ->
                                            Node.Empty, rest
                                    |   _ ->
                                            parseArgList rest
                match tryToken rest2 with
                |   Some(Token.PyRightParen( _ , _ , _ ), rest3 ) ->
                        let op2 = List.head rest2
                        (Node.Call(spanStart, getPosition(rest3), op1, node, op2), rest3)
                |   _ ->
                    raise (SyntaxError(List.head rest2, "Missing ')' in call expression!"))
        |   Some(Token.PyLeftBracket( _ , _ , _ ), rest ) ->
                let op1 = List.head stream
                let node, rest2 =   match tryToken rest with
                                    |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                                            Node.Empty, rest
                                    |   _ ->
                                            parseSubscriptList rest
                match tryToken rest2 with
                |   Some(Token.PyRightParen( _ , _ , _ ), rest3 ) ->
                        let op2 = List.head rest2
                        (Node.Index(spanStart, getPosition(rest3), op1, node, op2), rest3)
                |   _ ->
                    raise (SyntaxError(List.head rest2, "Missing ']' in index expression!"))
        |   Some(Token.PyDot( _ , _ , _ ), rest ) ->
                let op1 = List.head stream
                match tryToken rest with
                |   Some(Token.Name( _ , _ , _ , _ ), rest2 ) ->
                        let op2 = List.head rest
                        (Node.DotName(spanStart, getPosition(rest2), op1, op2), rest2)
                |   _ ->
                        raise (SyntaxError(List.head rest, "Expecting name literal after '.'"))
        |   _ ->  
            raise (SyntaxError(List.head stream, "Expecting '(', '[', '.' in trailer expression!"))

    and parseSubscriptList (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable nodes : Node list = []
        let mutable separators : Token List = []
        let mutable rest = stream
        let node, rest2 = parseSubscript rest
        nodes <- node :: nodes
        rest <- rest2
        while   match tryToken rest with
                |   Some(Token.PyComma( _ , _ , _ ), rest3 ) ->
                        separators <- List.head rest :: separators
                        rest <- rest3
                        match tryToken rest with
                        |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                                false
                        |   _ ->
                                let node2, rest4 = parseSubscript rest
                                nodes <- node2 :: nodes
                                rest <- rest4
                                true
                |   _ ->    false
            do ()
        (Node.SubscriptList(spanStart, getPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest )

    and parseSubscript (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable rest = stream
        let left =  match tryToken rest with
                    |   Some(Token.PyColon( _ , _ , _ ), _ ) ->
                            Node.Empty
                    |   Some(Token.PyComma( _ , _ , _ ), _ )
                    |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                            raise(SyntaxError(List.head rest, "Missing expression before ':' in subscript!"))
                    |   _ ->
                            let a, b = parseTest rest
                            rest <- b
                            a
        match tryToken rest with
        |   Some(Token.PyColon( _ , _ , _ ), rest2 ) ->
                let op1 = List.head rest
                rest <- rest2
                let right = match tryToken rest with
                            |   Some(Token.PyColon( _ , _ , _ ), _ )
                            |   Some(Token.PyComma( _ , _ , _ ), _ )
                            |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                                    Node.Empty
                            |   _ ->
                                    let a, b = parseTest rest
                                    rest <- b
                                    a
                match tryToken rest with
                |   Some(Token.PyColon( _ , _ , _ ), rest2 ) ->
                        let op2 = List.head rest
                        match tryToken rest2 with
                        |   Some(Token.PyComma( _ , _ , _ ), _ )
                        |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                                (Node.Subscript(spanStart, getPosition(rest), left, op1, right, op2, Node.Empty), rest )
                        |   _ ->
                                let a, b = parseTest rest2
                                rest <- b
                                (Node.Subscript(spanStart, getPosition(rest), left, op1, right, op2, a), rest )
                |   _ ->
                        (Node.Subscript(spanStart, getPosition(rest), left, op1, right, Token.Empty, Node.Empty), rest )
        |   _ ->
                (Node.Subscript(spanStart, getPosition(rest), left, Token.Empty, Node.Empty, Token.Empty, Node.Empty), rest )

    and parseExprList (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable nodes : Node List = []
        let mutable separators : Token List = []
        let mutable rest = stream
        match tryToken rest with
        |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                let node, rest2 = parseStarExpr rest
                nodes <- node :: nodes
                rest <- rest2
        |   Some( _ , _ ) ->
                let node, rest2 = parseExpr rest
                nodes <- node :: nodes
                rest <- rest2
        |   _ ->
                raise ( SyntaxError(List.head rest, "Expression list needs ar least one expression!") )
        while   match tryToken rest with
                |   Some(Token.PyComma( _ , _ , _ ), rest2) ->
                        separators <- List.head rest :: separators
                        rest <- rest2
                        match tryToken rest with
                        |   Some(Token.PyIn( _ , _ , _ ), _ ) ->
                                false
                        |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                                let node2, rest3 = parseStarExpr rest
                                nodes <- node2 :: nodes
                                rest <- rest3
                                true
                        |   _ ->
                                let node2, rest3 = parseExpr rest
                                nodes <- node2 :: nodes
                                rest <- rest3
                                true
                |   _ -> false
            do ()
        (Node.ExprList(spanStart, getPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest )

    and parseTestList (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable nodes : Node List = []
        let mutable separators : Token List = []
        let mutable node, rest = parseTest stream
        nodes <- node :: nodes
        while   match tryToken rest with
                |   Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                        separators <- List.head rest :: separators
                        rest <- rest2
                        match tryToken rest with
                        |   Some(Token.Newline( _ , _ , _ ), _ ) ->
                                false
                        |   Some(Token.PySemiColon( _ , _ , _ ), _ ) ->
                                false
                        |   _ ->
                                let node2, rest3 = parseTest rest
                                nodes <- node2 :: nodes
                                rest <- rest3
                                true
                |   _ -> false
            do ()
        (Node.TestList(spanStart, getPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest )

    and parseDictorSetMaker (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable nodes : Node list = []
        let mutable Separators : Token list = []
        let mutable isSet = true
        let mutable rest = stream
        match tryToken rest with
        |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                let node, rest2 = parseStarExpr rest
                rest <- rest2
                nodes <- node :: nodes
        |   Some(Token.PyPower( _ , _ , _ ), rest2) ->
                let op = List.head rest
                let node, rest3 = parseExpr rest2
                rest <- rest3
                isSet <- false
                nodes <- Node.DictionaryKW(spanStart, getPosition(rest), op, node) :: nodes
        |   Some( _ , _ ) ->
                let left, rest2 = parseTest rest
                match tryToken rest2 with
                |   Some(Token.PyColon( _ , _ , _ ), rest3 ) ->
                        let op = List.head rest2
                        let right, rest4 = parseTest rest3
                        isSet <- false
                        rest <- rest4
                        nodes <- Node.DictionaryEntry(spanStart, getPosition(rest4), left, op, right) :: nodes
                |   Some( _ , _ ) ->
                        nodes <- left :: nodes
                        rest <- rest2
                |   _ ->
                        raise (SyntaxError(List.head rest, "Empty token stream!"))
        |   _ ->
                raise (SyntaxError(List.head rest, "Empty token stream!"))

        match tryToken rest with
        |   Some(Token.PyAsync( _ , _ , _ ), _ )
        |   Some(Token.PyFor( _ , _ , _ ), _ ) ->
                let right, rest2 = parseCompIter rest
                nodes <- right :: nodes
                rest <- rest2
        |   Some(Token.PyComma( _ , _ , _ ), rest2) ->
                while   match tryToken rest with
                        |   Some(Token.PyComma( _ , _ , _ ), rest2) ->
                                Separators <- List.head rest :: Separators
                                rest <- rest2
                                match tryToken rest with
                                |   Some(Token.PyRightCurly( _ , _ , _ ), rest2) ->
                                        false
                                |   Some( _ , _ ) ->
                                        match isSet with
                                        |   true ->
                                                match tryToken rest with
                                                |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                                                        let right, rest6 = parseStarExpr rest
                                                        nodes <- right :: nodes
                                                        rest <- rest6
                                                        true
                                                |   Some( _ , _ ) ->
                                                        let right, rest6 = parseTest rest
                                                        nodes <- right :: nodes
                                                        rest <- rest6
                                                        true
                                                |   _ ->
                                                        raise (SyntaxError(List.head rest, "Empty token stream!" ))
                                        |   _ ->
                                                match tryToken rest with
                                                |   Some(Token.PyPower( _ , _ , _ ), rest2 ) ->
                                                        let op = List.head rest
                                                        let right, rest3 = parseExpr rest2
                                                        nodes <- Node.DictionaryKW(spanStart, getPosition(rest3), op, right) :: nodes
                                                        rest <- rest3
                                                |   Some( _ , _ ) ->
                                                        let left, rest2 = parseTest rest
                                                        match tryToken rest2 with
                                                        |   Some(Token.PyColon( _ , _ , _ ), rest3 ) ->
                                                                let op = List.head rest2
                                                                let right, rest4 = parseTest rest3
                                                                nodes <- Node.DictionaryEntry(spanStart, getPosition(rest4), left, op, right) :: nodes
                                                                rest <- rest4
                                                        |   Some( _ , _ ) ->
                                                                raise (SyntaxError(List.head rest2, "Expecting ':' in dictionary entry!"))
                                                        |   _ ->
                                                                raise (SyntaxError(List.head rest2, "Empty stream token!"))
                                                |   _ ->
                                                        raise (SyntaxError(List.head rest, "Empty token stream!"))
                                                true
                                |   _ ->
                                        raise (SyntaxError(List.head rest, "Empty token stream!"))
                        |   _ ->    false
                    do ()
        |   _ ->
                raise (SyntaxError(List.head rest, "Empty token stream!"))

        match isSet with
        |   true    ->
                (Node.SetContainer(spanStart, getPosition(rest), List.toArray(List.rev nodes), List.toArray(List.rev Separators)), rest)
        |   _ ->
                (Node.DictionaryContainer(spanStart, getPosition(rest), List.toArray(List.rev nodes), List.toArray(List.rev Separators)), rest)

    and parseArgList (stream : TokenStream) =
        let spanStart = getPosition stream
        let mutable nodes : Node List = []
        let mutable separators : Token List = []
        let mutable node, rest = parseArgument stream
        nodes <- node :: nodes
        while   match tryToken rest with
                |   Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                        separators <- List.head rest :: separators
                        rest <- rest2
                        match tryToken rest with
                        |   Some(Token.PyRightParen( _ , _ , _ ), _ ) ->
                                false
                        |   _ ->
                                let node2, rest3 = parseArgument rest
                                nodes <- node2 :: nodes
                                rest <- rest3
                                true
                |   _ -> false
            do ()
        (Node.ArgList(spanStart, getPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest )

    and parseArgument (stream : TokenStream) =
        let spanStart = getPosition stream
        let left, rest =    match tryToken stream with
                            |   Some(Token.PyMul( _ , _ , _ ), rest2 ) ->
                                    let op = List.head stream
                                    let node, rest3 = parseTest rest2
                                    (Node.StarArgument(spanStart, getPosition(rest2), op, node), rest3)
                            |   Some(Token.PyPower( _ , _ , _ ), rest2) ->
                                    let op = List.head stream
                                    let node, rest3 = parseTest rest2
                                    (Node.PowerArgument(spanStart, getPosition(rest2), op, node), rest3)
                            |   Some(Token.Name( _ , _ , _ , _ ) , rest2 )   ->
                                    (Node.Name(spanStart, getPosition(rest2), List.head stream), rest2)
                            |   _ ->
                                    raise (SyntaxError(List.head stream, "Missing argument!"))
        match tryToken rest with
        |   Some(Token.PyFor( _ , _ , _ ), _ ) ->
                let a, b = parseCompFor rest
                (Node.Argument(spanStart, getPosition(rest), left, Token.Empty, a), b)
        |   Some(Token.PyColonAssign( _ , _ , _ ), rest2 ) ->
                let op = List.head rest
                let a, b = parseTest rest2
                (Node.Argument(spanStart, getPosition(rest), left, op, a), b)
        |   Some(Token.PyAssign( _ , _ , _ ), rest2 ) ->
                let op = List.head rest
                let a, b = parseTest rest2
                (Node.Argument(spanStart, getPosition(rest), left, op, a), b)
        |   Some( _ , _ ) ->
                (Node.Argument(spanStart, getPosition(rest), left, Token.Empty, Node.Empty), rest)
        |   _ ->
                raise (SyntaxError(List.head rest, "Unexpected end of Token stream!"))

    and parseCompIter (stream : TokenStream) =
        match tryToken stream with
        |   Some(Token.PyAwait( _ , _ , _ ), _ )
        |   Some(Token.PyFor( _ , _ , _ ), _ ) ->
                parseCompFor stream
        |   Some(Token.PyIf( _ , _ , _ ), _ ) ->
                parseCompIf stream
        |   _ ->
                raise (SyntaxError(List.head stream, "Expecting 'async', 'for' or 'if' in comprehensive expression!"))

    and parseSyncCompFor (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyFor( _ , _ , _ ), rest ) ->
                let op1 = List.head stream
                let left, rest2 = parseExprList rest
                match tryToken rest2 with
                |   Some(Token.PyIn( _ , _ , _ ), rest3 ) ->
                        let op2 = List.head rest2
                        let right, rest4 = parseOrTest rest3
                        match tryToken rest4 with
                        |   Some(Token.PyIf( _ , _ , _ ), _ )
                        |   Some(Token.PyAsync( _ , _ , _ ), _ )
                        |   Some(Token.PyFor( _ , _ , _ ), _ ) ->
                                let next, rest5 = parseCompIter rest4
                                (Node.SyncCompFor(spanStart, getPosition(rest4), op1, left, op2, right, next), rest5)
                        |   Some( _ , _ ) ->
                                (Node.SyncCompFor(spanStart, getPosition(rest4), op1, left, op2, right, Node.Empty), rest4)
                        |   _ ->
                                raise (SyntaxError(List.head rest4, "Empty token stram!"))
                |   _ ->
                        raise (SyntaxError(List.head rest2, "Missing 'in' in comprehension expression!"))
        |   _ ->
                raise (SyntaxError(List.head stream, "Missing 'for' in comprehension expression!"))

    and parseCompFor (stream : TokenStream) =
        let spanStart = getPosition stream
        let op, rest =  match tryToken stream with
                        |   Some(Token.PyAsync( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                Token.Empty, stream
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        let node, rest3 = parseSyncCompFor rest
        match op with
        |   Token.Empty ->
                (node, rest3)
        |   _ ->
                (Node.CompFor(spanStart, getPosition(rest3), op, node ), rest3)

    and parseCompIf (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyIf( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let node, rest2 = parseTestNoCond rest
                match tryToken rest2 with
                |   Some(Token.PyIf( _ , _ , _ ), rest3 )
                |   Some(Token.PyAsync( _ , _ , _ ), rest3 )
                |   Some(Token.PyFor( _ , _ , _ ), rest3 ) ->
                        let next, rest4 = parseCompIter rest3
                        (Node.CompIf(spanStart, getPosition(rest4), op, node, next), rest4)
                |   Some( _ , _ ) ->
                        (Node.CompIf(spanStart, getPosition(rest2), op, node, Node.Empty), rest2)
                |   _ ->
                        raise (SyntaxError(List.head rest2, "Empty token stream!"))
        |   _ ->
                raise (SyntaxError(List.head stream, "Missing 'if' in comprehension expression!"))

    and parseYield (stream : TokenStream) =
        let spanStart = getPosition stream
        match tryToken stream with
        |   Some(Token.PyYield( _ , _ , _ ), rest ) ->
                let op = List.head stream
                match tryToken rest with
                |   Some(Token.PyFrom( _ , _ , _ ), rest2 ) ->
                        let op2 = List.head rest
                        let node, rest3 = parseTest rest2
                        (Node.YieldFrom(spanStart, getPosition(rest3), op, op2, node), rest3)
                |   Some( _ , rest2 ) ->
                        let node, rest3 = parseTestListStarExpr rest2
                        (Node.Yield(spanStart, getPosition(rest3), op, node), rest3)
                |   _ ->
                        raise (SyntaxError(List.head rest, "Empty token stream!" ))
        |   _ ->
                raise(SyntaxError(List.head stream, "Missing 'yield' in yield expression!"))

    // Statement rules in Python 3.9 //////////////////////////////////////////////////////////////

    and parseStmt (stream : TokenStream) =
        match tryToken stream with
        |   Some(Token.PyIf( _ , _ , _ ), _ )
        |   Some(Token.PyFor( _ , _ , _ ), _ )
        |   Some(Token.PyWhile( _ , _ , _ ), _ )
        |   Some(Token.PyAsync( _ , _ , _ ), _ )
        |   Some(Token.PyWith( _ , _ , _ ), _ )
        |   Some(Token.PyTry( _ , _ , _ ), _ )
        |   Some(Token.PyDef( _ , _ , _ ), _ )
        |   Some(Token.PyClass( _ , _ , _ ), _ )
        |   Some(Token.PyMatrice( _ , _ , _ ), _ ) ->
                parseCompoundStmt stream
        |   _ ->
                parseSimpleStmt stream

    and parseSimpleStmt (stream : TokenStream) =
        let mutable nodes : Node list = []
        let mutable separators : Token list = []
        let spanStart = getPosition stream
        let mutable node, rest = parseSmallStmt stream
        nodes <- node :: nodes
        while   match tryToken rest with
                |   Some(Token.PySemiColon( _ , _ , _ ), rest2 ) ->
                        separators <- List.head rest :: separators
                        match tryToken rest2 with
                        |   Some(Token.Newline( _ , _ , _ ), _ ) -> false
                        |   Some( _ , _ ) ->
                                let node2, rest3 = parseSmallStmt rest2
                                nodes <- node2 :: nodes
                                rest <- rest3
                                true
                        |   _ ->    raise (SyntaxError(List.head rest2, "Empty token stream!"))
                | _ ->  false
            do ()
        match tryToken rest with
        |   Some(Token.Newline( _ , _ , _ ), rest4 ) ->
                (Node.SimpleStmtList(spanStart, getPosition(rest4), List.toArray(List.rev nodes), List.toArray(List.rev separators), List.head rest), rest4)
        |   Some ( _ , _ ) ->
                raise (SyntaxError(List.head rest, "Expecting NEWLINE after simple statement list!"))
        |   _ ->
                raise (SyntaxError(List.head rest, "Empty token stream!"))

    and parseSmallStmt (stream : TokenStream) =
        match tryToken stream with
        |   Some(Token.PyDel( _ , _ , _ ), _ ) ->       parseDelStmt stream
        |   Some(Token.PyPass( _ , _ , _ ), _ ) ->      parsePassStmt stream
        |   Some(Token.PyBreak( _ , _ , _ ), _ ) ->     parseBreakStmt stream
        |   Some(Token.PyContinue( _ , _ , _ ), _ ) ->  parseContinueStmt stream
        |   Some(Token.PyReturn( _ , _ , _ ), _ ) ->    parseReturnStmt stream
        |   Some(Token.PyRaise( _ , _ , _ ), _ ) ->     parseRaiseStmt stream
        |   Some(Token.PyImport( _ , _ , _ ), _ ) ->    parseImportStmt stream
        |   Some(Token.PyFrom( _ , _ , _ ), _ ) ->      parseImportStmt stream
        |   Some(Token.PyGlobal( _ , _ , _ ), _ ) ->    parseGlobalStmt stream
        |   Some(Token.PyNonlocal( _ , _ , _ ), _ ) ->  parseNonlocalStmt stream
        |   Some(Token.PyAssert( _ , _ , _ ), _ ) ->    parseAssertStmt stream
        |   Some( _ , _ ) ->                            parseExprStmt stream
        |   _ ->    raise (SyntaxError(List.head stream, "Empty token stream!"))

    and parseExprStmt (stream : TokenStream) =
        (Node.Empty, stream )

    and parseAnnAssign (stream : TokenStream) =
        (Node.Empty, stream )

    and parseTestListStarExpr (stream : TokenStream) =
        (Node.Empty, stream )

    and parseDelStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyDel( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'del' in del statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        let two, rest3 = parseExprList rest
        (Node.DelStmt(spanStart, getPosition(rest3), one, two), rest3 )

    and parsePassStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyPass( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'pass' in pass statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        (Node.PassStmt(spanStart, getPosition(rest), one), rest )

    and parseBreakStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyBreak( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'break' in break statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        (Node.BreakStmt(spanStart, getPosition(rest), one), rest )

    and parseContinueStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyContinue( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'continue' in continue statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        (Node.ContinueStmt(spanStart, getPosition(rest), one), rest )

    and parseReturnStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyReturn( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'return' in return statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        match tryToken rest with
        |   Some(Token.Newline( _ , _ , _ ), _ )
        |   Some(Token.PySemiColon( _ , _ , _ ), _ ) ->
                (Node.ReturnStmt(spanStart, getPosition(rest), one, Node.Empty), rest )
        |   Some( _ , _ ) ->
                let node, rest2 = parseTestListStarExpr rest
                (Node.ReturnStmt(spanStart, getPosition(rest2), one, node), rest2 )
        |   _ ->
                raise (SyntaxError(List.head rest, "Empty token stream!"))

    and parseRaiseStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyRaise( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'raise' in raise statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        match tryToken rest with
        |   Some(Token.Newline( _ , _ , _ ), _ )
        |   Some(Token.PySemiColon( _ , _ , _ ), _ ) ->
                (Node.RaiseStmt(spanStart, getPosition(rest), one, Node.Empty, Token.Empty, Node.Empty), rest )
        |   Some( _ , _ ) ->
                let left, rest2 = parseTest rest
                match tryToken rest2 with
                |   Some(Token.PyFrom( _ , _ , _ ), rest3 ) ->
                        let two = List.head rest2
                        let right, rest4 = parseTest rest3
                        (Node.RaiseStmt(spanStart, getPosition(rest4), one, left, two, right), rest4 )
                |   Some( _ , _ ) ->
                        (Node.RaiseStmt(spanStart, getPosition(rest2), one, left, Token.Empty, Node.Empty), rest2 )
                |   _ ->
                        raise (SyntaxError(List.head rest , "Empty token stream!"))        
        |   _ ->
                raise (SyntaxError(List.head rest , "Empty token stream!"))

    and parseImportStmt (stream : TokenStream) =
        match tryToken stream with
        |   Some(Token.PyImport( _ , _ , _ ), _ ) ->    parseImportNameStmt stream
        |   Some(Token.PyFrom( _ , _ , _ ), _ ) ->      parseImportFromStmt stream
        |   Some( _ , _ ) ->    raise (SyntaxError(List.head stream, "Expecting 'import' or 'from' in import statement!"))
        |   _ ->    raise (SyntaxError(List.head stream, "Empty token stream!"))

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
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyGlobal( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'global' in global statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        let mutable nodes : Token list = []
        let mutable separators : Token list = []
        let first, rest2 = match tryToken rest with
                            |   Some(Token.Name( _ , _ , _ , _ ), rest3 ) ->
                                    List.head rest, rest3
                            |   Some( _ , _ ) ->
                                    raise (SyntaxError(List.head rest, "Expecting name literal in 'global' statement!"))
                            |   _ ->
                                    raise (SyntaxError(List.head rest, "Empty token stream!"))
        nodes <- first :: nodes
        let mutable restAgain = rest2
        while   match tryToken restAgain with
                |   Some(Token.PyComma( _ , _ , _ ), rest5 ) ->
                        separators <- List.head restAgain :: separators
                        match tryToken rest5 with
                        |   Some (Token.Name( _ , _ , _ , _ ), rest6 ) ->
                                nodes <- List.head rest5 :: nodes
                                restAgain <- rest6
                                true
                        |   Some ( _ , _ ) ->
                                raise (SyntaxError(List.head rest5, "Expecting name literal in 'global' statement!"))
                        |   _ ->    raise (SyntaxError(List.head restAgain, "Empty token stream!"))
                |   Some ( _ , _ ) ->   false
                |   _ ->    raise (SyntaxError(List.head restAgain, "Empty token stream!"))
            do ()
        (Node.GlobalStmt(spanStart, getPosition(stream), one, List.toArray(List.rev nodes), List.toArray(List.rev separators)), stream )

    and parseNonlocalStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyNonlocal( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'nonlocal' in nonlocal statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        let mutable nodes : Token list = []
        let mutable separators : Token list = []
        let first, rest2 = match tryToken rest with
                            |   Some(Token.Name( _ , _ , _ , _ ), rest3 ) ->
                                    List.head rest, rest3
                            |   Some( _ , _ ) ->
                                    raise (SyntaxError(List.head rest, "Expecting name literal in 'nonlocal' statement!"))
                            |   _ ->
                                    raise (SyntaxError(List.head rest, "Empty token stream!"))
        nodes <- first :: nodes
        let mutable restAgain = rest2
        while   match tryToken restAgain with
                |   Some(Token.PyComma( _ , _ , _ ), rest5 ) ->
                        separators <- List.head restAgain :: separators
                        match tryToken rest5 with
                        |   Some (Token.Name( _ , _ , _ , _ ), rest6 ) ->
                                nodes <- List.head rest5 :: nodes
                                restAgain <- rest6
                                true
                        |   Some ( _ , _ ) ->
                                raise (SyntaxError(List.head rest5, "Expecting name literal in 'nonlocal' statement!"))
                        |   _ ->    raise (SyntaxError(List.head restAgain, "Empty token stream!"))
                |   Some ( _ , _ ) ->   false
                |   _ ->    raise (SyntaxError(List.head restAgain, "Empty token stream!"))
            do ()
        (Node.NonlocalStmt(spanStart, getPosition(stream), one, List.toArray(List.rev nodes), List.toArray(List.rev separators)), stream )

    and parseAssertStmt (stream : TokenStream) =
        let spanStart = getPosition stream
        let one, rest = match tryToken stream with
                        |   Some(Token.PyAssert( _ , _ , _ ), rest2 ) ->
                                List.head stream, rest2
                        |   Some( _ , _ ) ->
                                raise (SyntaxError(List.head stream, "Expecting 'assert' in assert statement!"))
                        |   _ ->
                                raise (SyntaxError(List.head stream, "Empty token stream!"))
        match tryToken rest with
        |   Some(Token.Newline( _ , _ , _ ), _ )
        |   Some(Token.PySemiColon( _ , _ , _ ), _ ) ->
                (Node.AssertStmt(spanStart, getPosition(rest), one, Node.Empty, Token.Empty, Node.Empty), rest )
        |   Some( _ , _ ) ->
                let left, rest2 = parseTest rest
                match tryToken rest2 with
                |   Some(Token.PyComma( _ , _ , _ ), rest3 ) ->
                        let two = List.head rest2
                        let right, rest4 = parseTest rest3
                        (Node.AssertStmt(spanStart, getPosition(rest4), one, left, two, right), rest4 )
                |   Some( _ , _ ) ->
                        (Node.AssertStmt(spanStart, getPosition(rest2), one, left, Token.Empty, Node.Empty), rest2 )
                |   _ ->
                        raise (SyntaxError(List.head rest , "Empty token stream!"))        
        |   _ ->
                raise (SyntaxError(List.head rest , "Empty token stream!"))

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
