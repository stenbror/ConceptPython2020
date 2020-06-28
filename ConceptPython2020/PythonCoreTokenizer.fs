
namespace PythonCoreFramework.Runtime

module PythonCoreTokenizer =

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

    let keywords =  
        [ 
            ( "and",        Token.PyAnd );
            ( "as",         Token.PyAs );
            ( "assert",     Token.PyAssert );
            ( "async",      Token.PyAsync );
            ( "await",      Token.PyAwait );
            ( "break",      Token.PyBreak );
            ( "class",      Token.PyClass );
            ( "continue",   Token.PyContinue );
            ( "def",        Token.PyDef );
            ( "del",        Token.PyDel );
            ( "elif",       Token.PyElif );
            ( "else",       Token.PyElse );
            ( "except",     Token.PyExcept );
            ( "finally",    Token.PyFinally );
            ( "for",        Token.PyFor );
            ( "from",       Token.PyFrom );
            ( "global",     Token.PyGlobal );
            ( "if",         Token.PyIf );
            ( "import",     Token.PyImport );
            ( "in",         Token.PyIn );
            ( "is",         Token.PyIs );
            ( "lambda",     Token.PyLambda );
            ( "nonlocal",   Token.PyNonlocal );
            ( "not",        Token.PyNot );
            ( "or",         Token.PyOr );
            ( "pass",       Token.PyPass );
            ( "raise",      Token.PyRaise );
            ( "return",     Token.PyReturn );
            ( "try",        Token.PyTry );
            ( "while",      Token.PyWhile );
            ( "with",       Token.PyWith );
            ( "yield",      Token.PyYield );
            ( "False",      Token.PyFalse );
            ( "None",       Token.PyNone );
            ( "True",       Token.PyTrue );
        ] |> Map.ofList

