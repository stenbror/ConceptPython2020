namespace TestPythonCoreFramework.Runtime

module TestPythonCoreParser =

    open Xunit

    open PythonCoreFramework.Runtime.PythonCoreParser

    [<Fact>]
    let ``NamedExpr - Expecting correct`` () =

        let nodes, rest =   [ 
                                Token.Name(0ul, 2ul, [||], "ab"); 
                                Token.PyColonAssign(3ul, 5ul, [||]); 
                                Token.Name(6ul, 8ul, [| |], "cd"); 
                                Token.EOF(8ul) 
                            ] |> parseNamedExpr

        Assert.Equal (rest, [| Token.EOF(8ul) |]; )
        Assert.Equal (
                        Node.NamedExpr(0ul, 8ul, 
                            Node.Name(0ul, 3ul, Token.Name(0ul, 2ul, [||], "ab")),
                            Token.PyColonAssign(3ul, 5ul, [||]),
                            Node.Name(6ul, 8ul, Token.Name(6ul, 8ul, [||], "cd"))
                        ), nodes )
