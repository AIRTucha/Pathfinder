module PathfinderTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Pathfinder exposing (..)
import Dict exposing(..)

{-| Main test string which is used for testings
-}
testStr: String
testStr = "string"
{-| Derived test string which are used for tests whic requers multiple string values
-}
testStr1 = testStr ++ "1"
testStr2 = testStr ++ "2"
testStr3 = testStr ++ "3"
testStr4 = testStr ++ "4"
{- Ttest suit for parse
-}
suite : Test
suite =
    describe "Parse"
        [ describe "toString"
            [ test "int" <|
                \_ -> 
                    Pathfinder.toString int
                        |> Expect.equal "Int"
            , test "float" <|
                \_ -> 
                    Pathfinder.toString float
                        |> Expect.equal "Float"
            , test "string" <|
                \_ -> 
                    Pathfinder.toString str
                        |> Expect.equal "String"
            , test "ordered" <|
                \_ -> 
                    Pathfinder.toString (query <?> any)
                        |> Expect.equal "( Query ) ? ( Any )"
            , test "unordere" <|
                \_ ->
                    Pathfinder.toString (p "test" <&> any <&> str)
                        |> Expect.equal "( Path test ) & ( Any ) & ( String )"
            , test "comples" <|
                \_ ->
                    Pathfinder.toString (p "test" <&> (any <&> str) <*> (int </> float))
                        |> Expect.equal "( ( Path test ) & ( Any ) & ( String ) ) * ( ( Int ) / ( Float ) )"
            ]
        , describe "Examples"
            [ test "Parse name" <|
                let
                    resultHanlder result =
                       case result of 
                            Str name ->
                                Just name
                            
                            _ ->
                                Nothing
                in
                    \_ ->
                        "someUrl/userName"
                            |> parse (p "someUrl" </> str)
                            |> resultHanlder
                            |> Expect.equal ( Just "userName")
            , test "Parse Query" <|
                let
                    resultHanlder result =
                       case result of 
                            Interger age ->
                                Just age
                            
                            _ ->
                                Nothing
                in
                    \_ ->
                        "someUrl?age=10"
                            |> parse (p "someUrl" <?> (p "age" <=> int))
                            |> resultHanlder
                            |> Expect.equal ( Just 10)
            , test "Parse Multiple values" <|
                let
                    resultHanlder result =
                       case result of 
                            MultyValue [ Str name, Interger id] ->
                                Just (name, id)
                            
                            _ ->
                                Nothing
                in
                    \_ ->
                        "someUrl/userName/1/someRandomStuff"
                            |> parse (p "someUrl" </> str </> int </> any )
                            |> resultHanlder
                            |> Expect.equal ( Just ("userName", 1))
            , test "Parse fails" <|
                let
                    resultHanlder result =
                       case result of 
                            Floating value->
                                Ok value
                            
                            Failure err ->
                                Err err
                            
                            _ ->
                                Err "Unexpected path."
                in
                    \_ ->
                        "10*3.1415"
                            |> parse ( any </> float )
                            |> resultHanlder
                            |> Expect.equal ( Err "10*3.1415 does not contain /")
            ]
        , describe "Build a tree"
            [ describe "Random cases" <|
                [ test "parsing of url and typical query" <|
                    \_ ->
                        (p "test" <?> ((p "name" <=> str) <&> (p "age" <=> int )))
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    ( NodeURL <| ParsePath "test") 
                                    ( UnorderedURL '&' 
                                        [ (OrderedURL '=' (NodeURL <| ParsePath "name") (NodeURL <| ParseStr))
                                        , (OrderedURL '=' (NodeURL <| ParsePath "age") (NodeURL <| ParseInt)) 
                                        ]
                                    )
                                ) 
                ]
            , describe "Ordered devider between" 
                [ test "two simple nodes" <|
                    \_ -> 
                        p testStr </> int
                            |> Expect.equal ( OrderedURL '/' (NodeURL <| ParsePath testStr) (NodeURL ParseInt) )
                , test "three simple nodes" <|
                    \_ -> 
                        float </> int <?> p testStr 
                            |> Expect.equal ( OrderedURL '/' (NodeURL ParseFloat) <| OrderedURL '?' (NodeURL ParseInt) (NodeURL <| ParsePath testStr) )
                , test "three simple nodes, left" <|
                    \_ -> 
                        float <?> (int </> p testStr)
                            |> Expect.equal ( OrderedURL '?' (NodeURL ParseFloat) <| OrderedURL '/' (NodeURL ParseInt) (NodeURL <| ParsePath testStr) )
                , test "three simple nodes, right" <|
                    \_ -> 
                        (float </> int) <&> p testStr 
                            |> Expect.equal ( UnorderedURL '&' [ OrderedURL '/' (NodeURL ParseFloat) (NodeURL ParseInt), NodeURL <| ParsePath testStr ])
                , test "node and two other nodes devided unorderedly" <|
                    \_ -> 
                        float </> int <&> p testStr 
                            |> Expect.equal ( OrderedURL '/' (NodeURL ParseFloat) ( UnorderedURL '&' [NodeURL ParseInt,NodeURL (ParsePath testStr)] ) )
                , test "node and two other nodes devided unorderedly, left" <|
                    \_ -> 
                        (float </> int) <&> p testStr 
                            |> Expect.equal ( UnorderedURL '&' [ OrderedURL '/' (NodeURL ParseFloat) (NodeURL ParseInt), NodeURL <| ParsePath testStr ])
                , test "node and two other nodes devided unorderedly, right" <|
                    \_ -> 
                        float </> (int <&> p testStr)
                            |> Expect.equal ( OrderedURL '/' (NodeURL ParseFloat) ( UnorderedURL '&' [NodeURL ParseInt,NodeURL (ParsePath testStr)] ) )
                , test "two ordered subtrees" <|
                    \_ ->
                        (int </> float) <?> (str </> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat)) 
                                    (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                )
                , test "ordered and unordered subtrees" <|
                    \_ ->
                        (int </> float) <?> (str <&> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat))
                                    (UnorderedURL '&' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                )
                , test "unordered and ordered subtrees" <|
                    \_ ->
                        (int <*> float) <?> (str </> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (UnorderedURL '*' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                )
                , test "two unordered subtrees" <|
                    \_ ->
                        (int <&> float) <?> (str <&> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (UnorderedURL '&' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    (UnorderedURL '&' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                )
                ]
            , describe "Unordered devider between"
                [ test "two simple nodes" <|
                    \_ -> 
                        float <&> p testStr 
                            |> Expect.equal ( UnorderedURL '&' [NodeURL ParseFloat, NodeURL (ParsePath testStr)])
                , test "three simple nodes" <|
                    \_ -> 
                        float <&> int <&> p testStr
                            |> Expect.equal ( UnorderedURL '&' [ NodeURL ParseFloat, NodeURL ParseInt, NodeURL <| ParsePath testStr] )
                , test "three simple nodes, left" <|
                    \_ -> 
                        (float <&> int) <&> p testStr
                            |> Expect.equal ( UnorderedURL '&' [ NodeURL ParseFloat, NodeURL ParseInt, NodeURL <| ParsePath testStr] )
                , test "three simple nodes, right" <|
                    \_ -> 
                        float <&> (int <&> p testStr)
                            |> Expect.equal ( UnorderedURL '&' [ NodeURL ParseFloat, NodeURL ParseInt, NodeURL <| ParsePath testStr] )
                , test "node and two other nodes devided orderedly" <|
                    \_ -> 
                        float <&> int <?> p testStr
                            |> Expect.equal ( OrderedURL '?' (UnorderedURL '&' ([NodeURL ParseFloat,NodeURL ParseInt])) (NodeURL (ParsePath testStr)) )
                , test "node and two other nodes devided orderedly, left" <|
                    \_ -> 
                        (float <&> int) <?> p testStr
                            |> Expect.equal ( OrderedURL '?' (UnorderedURL '&' ([NodeURL ParseFloat,NodeURL ParseInt])) (NodeURL (ParsePath testStr)) )
                , test "node and two other nodes devided orderedly, right" <|
                    \_ -> 
                        float <&> (int <?> p testStr)
                            |> Expect.equal (  UnorderedURL '&' ([NodeURL ParseFloat,OrderedURL '?' (NodeURL ParseInt) (NodeURL (ParsePath testStr))]) )
                , test "node and two other nodes devided unorderedly with different char" <|
                    \_ ->
                        int <&> str <*> any
                            |> Expect.equal (UnorderedURL '*' [UnorderedURL '&' [NodeURL ParseInt, NodeURL ParseStr], NodeURL ParseAny])
                , test "node and two other nodes devided unorderedly with different char, left" <|
                    \_ ->
                        (int <&> str) <*> any
                            |> Expect.equal (UnorderedURL '*' [UnorderedURL '&' [NodeURL ParseInt, NodeURL ParseStr], NodeURL ParseAny])
                , test "node and two other nodes devided unorderedly with different char, right" <|
                    \_ ->
                        int <&> (str <*> any)
                            |> Expect.equal (UnorderedURL '&' [NodeURL ParseInt, UnorderedURL '*' [NodeURL ParseStr, NodeURL ParseAny]])
                , test "two ordered subtrees" <|
                    \_ ->
                        (int </> float) <&> (str </> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&'
                                    [ (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat)) 
                                    , (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                    ]
                                )
                , test "ordered and unordered subtrees" <|
                    \_ ->
                        (int </> float) <?> (str <&> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat))
                                    (UnorderedURL '&' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                )
                , test "unordered and ordered subtrees" <|
                    \_ ->
                        (int <*> float) <&> (str </> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' 
                                    [ (UnorderedURL '*' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    , (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                    ]
                                )
                , test "two unordered subtrees" <|
                    \_ ->
                        (int <&> float) <&> (str <&> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' ([NodeURL ParseInt,NodeURL ParseFloat,NodeURL ParseStr,NodeURL (ParsePath testStr)]))
                , test "two unordered subtrees, with different char" <|
                    \_ ->
                        (int <*> float) <&> (str <*> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' 
                                    [(UnorderedURL '*' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    ,(UnorderedURL '*' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                    ]
                                )
                , test "two unordered subtrees, with different chars between nodes, left" <|
                    \_ ->
                        (int <&> float) <&> (str <*> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' 
                                    [ NodeURL ParseInt
                                    , NodeURL ParseFloat
                                    , UnorderedURL '*' [NodeURL ParseStr, NodeURL (ParsePath testStr)]
                                    ]
                                )
                , test "two unordered subtrees, with different chars between nodes, right" <|
                    \_ ->
                        (int <&> float) <*> (str <*> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '*' 
                                    ([ UnorderedURL '&' ([NodeURL ParseInt, NodeURL ParseFloat])
                                    ,  NodeURL ParseStr
                                    ,  NodeURL (ParsePath testStr)
                                    ])
                                )
                ]
            ]
        , describe "Parse tree"
            [ describe "Random cases" <|
                [ test "url and typical query from two values" <|
                    \_ ->
                        "test?name=testName&age=18"
                            |> parse (p "test" <?>((p "name" <=> str) <&> (p "age" <=> int) ))
                            |> Expect.equal ( MultyValue [Str "testName", Interger 18 ])
                , test "url and typical query from two values, inverted" <|
                    \_ ->
                        "test?age=18&name=testName"
                            |> parse (p "test" <?>((p "name" <=> str) <&> (p "age" <=> int) ))
                            |> Expect.equal ( MultyValue [Str "testName", Interger 18 ])
                , test "multiple urls" <|
                    \_ ->
                        "test1/test2/test3?name"
                            |> parse ( p "test1" </> p "test2" </> p "test3" <?> str )
                            |> Expect.equal ( Str "name" )
                , test "query inside two urls" <|
                    \_ ->
                        "test1/test2?age=18&name=testName/test3"
                            |> parse ( p "test1" </> p "test2" <?> query </> p "test3")
                            |> Expect.equal ( Query <| Dict.fromList [("age", "18"),("name","testName")] )
                , test "ordered and unordered mix" <|
                    \_ ->
                        "test1/test2?18&name/test3"
                            |> parse ( p "test1" </> p "test2" <?> (p "name" <&> int) </> p "test3")
                            |> Expect.equal ( Interger 18 )
                ]
            , describe "Ordered sub tree"
                [ test "tree and int" <|
                    \_ ->
                        testStr1 ++ "/" ++ testStr2 ++ "/10"
                            |> parse ( (p testStr1 </> p testStr2) </> int )
                            |> Expect.equal ( Interger 10 )
                , test "tree or int" <|
                    \_ ->
                        (testStr1 ++ "/" ++ testStr2) ++ "*10"
                            |> parse ( (p testStr1 </> p testStr2) <*> int )
                            |> Expect.equal ( Interger 10 )
                ]
            , describe "Unordered sub tree"
                [ test "tree and int" <|
                    \_ ->
                        (testStr1 ++ "&" ++ testStr2) ++ "/10"
                            |> parse ( (p testStr1 <&> p testStr2) </> int )
                            |> Expect.equal ( Interger 10 )
                , test "tree or int, same devider" <|
                    \_ ->
                        (testStr1 ++ "*" ++ testStr2) ++ "*10"
                            |> parse ( (p testStr1 <*> p testStr2) <*> int )
                            |> Expect.equal ( Interger 10 )
                , test "tree or int, different deviders" <|
                    \_ ->
                        (testStr1 ++ "*" ++ testStr2) ++ "&10"
                            |> parse ( (p testStr1 <*> p testStr2) <&> int )
                            |> Expect.equal ( Interger 10 )
                ]

            , describe "Path"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr   
                                |> parse (p testStr)
                                |> Expect.equal Succes
                    , describe "Ordered" <|
                        [ test "two paths" <|
                            \_ ->
                                testStr ++ "/" ++ testStr
                                    |> parse (p testStr </> p testStr)
                                    |> Expect.equal Succes
                        , test "path and int" <|
                            \_ ->
                                testStr ++ "/10"
                                    |> parse (p testStr </> int)
                                    |> Expect.equal ( Interger 10 ) 
                        , test "path and float" <|
                            \_ ->
                                testStr ++ "/3.1415"
                                    |> parse (p testStr </> float)
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "path and string" <|
                            \_ ->
                                testStr ++ "/" ++ testStr
                                    |> parse (p testStr </> str)
                                    |> Expect.equal ( Str testStr )
                        , test "path and any" <|
                            \_ ->
                                testStr ++ "/" ++ testStr
                                    |> parse (p testStr </> any)
                                    |> Expect.equal ( Succes )
                        , test "path and query" <|
                            \_ ->
                                testStr ++ "/" ++ testStr ++ "=" ++ testStr
                                    |> parse (p testStr </> query)
                                    |> Expect.equal ( Query <| Dict.fromList [(testStr, testStr)])
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "two paths" <|
                                \_ ->
                                    testStr1 ++ "*" ++ testStr2
                                        |> parse (p testStr1 <*> p testStr2)
                                        |> Expect.equal Succes
                            , test "path or int" <|
                                \_ ->
                                    testStr ++ "&10"
                                        |> parse (p testStr <&> int)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "path or float" <|
                                \_ ->
                                    testStr ++ "&3.1415"
                                        |> parse (p testStr <&> float)
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "path or string" <|
                                \_ ->
                                    testStr1 ++ "*" ++ testStr2
                                        |> parse (p testStr1 <*> str)
                                        |> Expect.equal ( Str testStr2 )
                            , test "path or any" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2
                                        |> parse (p testStr1 <&> any)
                                        |> Expect.equal ( Succes )
                            , test "path or query" <|
                                \_ ->
                                    testStr ++ "&" ++ testStr ++ "=" ++ testStr
                                        |> parse (p testStr <&> query)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr, testStr)])
                            ]
                        , describe "inverted"
                            [ test "two paths" <|
                                \_ ->
                                    testStr2 ++ "*" ++ testStr1
                                        |> parse (p testStr1 <*> p testStr2)
                                        |> Expect.equal Succes
                            , test "path or int" <|
                                \_ ->
                                    "10&" ++ testStr
                                        |> parse (p testStr <&> int)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "path or float" <|
                                \_ ->
                                    "3.1415&" ++ testStr
                                        |> parse (p testStr <&> float)
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "path or string" <|
                                \_ ->
                                    testStr2 ++ "*" ++ testStr1
                                        |> parse (p testStr1 <*> str)
                                        |> Expect.equal ( Str testStr2 )
                            , test "path or any" <|
                                \_ ->
                                    testStr2 ++ "&" ++ testStr1
                                        |> parse (p testStr1 <&> any)
                                        |> Expect.equal ( Succes )
                            , test "path or query" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr
                                        |> parse (p testStr1 <&> query)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr)])
                            ]
                        ]
                    ]
                , describe "Error" 
                    [ test "Incorrect path" <|
                        \_ ->
                            testStr1
                                |> parse (p testStr)
                                |> Expect.equal ( Failure <| testStr ++ " is not " ++ testStr1 )
                    , test "Incorrect path after ordered divider" <|
                        \_ ->
                            testStr1 ++ "/" ++ testStr2
                                |> parse ( p testStr1 </> p testStr )
                                |> Expect.equal ( Failure <| testStr ++ " is not " ++ testStr2 )
                    , test "Incorrect path after unordered divider" <|
                        \_ ->
                                testStr1 ++ "&" ++ testStr2
                                    |> parse ( p testStr1 <&> p testStr )
                                    |> Expect.equal ( Failure <| "Start of " ++ testStr2 ++ " does not have any value which can be corectly parsed by: Path string, separated by &." )
                    , test "Incorrect ordered devider between paths" <|
                        \_ ->
                            let
                                path = testStr1 ++ "/" ++ testStr2
                            in
                                path
                                    |> parse (p testStr1 <?> p testStr2)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between paths" <|
                        \_ ->
                            let
                                path = testStr1 ++ "&" ++ testStr2
                            in
                                path
                                    |> parse (p testStr1 <*> p testStr2)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Path " ++ testStr1 ++ " or Path " ++ testStr2 ++ ", separated by *." )     
                    , test "Incorrect ordered devider instead of unordered one between paths" <|
                        \_ ->
                            let
                                path = testStr1 ++ "/" ++ testStr2
                            in
                                path
                                    |> parse (p testStr1 <?> p testStr2)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )     
                    , test "Incorrect unordered devider instead of ordered one between paths" <|
                        \_ ->
                            let
                                path = testStr1 ++ "/" ++ testStr2
                            in
                                path
                                    |> parse (p testStr1 <*> p testStr2)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Path " ++ testStr1 ++ " or Path " ++ testStr2 ++ ", separated by *." )      
                    ]
                ]
            , describe "Integer"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            "10"
                                |> parse int 
                                |> Expect.equal ( Interger 10 ) 
                    , describe "Ordered" <|
                        [ test "two ints" <|
                            \_ ->
                                "10/9"
                                    |> parse  (int </> int) 
                                    |> Expect.equal ( MultyValue <| Interger 10 :: Interger 9 :: [] )
                        , test "int and path" <|
                            \_ ->
                                "9/" ++ testStr
                                    |> parse (int </> p testStr)
                                    |> Expect.equal ( Interger 9 )
                        , test "int and float" <|
                            \_ ->
                                "10/9.123"
                                    |> parse  (int </> float) 
                                    |> Expect.equal ( MultyValue <| Interger 10 :: Floating 9.123 :: [] )
                        , test "int and string" <|
                            \_ ->
                                "10/" ++ testStr
                                    |> parse  (int </> str) 
                                    |> Expect.equal ( MultyValue <| Interger 10 :: Str testStr :: [] )
                        , test "int and any" <|
                            \_ ->
                                "10/" ++ testStr
                                    |> parse  (int </> any) 
                                    |> Expect.equal ( Interger 10 )
                        , test "int and query" <|
                            \_ ->
                                "10/" ++ testStr ++ "=" ++ testStr
                                    |> parse (int </> query)
                                    |> Expect.equal ( MultyValue 
                                        [ Interger 10
                                        , Query <| Dict.fromList [(testStr, testStr)]
                                        ]
                                    )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "int or path" <|
                                \_ ->
                                     "10&" ++ testStr
                                        |> parse (int <&> p testStr)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "int or float" <|
                                \_ ->
                                    "10&3.1415"
                                        |> parse (int <&> float )
                                        |> Expect.equal ( MultyValue <| [Interger 10, Floating 3.1415] )
                            , test "int or string" <|
                                \_ ->
                                    "10*" ++ testStr
                                        |> parse (int <*> str)
                                        |> Expect.equal ( MultyValue <| [Interger 10, Str testStr] )
                            , test "int or any" <|
                                \_ ->
                                        "10&" ++ testStr
                                            |> parse ( int <&> any)
                                            |> Expect.equal ( Interger 10 )
                            , test "int or query" <|
                                \_ ->
                                    "10&" ++ testStr ++ "=" ++ testStr
                                        |> parse ( int <&> query )
                                        |> Expect.equal (MultyValue <| [ Interger 10, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        , describe "inverted"
                            [ test "int or path" <|
                                \_ ->
                                     testStr ++ "&10"
                                        |> parse (int <&> p testStr)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "int or float" <|
                                \_ ->
                                    "3.1415&10"
                                        |> parse (int <&> float )
                                        |> Expect.equal ( MultyValue <| [Interger 10, Floating 3.1415] )
                            , test "int or string" <|
                                \_ ->
                                    testStr ++ "*10"
                                        |> parse (int <*> str)
                                        |> Expect.equal ( MultyValue <| [Interger 10, Str testStr] )
                            , test "int or any" <|
                                \_ ->
                                    testStr ++ "&10"
                                        |> parse ( int <&> any)
                                        |> Expect.equal ( Interger 10 )
                            , test "int or query" <|
                                \_ ->
                                    testStr ++ "=" ++ testStr ++ "&10"
                                        |> parse ( int <&> query )
                                        |> Expect.equal (MultyValue <| [ Interger 10, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect int" <|
                        \_ ->
                            "9.14"
                                |> parse int
                                |> Expect.equal (Failure "could not convert string '9.14' to an Int")
                    , test "Incorrect separator between ints" <|
                        \_ ->
                            "10?43"
                                |> parse (int </> int)
                                |> Expect.equal ( Failure <| "10?43 does not contain /")
                    , test "Incorrect int after ordered devider" <|
                        \_ ->
                            "5?a3"
                                |> parse (int <?> int)
                                |> Expect.equal ( Failure "could not convert string 'a3' to an Int" )
                    , test "Incorrect int after unordered divider" <|
                        \_ ->
                            "10&a43"
                                |> parse ( int <&> int )
                                |> Expect.equal ( Failure <| "Start of a43 does not have any value which can be corectly parsed by: Int, separated by &." )
                    , test "Incorrect ordered devider between paths" <|
                        \_ ->
                            let
                                path = "10/34"
                            in
                                path
                                    |> parse (int <?> int)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between paths" <|
                        \_ ->
                            let
                                path = "10&23"
                            in
                                path
                                    |> parse (int <*> int)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Int or Int, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between paths" <|
                        \_ ->
                            let
                                path = "10/2"
                            in
                                path
                                    |> parse (int <&> int)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Int or Int, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between paths" <|
                        \_ ->
                            let
                                path = "10*20"
                            in
                                path
                                    |> parse (int </> int)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Floating"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            "3.1415"
                                |> parse float 
                                |> Expect.equal ( Floating 3.1415 ) 
                    , describe "Ordered" <|
                        [ test "two floats" <|
                            \_ ->
                                "3.1415/43.2"
                                    |> parse  (float </> float) 
                                    |> Expect.equal ( MultyValue <| Floating 3.1415 :: Floating 43.2 :: [] )
                        , test "float and path" <|
                            \_ ->
                                "3.1415/" ++ testStr
                                    |> parse (float </> p testStr)
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "float and float" <|
                            \_ ->
                                "3.1415/9"
                                    |> parse  (float </> int) 
                                    |> Expect.equal ( MultyValue <| Floating 3.1415 :: Interger 9 :: [] )
                        , test "float and string" <|
                            \_ ->
                                "3.1415/" ++ testStr
                                    |> parse  (float </> str) 
                                    |> Expect.equal ( MultyValue <| Floating 3.1415 :: Str testStr :: [] )
                        , test "float and any" <|
                            \_ ->
                                "3.1415/" ++ testStr
                                    |> parse  (float </> any) 
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "float and query" <|
                            \_ ->
                                "3.1415/" ++ testStr ++ "=" ++ testStr
                                    |> parse (float </> query)
                                    |> Expect.equal ( MultyValue 
                                        [ Floating 3.1415
                                        , Query <| Dict.fromList [(testStr, testStr)]
                                        ]
                                    )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "float or path" <|
                                \_ ->
                                     "3.1415&" ++ testStr
                                        |> parse (float <&> p testStr)
                                        |> Expect.equal ( Floating 3.1415 ) 
                            , test "float or int" <|
                                \_ ->
                                    "3.1415&3"
                                        |> parse (float <&> int )
                                        |> Expect.equal ( MultyValue <| [Floating 3.1415, Interger 3] )
                            , test "float or string" <|
                                \_ ->
                                    "3.1415*" ++ testStr
                                        |> parse (float <*> str)
                                        |> Expect.equal ( MultyValue <| [Floating 3.1415, Str testStr] )
                            , test "float or any" <|
                                \_ ->
                                        "3.1415&" ++ testStr
                                            |> parse ( float <&> any)
                                            |> Expect.equal ( Floating 3.1415 )
                            , test "float or query" <|
                                \_ ->
                                    "3.1415&" ++ testStr ++ "=" ++ testStr
                                        |> parse ( float <&> query )
                                        |> Expect.equal (MultyValue <| [ Floating 3.1415, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        , describe "inverted"
                            [ describe "limitations" <|
                                    [ test "float or int" <|
                                        \_ ->
                                            "3&3.1415"
                                                |> parse (float <&> int )
                                                |> Expect.equal ( Failure "Start of 3.1415 does not have any value which can be corectly parsed by: Int, separated by &." )

                                    ]
                            , test "float or path" <|
                                \_ ->
                                     testStr ++ "&3.1415"
                                        |> parse (float <&> p testStr)
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "float or string" <|
                                \_ ->
                                    testStr ++ "*3.1415"
                                        |> parse (float <*> str)
                                        |> Expect.equal ( MultyValue <| [Floating 3.1415, Str testStr] )
                            , test "float or any" <|
                                \_ ->
                                    testStr ++ "&10"
                                        |> parse (float <&> any)
                                        |> Expect.equal ( Floating 10 )
                            , test "float or query" <|
                                \_ ->
                                    testStr ++ "=" ++ testStr ++ "&10"
                                        |> parse ( float <&> query )
                                        |> Expect.equal (MultyValue <| [ Floating 10, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect int" <|
                        \_ ->
                            "9.14f"
                                |> parse float
                                |> Expect.equal (Failure "could not convert string '9.14f' to a Float")
                    , test "Incorrect separator between floats" <|
                        \_ ->
                            "10?43"
                                |> parse (float </> float)
                                |> Expect.equal ( Failure <| "10?43 does not contain /")
                    , test "Incorrect float after ordered devider" <|
                        \_ ->
                            "5?a3"
                                |> parse (float <?> float)
                                |> Expect.equal ( Failure "could not convert string 'a3' to a Float" )
                    , test "Incorrect float after unordered divider" <|
                        \_ ->
                            "10&a43.0"
                                |> parse ( float <&> float )
                                |> Expect.equal ( Failure <| "Start of a43.0 does not have any value which can be corectly parsed by: Float, separated by &." )
                    , test "Incorrect ordered devider between paths" <|
                        \_ ->
                            let
                                path = "10.0/34"
                            in
                                path
                                    |> parse (float <?> float)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between paths" <|
                        \_ ->
                            let
                                path = "10&23.2"
                            in
                                path
                                    |> parse (float <*> float)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Float or Float, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between paths" <|
                        \_ ->
                            let
                                path = "10.1/2.4"
                            in
                                path
                                    |> parse (float <&> float)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Float or Float, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between paths" <|
                        \_ ->
                            let
                                path = "10.2*20.5"
                            in
                                path
                                    |> parse (float </> float)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Str"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr
                                |> parse str 
                                |> Expect.equal ( Str testStr ) 
                    , describe "Ordered" <|
                        [ test "two string" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2
                                    |> parse  (str </> str) 
                                    |> Expect.equal ( MultyValue <| Str testStr1 :: Str testStr2 :: [] )
                        , test "string and path" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2
                                    |> parse (str </> p testStr2)
                                    |> Expect.equal ( Str testStr1 )
                        , test "string and int" <|
                            \_ ->
                                testStr ++ "/9"
                                    |> parse  (str </> int) 
                                    |> Expect.equal ( MultyValue <| Str testStr :: Interger 9 :: [] )
                        , test "string and float" <|
                            \_ ->
                                testStr ++ "/3.1415"
                                    |> parse  (str </> float) 
                                    |> Expect.equal ( MultyValue <| Str testStr :: Floating 3.1415 :: [] )
                        , test "string and any" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2
                                    |> parse  (str </> any) 
                                    |> Expect.equal ( Str testStr1 )
                        , test "string and query" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2 ++ "=" ++ testStr3
                                    |> parse (str </> query)
                                    |> Expect.equal ( MultyValue 
                                        [ Str testStr1
                                        , Query <| Dict.fromList [(testStr2, testStr3)]
                                        ]
                                    )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "string or path" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2
                                        |> parse (str <&> p testStr2)
                                        |> Expect.equal ( Str testStr1 ) 
                            , test "string or float" <|
                                \_ ->
                                    testStr ++ "&3.1415"
                                        |> parse (str <&> float )
                                        |> Expect.equal ( MultyValue <| [Str testStr, Floating 3.1415] )
                            , test "string or int" <|
                                \_ ->
                                    testStr ++ "*10"
                                        |> parse (str <*> int)
                                        |> Expect.equal ( MultyValue <| [Str testStr, Interger 10] )
                            , test "string or any" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2
                                        |> parse ( str <&> any)
                                        |> Expect.equal ( Str testStr1 )
                            , test "string or query" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr3
                                        |> parse ( str <&> query )
                                        |> Expect.equal (MultyValue <| [ Str testStr1, Query <| Dict.fromList [(testStr2, testStr3)] ])
                            ]
                        , describe "inverted"
                            [ describe "limitations"
                                [ test "string or path" <|
                                    \_ ->
                                        testStr2 ++ "&" ++ testStr1
                                            |> parse (str <&> p testStr2)
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Path " ++ testStr2 ++ ", separated by &." )
                                , test "string or float" <|
                                    \_ ->
                                        "3.1415&" ++ testStr
                                            |> parse (str <&> float )
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Float, separated by &.")
                                , test "string or int" <|
                                    \_ ->
                                        "10*" ++ testStr
                                            |> parse (str <*> int)
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Int, separated by *." )
                                , test "string or any" <|
                                    \_ ->
                                        "10&" ++ testStr
                                            |> parse ( str <&> any)
                                            |> Expect.equal ( Str "10" )
                                , test "string or query" <|
                                    \_ ->
                                        testStr2 ++ "=" ++ testStr3 ++ "&" ++ testStr1
                                            |> parse ( str <&> query )
                                            |> Expect.equal (Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Query, separated by &.")
                                ]
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect separator between values" <|
                        \_ ->
                            let
                                path = testStr ++ "?" ++ testStr
                            in
                                path
                                    |> parse (str </> str)
                                    |> Expect.equal ( Failure <| path ++ " does not contain /")
                    , test "Incorrect ordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parse (str <?> str)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "&" ++ testStr
                            in
                                path
                                    |> parse (str <*> str)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: String or String, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parse (str <&> str)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: String or String, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "*" ++ testStr
                            in
                                path
                                    |> parse (str </> str)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Any"
                  [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr
                                |> parse any 
                                |> Expect.equal ( Succes ) 
                    , describe "Ordered" <|
                        [ test "two any" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2
                                    |> parse  (any </> any) 
                                    |> Expect.equal ( Succes )
                        , test "any and path" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2
                                    |> parse (any </> p testStr2)
                                    |> Expect.equal ( Succes )
                        , test "any and int" <|
                            \_ ->
                                testStr ++ "/9"
                                    |> parse  (any </> int) 
                                    |> Expect.equal ( Interger 9 )
                        , test "any and float" <|
                            \_ ->
                                testStr ++ "/3.1415"
                                    |> parse  (any </> float) 
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "any and string" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2
                                    |> parse  (any </> str) 
                                    |> Expect.equal ( Str testStr2 )
                        , test "any and query" <|
                            \_ ->
                                testStr1 ++ "/" ++ testStr2 ++ "=" ++ testStr3
                                    |> parse (any </> query)
                                    |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "any or path" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2
                                        |> parse (any <&> p testStr2)
                                        |> Expect.equal ( Succes ) 
                            , test "any or float" <|
                                \_ ->
                                    testStr ++ "&3.1415"
                                        |> parse (any <&> float )
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "any or int" <|
                                \_ ->
                                    testStr ++ "*10"
                                        |> parse (any <*> int)
                                        |> Expect.equal ( Interger 10 )
                            , test "any or string" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2
                                        |> parse ( any <&> str)
                                        |> Expect.equal ( Str testStr2 )
                            , test "any or query" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr3
                                        |> parse ( any <&> query )
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                            ]
                        , describe "inverted"
                            [ describe "limitations"
                                [ test "any or path" <|
                                    \_ ->
                                        testStr2 ++ "&" ++ testStr1
                                            |> parse (any <&> p testStr2)
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Path " ++ testStr2 ++ ", separated by &." )
                                , test "any or float" <|
                                    \_ ->
                                        "3.1415&" ++ testStr
                                            |> parse (any <&> float )
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Float, separated by &.")
                                , test "any or int" <|
                                    \_ ->
                                        "10*" ++ testStr
                                            |> parse (any <*> int)
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Int, separated by *." )
                                , test "any or query" <|
                                    \_ ->
                                        testStr2 ++ "=" ++ testStr3 ++ "&" ++ testStr1
                                            |> parse ( any <&> query )
                                            |> Expect.equal (Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Query, separated by &.")
                                ]
                            , test "any or string" <|
                                \_ ->
                                    "10&" ++ testStr
                                        |> parse ( any <&> str)
                                        |> Expect.equal ( Str testStr )
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect separator between values" <|
                        \_ ->
                            let
                                path = testStr ++ "?" ++ testStr
                            in
                                path
                                    |> parse (any </> any)
                                    |> Expect.equal ( Failure <| path ++ " does not contain /")
                    , test "Incorrect ordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parse (any <?> any)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "&" ++ testStr
                            in
                                path
                                    |> parse (any <*> any)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Any or Any, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parse (any <&> any)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Any or Any, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "*" ++ testStr
                            in
                                path
                                    |> parse (any </> any)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Query"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr1 ++ "=" ++ testStr2 
                                |> parse query 
                                |> Expect.equal ( Query <| Dict.fromList [(testStr1, testStr2)] ) 
                    , describe "Ordered" <|
                        [ test "two query" <|
                            \_ ->
                                testStr1 ++ "=" ++ testStr2 ++ "/" ++ testStr3 ++ "=" ++ testStr4
                                    |> parse  (query </> query) 
                                    |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Query <| Dict.fromList [(testStr3, testStr4)]] )
                        , test "query and path" <|
                            \_ ->
                                testStr1 ++ "=" ++ testStr2 ++ "/" ++ testStr3
                                    |> parse (query </> p testStr3)
                                    |> Expect.equal ( Query <| Dict.fromList [(testStr1, testStr2)] )
                        , test "query and int" <|
                            \_ ->
                                testStr1 ++ "=" ++ testStr2 ++ "/9"
                                    |> parse  (query </> int) 
                                    |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Interger 9] )
                        , test "query and float" <|
                            \_ ->
                                testStr1 ++ "=" ++ testStr2 ++ "/3.1415"
                                    |> parse  (query </> float) 
                                    |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Floating 3.1415] )
                        , test "query and string" <|
                            \_ ->
                                testStr1 ++ "=" ++ testStr2 ++ "/" ++ testStr3
                                    |> parse  (query </> str) 
                                    |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Str testStr3] )
                        , test "query and any" <|
                            \_ ->
                                testStr2 ++ "=" ++ testStr3 ++ "/" ++ testStr1
                                    |> parse (query </> any)
                                    |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "two query" <|
                                \_ ->
                                    testStr1 ++ "=" ++ testStr2 ++ "&" ++ testStr3 ++ "=" ++ testStr4
                                        |> parse  (query <&> query) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Query <| Dict.fromList [(testStr3, testStr4)]] )
                            , test "query and path" <|
                                \_ ->
                                    testStr1 ++ "=" ++ testStr2 ++ "*" ++ testStr3
                                        |> parse (query <*> p testStr3)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr1, testStr2)] )
                            , test "query and int" <|
                                \_ ->
                                    testStr1 ++ "=" ++ testStr2 ++ "&9"
                                        |> parse  (query <&> int) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Interger 9] )
                            , test "query and float" <|
                                \_ ->
                                    testStr1 ++ "=" ++ testStr2 ++ "*3.1415"
                                        |> parse  (query <*> float) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Floating 3.1415] )
                            , test "query and string" <|
                                \_ ->
                                    testStr1 ++ "=" ++ testStr2 ++ "*" ++ testStr3
                                        |> parse  (query <*> str) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Str testStr3] )
                            , test "query and any" <|
                                \_ ->
                                    testStr2 ++ "=" ++ testStr3 ++ "&" ++ testStr1
                                        |> parse (query <&> any)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                            ]
                        , describe "inverted"
                             [ test "two query" <|
                                \_ ->
                                    testStr4 ++ "=" ++ testStr3 ++ "&" ++ testStr2 ++ "=" ++ testStr1
                                        |> parse  (query <&> query) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr4, testStr3)], Query <| Dict.fromList [(testStr2, testStr1)]] )
                            , test "query and path" <|
                                \_ ->
                                    testStr3 ++ "*" ++ testStr1 ++ "=" ++ testStr2
                                        |> parse (query <*> p testStr3)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr1, testStr2)] )
                            , test "query and int" <|
                                \_ ->
                                    "9&" ++ testStr1 ++ "=" ++ testStr2 
                                        |> parse  (query <&> int) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Interger 9] )
                            , test "query and float" <|
                                \_ ->
                                    "3.1415*" ++ testStr1 ++ "=" ++ testStr2
                                        |> parse  (query <*> float) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Floating 3.1415] )
                            , test "query and string" <|
                                \_ ->
                                    testStr3 ++ "*" ++ testStr1 ++ "=" ++ testStr2
                                        |> parse  (query <*> str) 
                                        |> Expect.equal ( MultyValue [Query <| Dict.fromList [(testStr1, testStr2)], Str testStr3] )
                            , test "query and any" <|
                                \_ ->
                                    testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr3 
                                        |> parse (query <&> any)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect separator between values" <|
                        \_ ->
                            let
                                path = testStr ++ "=" ++ testStr ++ "?" ++ testStr ++ "=" ++ testStr
                            in
                                path
                                    |> parse (query </> query)
                                    |> Expect.equal ( Failure <| path ++ " does not contain /")
                    , test "Incorrect ordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "=" ++ testStr ++ "/" ++ testStr ++ "=" ++ testStr
                            in
                                path
                                    |> parse (query <?> query)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "=" ++ testStr ++ "&" ++ testStr ++ "=" ++ testStr
                            in
                                path
                                    |> parse (query <*> query)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Query or Query, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "=" ++ testStr ++ "/" ++ testStr ++ "=" ++ testStr
                            in
                                path
                                    |> parse (query <&> query)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Query or Query, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "=" ++ testStr ++ "*" ++ testStr ++ "=" ++ testStr
                            in
                                path
                                    |> parse (query </> query)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            ]
        ]
        