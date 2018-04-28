module Pathfinder exposing
    ( int
    , float
    , p
    , str
    , any
    , query
    , toString
    , parse
    , (</>)
    , (<=>)
    , (<?>)
    , (<&>)
    , (<*>)
    , (<%>)
    , orderedDevider
    , unorderedDevider
    , ParsingResult(..)
    , URL(..)
    , ParseNode(..)
    )

{-| Typesafe eDSL for URL parsing. The library can parse single value, multiple values with a strict or arbitrary order, queries. It also allows writing typesafe parser for a query.

# Parsing nodes

Shortcut functions which handle creation of parsing nodes.

@docs int
    , float
    , p
    , str
    , any
    , query

# Ordered Dividers

Predefined infix operators for an ordered combination of parsing nodes.

    url = int </> str

    parse url "10/somevalue" 
    -- MultiValue [ Integer 10, Str "somevalue" ]

    parse url "somevalue/10" 
    -- Failuer "could not convert string 'somevalue' to an Int"

@docs (</>)
    , (<=>)
    , (<?>)

# Unordered Dividers

Predefined infix operators for an unordered combination of parsing nodes.

    url = int <&> str

    parse url "10&somevalue" 
    -- MultiValue [ Integer 10, Str "somevalue" ]

    parse url "somevalue&10" 
    -- MultiValue [ Integer 10, Str "somevalue" ]

@docs (<&>)
    , (<*>)
    , (<%>)

# Dividers factory

Abstract factories for creation of custom parsing node combinators

@docs orderedDevider
    , unorderedDevider

# Functions

@docs parse
    , toString

# Data Structures

@docs  ParsingResult
    , URL
    , ParseNode
-}


import Break exposing (break)
import List exposing (reverse)
import String exposing(toFloat, toInt, dropLeft, left, length, uncons, cons, fromChar, startsWith)
import Dict exposing(..)
import Maybe
import Result
import Tuple 

-- TYPES


{-| Parsing options
-}
type ParseNode 
    = ParsePath String
    | ParseFloat
    | ParseInt
    | ParseStr
    | ParseAny
    | ParseQuery


{-| Parsing tree
-}
type URL
    = OrderedURL Char URL URL
    | UnorderedURL Char (List URL)
    | NodeURL ParseNode


{-| Possible parsing results
-}
type ParsingResult
    = Integer Int
    | Floating Float
    | Str String
    | MultiValue (List ParsingResult)
    | Failure String
    | Query (Dict String String)
    | Success


-- SHORTCUTS FOR BUILDING OF PARSING TREE

{-| A shortcut for creation of a parsing node for Float

    parse float "3.1415"
    -- Floating 3.1415
-}
float : URL
float =
    NodeURL ParseFloat


{-| A shortcut for creation of a parsing node for Int

    parse int "13"
    -- Integer 13
-}
int : URL
int =
    NodeURL ParseInt


{-| A shortcut for creation of a parsing node which specifies precise part of a path

    parse ( p "someUrl" </> int ) "someUrl/10"
    -- Integer 10
-}
p : String -> URL
p string =
    NodeURL <| ParsePath string


{-| A shortcut for creation of a parsing node for String

    parse str "name"
    -- Success
-}
str: URL 
str = 
    NodeURL ParseStr


{-| A shortcut for creation of a parsing node which skips a part of the path

    parse (int </> any </> float) "10/some&strange?stuff/3.1415"
    -- MultiValue [ Integer 10, Floating 3.1415 ]
-}
any: URL
any = 
    NodeURL ParseAny


{-| A shortcut for creation of a parsing node for an old good url query

    parse (query) "value1=10&value2=13&value3=name"
    -- Dict.fromList [ ("value1", "10"), ("value2", "13"), ("value3", "name") ]
-}
query : URL
query =
    NodeURL ParseQuery


{-| Create a string which describes the path, it is handy for debugging

    toString (int </> float)  -- ( Int ) / ( Float )
-}
toString: URL -> String
toString url =
    case url of 
        OrderedURL char url1 url2 ->
            "( " ++ toString url1 ++ " ) " ++ (fromChar char) ++ " ( " ++ toString url2 ++ " )"

        UnorderedURL char subUrls ->
            "( " ++ String.join (" ) " ++ (fromChar char) ++ " ( ") (List.map toString subUrls) ++ " )" 

        NodeURL node ->
            case node of
                ParsePath path ->
                    "Path " ++ path   
                
                ParseFloat ->
                    "Float"

                ParseInt ->
                    "Int"
                
                ParseStr ->
                    "String"

                ParseAny ->
                    "Any"
    
                ParseQuery ->
                    "Query"


-- PUBLIC API

{-| Performs parsing of a string in according to provided parsing tree.

    parse (p "path" </> float) "path/3.1415"                    -- Floating 3.1415
    parse (any <?> (p "name" <=> str)) "someStrangeStuff?name"  -- Str "name"
    parse (str <&> int) "19&somePath"                           -- MultiValue [ Integer 19, String "somePath" ]
    parse (p "path" </> any) "somePath/otherPath"               -- Success
-}
parse : URL -> String -> ParsingResult
parse value string =
    case parsingLoop value [] string Nothing of
        Ok ( result, "" ) ->
            makeValue result

        Ok (result, stringEnding ) ->
            Failure <| stringEnding ++ " is not specified"

        Err err ->
            Failure err


{-| Infix shortcut for a function which creates an ordered parsing tree from subnodes provided as arguments which are separated by /.
-}
(</>): URL -> URL -> URL
(</>) = orderedDevider '/'


{-| Infix shortcut for a function which creates an ordered parsing tree from subnodes provided as arguments which are separated by =.
-}
(<=>): URL -> URL -> URL
(<=>) = orderedDevider '='


{-| Infix shortcut for a function which creates an ordered parsing tree from subnodes provided as arguments which are separated by ?.
-}
(<?>): URL -> URL -> URL
(<?>) = orderedDevider '?'


{-| Infix shortcut for a function which creates an unordered parsing tree from subnodes provided as arguments which are separated by &.
-}
(<&>): URL -> URL -> URL
(<&>) = unorderedDevider '&'


{-| Infix shortcut for a function which creates an unordered parsing tree from subnodes provided as arguments which are separated by *.
-}
(<*>): URL -> URL -> URL
(<*>) = unorderedDevider '*'


{-| Infix shortcut for a function which creates an unordered parsing tree from subnodes provided as arguments which are separated by %.
-}
(<%>): URL -> URL -> URL
(<%>) = unorderedDevider '%'


{-| Creates an ordered parsing tree from subnodes provided as arguments which are separated by specified Char.

It can be used for the creation of nice infix shortcuts.

    (</>) = orderedDevider '/'
-}
orderedDevider: Char -> URL -> URL -> URL
orderedDevider char url1 url2 =
    OrderedURL char url1 url2


{-| Creates an unordered parsing tree from subnodes provided as arguments which are separated by specified Char.

It can be used for the creation of nice infix shortcuts.

    (<&>) = unorderedDevider '&'
-}
unorderedDevider: Char -> URL -> URL -> URL
unorderedDevider char url1 url2 =
    case url1 of
        OrderedURL _ a _ ->
            merge ( (::) url1 ) char url1 url2

        UnorderedURL char1 urls1 ->
            if char1 == char then 
                merge ( List.append urls1 ) char url1 url2
            else 
                merge ( (::) url1 ) char url1 url2

        NodeURL a ->
            merge ( (::) url1 ) char url1 url2

-- HELPER FUNCTION

{-| Iterates over parsing tree and tries to parse it part by part.
-}
parsingLoop : URL -> (List ParsingResult) -> String -> Maybe Char -> Result (String) ( List ParsingResult, String )
parsingLoop url result string tailChar =
    case url of
        OrderedURL char suburl nextURL ->
            case parsingLoop suburl result string (Just char) of
                Ok (newResult, newString) ->
                    parsingLoop nextURL newResult newString tailChar

                a -> a

        NodeURL node ->
            case tailChar of
                Just char ->
                    string
                        |> break char
                        |> Result.andThen (parseNode result node)
                
                Nothing ->
                    parseNode result node (string, "")

        UnorderedURL char urls ->
            urls
                |> zipIndex
                |> parseUnordered char tailChar string [] [] []


{-| Iterate over unordered part of parsing tree and tries to find any combination which produces meaningful output.
-}
parseUnordered
    : Char
    -> Maybe Char 
    -> String 
    -> List (Int, URL) 
    -> List (Int, List ParsingResult) 
    -> List (Int, List ParsingResult)  
    -> List (Int, URL) 
    -> Result String (List ParsingResult, String)
parseUnordered char tailChar string prevUrls result curResult urls =
    case urls of 
        [] ->
            let
                newResult = (List.append curResult result)
            in
                case prevUrls of
                    [] ->
                        newResult
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> flattenUtilFailure []
                            |> Result.map (\ v -> ( v, string) )

                    head :: tail ->
                        if 0 < List.length curResult then 
                            parseUnordered char tailChar string [] newResult [] prevUrls
                        else
                            let
                                template = prevUrls
                                    |> List.sortBy Tuple.first
                                    |> List.map (Tuple.second >> toString)
                                    |> String.join " or "
                            in
                               Err <| "Start of " ++ string ++ " does not have any value which can be correctly parsed by " ++ template ++ ", separated by " ++ fromChar char ++ "."
                    
        (i, url) :: restOfUrls ->
            let 
                newTailChar = 
                    if restOfUrls == [] && prevUrls == [] then 
                        tailChar 
                    else 
                        Just char
            in
                case parsingLoop url [] string newTailChar of
                    Ok (newResult, restOfString) ->
                        parseUnordered char tailChar restOfString prevUrls result ((i, newResult) :: curResult) restOfUrls
                    
                    Err _ ->
                        parseUnordered char tailChar string ((i, url) :: prevUrls) result curResult restOfUrls

{-| Performs flattening of the nested ParsingResult list.
Processing is aborted in case of the first Failure which is reported as Err.
-}
flattenUtilFailure: List ParsingResult -> List( List ParsingResult ) -> Result String (List ParsingResult)
flattenUtilFailure accum result =
    case result of 
        [] ->
            Ok accum
        
        head :: tail ->
            case findFailure head of
                Just err ->
                    Err err

                Nothing ->
                    flattenUtilFailure (List.append head accum) tail


{-| Iterates over a list of URL values and tries to find any Failure.
-}
findFailure: List ParsingResult -> Maybe String
findFailure result =
    case result of 
        [] ->
            Nothing
        
        (Failure err) :: tail ->
            Just err

        _ :: tail ->
            findFailure tail 


{-| Generate List of indeces for a given array.
-}
indices : List a -> List Int
indices list =
    List.range 1 (List.length list)


{-| Zip List with a List of its indeces.
-}
zipIndex: List a -> List ( Int, a )
zipIndex list =
    list |>
      List.map2 (,) (indices list)


{-| Parse a single parsing node.
-}
parseNode
    : List ParsingResult
    -> ParseNode
    -> ( String, a )
    -> Result String ( List ParsingResult, a )
parseNode result node strings =
    case node of
        ParsePath path ->
            checkEqual path strings
                |> Result.map ((,) result)

        ParseFloat ->
            parseValue String.toFloat strings
                |> packValue Floating result
        
        ParseInt ->
            parseValue String.toInt strings
                |> packValue Integer result
                        
        ParseStr ->
            parseValue Ok strings
                |> packValue Str result

        ParseAny ->
            Ok (result, Tuple.second strings)

        ParseQuery ->
            parseValue parseQuery strings
                |> packValue Query result

{-| Apply parser to the first and pack result to the second.
-}
parseValue : (a -> Result x b) -> ( a, c ) -> Result x ( b, c )
parseValue parse (head, tail) =
    parse head
        |> Result.map ( \ value -> ( value, tail ))


{-| Parse a URL query.
-}
parseQuery : String -> Result String (Dict String String)
parseQuery string =
    let 
        (oks, errs) =
            string
                |> String.split "&"
                |> List.map (break '=')
                |> partitionLift ( [], [] )
    in
        if (List.length errs) > 0 then
            errs
                |> String.concat
                |> String.append "Query is not correct: "
                |> Err
        else
            oks
                |> Dict.fromList
                |> Ok


{-| Iterate over a List of values and separate them into Tuple of Lists in a way that first is Oks and second is  Errs.
-}
partitionLift: ( List a, List b ) -> List (Result b a) -> ( List a, List b )
partitionLift (succes, failure) list =
    case list of
        [] ->
            ( succes, failure )

        (Ok head) :: tail ->
            partitionLift ( head :: succes, failure ) tail

        (Err head) :: tail ->
            partitionLift ( succes, head :: failure ) tail

{-| Pack supplied value by the packer and add it to existing result.
-}
packValue: (a -> b) -> List b -> Result c ( a, d ) -> Result c ( List b, d )
packValue packer result input =
    case input of
        Ok ( value, tail ) ->
            ( packer value :: result, tail)
                |> Ok
        
        Err error ->
            Err error


{-| Return second if the string is equal to the first value of the Tuple.
-}
checkEqual : String -> ( String, value ) -> Result String value
checkEqual path (string, tail) =
    if path == string then
        Ok tail
    else 
        Err ( path ++ " is not " ++ string)
            

{-| Prepare URLValues for an output.
-}
makeValue: (List ParsingResult) -> ParsingResult
makeValue list =
    case list of
        head :: [] ->
            head 

        head :: tail ->
            MultiValue <| reverse list 
        
        [] ->
            Success


{-| Merge unordered parsing trees in according to append function.
-}
merge: (List URL -> List URL) -> Char -> a -> URL -> URL
merge append char url1 url2 =
    UnorderedURL char <| 
        append <| 
            case url2 of
                UnorderedURL char2 urls2 ->
                    if char == char2 then
                        urls2 
                    else
                        [url2]
                
                _ ->
                    [url2] 
