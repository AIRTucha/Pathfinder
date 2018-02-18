# Pathfinder
[![Build Status](https://travis-ci.org/AIRTucha/pathfinder.svg?branch=master)](https://travis-ci.org/AIRTucha/pathfinder)

An embedded DSL for a typesafe parsing of URL. It is designed for processing URLs of any complexity. 

The library can parse single value, multiple values with the strict or arbitrary order and queries. It also allows writing typesafe parser for a query.

The library contains two main parts:

* A DSL for building parsing tree
* Function which performs parsing of a string in according to provided parsing tree

# Examples

Parsing of a string form a simple path.
```elm
parseName: String -> Maybe String
parseName url =
    case parse (p "someUrl" </> str) url of
        Str name ->
            Just name
        
        _ ->
            Nothing


parseName "someUrl/userName"  -- Just "userName"
```
Parsing of an integer from a path which contains a query.
```elm
parseQuery: String -> Maybe Int
parseQuery url =
    case parse (p "someUrl" <?> (p "age" <=> int)) url of
        Integer age ->
            Just age
        
        _ ->
            Nothing


parseQuery "someUrl?age=10"  -- Just 10
```
Parsing of multiple values.
```elm
parseNameAndId: String -> Just ( String, Int )
parseNameAndId url = 
    case parse (p "someUrl" </> str </> int </> any ) url of
        MultiValue [ Str name, Integer id] ->
                    Just (name, id)
                
        _ ->
            Nothing


parseNameAndId "someUrl/userName/1/someRandomStuff"  -- Just ("userName", 1)
```
Error handling in case of unsuccesful parsing.
```elm
getDevider: String -> Result String Float
getDevider url =
    case (parse any </> float) url of
        Floating value->
            Ok value
            
        Failure err ->
            Err err
        
        _ ->
            Err "Unexpected path."


getDevider "10*3.1415"  -- Err "10*3.1415 does not contain /"
```

# Contributing

Feedback and contributions are very welcome.

For installation of local npm dependencies run:
```
npm install
```

For installation of local elm dependencies run:
```
npm run install
```

Repeat the same actions for tests folder.

Run tests from root folder by:
```
npm test
```

Any Elm CLI commands can be called by:
```
npm run elm [comands]
```
