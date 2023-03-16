module Morphir.Elm.Generator.API exposing
    ( Generator
    , Seed
    , alphaChar
    , andThen
    , anyChar
    , bool
    , combine
    , constant
    , int
    , intRange
    , list
    , map
    , map2
    , maybe
    , next
    , nextN
    , niceFloat
    , oneOf
    , seed
    , string
    )

{-| A Library for random data generation.

There are two main approaches to generating random numbers: _True Random Number Generators (TRNGs)_ and _Pseudo-Random Number Generators (PRNGs)_.
TRNGs generate numbers from truly random physical phenomena, for example the little variations in someone’s mouse movements.
Due to their reliance on a physical phenomena, TRNGs take considerably longer time to generate random numbers.
Therefore, most computer programs that need random numbers quickly tend to rely on PRNGs which are much more efficient than TRNGs.
Despite their efficiency, PRNGs are not suitable for applications that need truly unpredictable random numbers, such as encryption key generators.
PRNGs take an initial value (called seed) and apply an algorithm to generate a seemingly random number.
The modern algorithms used by PRNGs are so good that the numbers generated by them look quite random.

-}

import Random
import Random.Char as Random
import Random.Extra as Random
import Random.String as Random


{-| A `Generator a` represents a generator that knows how to generate random value of a certain type `a`.
-}
type Generator a
    = Generator (Random.Generator a)


{-| Sometimes it is useful to be able to reproduce the sequences given by a pseudo-random number generator.
By re-using a seed value, the same sequence should be reproducible from run to run, and in fact, it is a requirement in functional programming that function outputs are predictable.

Seeds are how we ensure reproducible randomness. Our generator takes an initial value (seed) and apply an algorithm to generate a seemingly random values, which means that providing the same seed multiple times will produce the same
sequence of randomness over and over again. This is useful when we want to reproduce testing a function with some input that causes our logic to fail.

-}
type alias Seed =
    Random.Seed


fromRandom : Random.Generator a -> Generator a
fromRandom =
    Generator


{-| Create a seed from an integer
-}
seed : Int -> Seed
seed =
    Random.initialSeed


{-| Produce a generator that always generates the same value specified value.
-}
constant : a -> Generator a
constant =
    fromRandom << Random.constant


{-| A generator that generates True or False.
-}
bool : Generator Bool
bool =
    fromRandom <| Random.uniform True [ False ]


{-| A generator that generates a single random **ASCII** character.
-}
anyChar : Generator Char
anyChar =
    fromRandom <| Random.ascii


{-| A generator that generates a single random character from the english alphabet.
-}
alphaChar : Generator Char
alphaChar =
    fromRandom <| Random.english


{-| A generator that generates a random integer value.
-}
int : Generator Int
int =
    fromRandom <| Random.int Random.minInt Random.maxInt


intRange : Int -> Int -> Generator Int
intRange from to =
    fromRandom <| Random.int from to


niceFloat : Generator Float
niceFloat =
    fromRandom <|
        Random.float
            (toFloat Random.minInt)
            (toFloat Random.maxInt)


{-| Generates a string of 3 to 10 characters.
-}
string : Generator String
string =
    fromRandom <|
        Random.rangeLengthString 3 10 Random.english


maybe : Generator a -> Generator (Maybe a)
maybe =
    useRandom
        (Random.andThen
            (\a ->
                Random.uniform
                    (Just a)
                    [ Nothing ]
            )
        )


{-| Generate a list of variable length 1 - 100.
-}
list : Generator a -> Generator (List a)
list =
    useRandom
        (\internalGenA ->
            Random.int 1 100
                |> Random.andThen
                    (\i ->
                        Random.list i internalGenA
                    )
        )


oneOf : a -> List a -> Generator a
oneOf default ls =
    fromRandom <| Random.uniform default ls


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen fn genA =
    toRandom genA
        |> Random.andThen (fn >> toRandom)
        |> fromRandom


map : (a -> b) -> Generator a -> Generator b
map fn =
    toRandom
        >> Random.map fn
        >> fromRandom


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 fn genA genB =
    Random.map2 fn
        (toRandom genA)
        (toRandom genB)
        |> fromRandom


toRandom : Generator a -> Random.Generator a
toRandom generator =
    case generator of
        Generator rg ->
            rg


useRandom : (Random.Generator a -> Random.Generator b) -> Generator a -> Generator b
useRandom use =
    toRandom
        >> use
        >> fromRandom


{-| Combine a list of generators into one single generator of `List a`.
-}
combine : List (Generator a) -> Generator (List a)
combine generators =
    generators
        |> List.map toRandom
        |> Random.combine
        |> fromRandom


{-| Generate a random value using:

  - Seed - a seed
  - Generator a - a value generator

-}
next : Seed -> Generator a -> ( a, Seed )
next sd gen =
    Random.step
        (toRandom gen)
        sd


{-| Generate a mulitple random values.
If a negative size is provided, then no data will be generated.
The params are:

  - Int - The size of the data to generate
  - Seed - a seed to start the generation
  - Generator a - a value generator

-}
nextN : Int -> Seed -> Generator a -> List a
nextN n sd gen =
    nextNHelper n sd gen []


nextNHelper : Int -> Seed -> Generator a -> List a -> List a
nextNHelper n sd genA generatedSoFar =
    if n < 0 && List.isEmpty generatedSoFar then
        -- invalid negative starting value for n.
        -- default n to 0
        nextNHelper 0 sd genA generatedSoFar

    else if n == 0 then
        generatedSoFar

    else
        let
            ( val, nextSeed ) =
                Random.step (toRandom genA) sd
        in
        nextNHelper (n - 1) nextSeed genA (val :: generatedSoFar)
