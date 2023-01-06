namespace AmountToFullText

type Error =
    | TooMuchDecimalPlaces
    | CharIsNotDigit
    | AmountToBig

module internal Amount =

    module Domain =
        type Language =
            | Dutch
    
        type Currency =
            | Euro
    
        type RealNumber =
            | One
            | Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
    
        module RealNumber =
            let tryCreate (x:char) =
                match x with
                | '1' -> Ok RealNumber.One
                | '2' -> Ok RealNumber.Two 
                | '3' -> Ok RealNumber.Three 
                | '4' -> Ok RealNumber.Four
                | '5' -> Ok RealNumber.Five 
                | '6' -> Ok RealNumber.Six 
                | '7' -> Ok RealNumber.Seven 
                | '8' -> Ok RealNumber.Eight 
                | '9' -> Ok RealNumber.Nine 
                | _ -> Error Error.CharIsNotDigit
    
        type Number =
            | Zero
            | Real of RealNumber
    
        type Jargon =
            | WordForConcatText of Language
            | WordForCurrencyDescription of Language * Currency
            | WordForCurrencyCentsDescription of Language * Currency
            | WordForZero of Language
            | WordForSingles of Language * Number
            | WordForSinglesButSupressOne of Language * Number
            | WordForTens of Language * Number
            | WordForEleven of Language
            | WordForTwelve of Language
            | WordForThirteen of Language
            | WordForFourteen of Language
            | WordForHundred of Language
            | WordForThousand of Language
            | WordForMillion of Language
            member jargon.Word =
                match jargon with
     
                | WordForConcatText language ->
                    match language with
                    | Dutch -> "en"
    
                | WordForCurrencyDescription (language, currency) ->
                    match language with
                    | Dutch -> 
                        match currency with
                        | Euro -> "euro"
    
                | WordForCurrencyCentsDescription (language, currency) ->
                    match language with
                    | Dutch -> 
                        match currency with
                        | Euro -> "eurocent"
    
                | WordForMillion (language:Language) ->
                    match language with
                    | Dutch ->  "miljoen"
    
                | WordForThousand (language:Language) ->
                    match language with
                    | Dutch ->  "duizend"
    
                | WordForHundred (language:Language) ->
                    match language with
                    | Dutch ->  "honderd"
    
                | WordForZero (language:Language) ->
                    match language with
                    | Dutch ->  "nul"
    
                | WordForSingles (language, number) ->
                    match language with
                    | Dutch ->
                        match number with
                        | Zero -> ""
                        | Real realNumber ->
                            match realNumber with
                            | One -> "een"
                            | Two -> "twee"
                            | Three -> "drie"
                            | Four -> "vier"
                            | Five -> "vijf"
                            | Six -> "zes"
                            | Seven -> "zeven"
                            | Eight -> "acht"
                            | Nine -> "negen"
    
                | WordForSinglesButSupressOne (language, number) ->
                    match language with
                    | Dutch ->
                        match number with
                        | Zero -> ""
                        | Real realNumber ->
                            match realNumber with
                            | One -> ""
                            | _ -> (Jargon.WordForSingles (language,number)).Word
    
                | WordForTens (language, number) ->
                    match language with
                    | Dutch ->
                        match number with
                        | Zero -> ""
                        | Real realNumber ->
                            match realNumber with
                            | One -> "tien"
                            | Two -> "twintig"
                            | Three -> "dertig"
                            | Four -> "veertig"
                            | Five -> "vijftig"
                            | Six -> "zestig"
                            | Seven -> "zeventig"
                            | Eight -> "tachtig"
                            | Nine -> "negentig"
    
                | WordForEleven (language:Language) -> 
                    match language with
                    | Dutch -> "elf"
    
                | WordForTwelve (language:Language) -> 
                    match language with
                    | Dutch -> "twaalf"
    
                | WordForThirteen (language:Language) -> 
                    match language with
                    | Dutch -> "dertien"
    
                | WordForFourteen (language:Language) -> 
                    match language with
                    | Dutch -> "veertien"
    
    
    
        module Number =
            let tryCreate (x:char) =
                match x with
                | '0' -> Number.Zero |> Ok
                | _ -> x |> RealNumber.tryCreate |> Result.map (fun number -> Number.Real number)
    
            let tryCreateFromTuple (xs:char * char) =
                let a, b = xs
                let resultA = a |> tryCreate
                let resultB = b |> tryCreate
                match resultA, resultB with
                | Error error, _
                | _, Error error -> error |> Error
                | Ok aOk, Ok bOk -> (aOk, bOk) |> Ok
    
        type Decimals =
            | Decimals of Number * Number
    
        module Decimals =
            let tryCreate charsDecimals =
                match charsDecimals with
                | [] -> Decimals (Number.Zero, Number.Zero) |> Ok
                | [a] -> a |> Number.tryCreate |> Result.map (fun number ->  Decimals (number, Number.Zero))
                | [a;b] -> (a,b) |> Number.tryCreateFromTuple |> Result.map (fun (aNumber, bNumber) ->  Decimals (aNumber, bNumber))
                | _ -> Error.TooMuchDecimalPlaces |> Error
    
        type AmountType =
            | Ones of Language * Currency * Number * Decimals
            | Tens of Language * Currency * RealNumber * Number * Decimals
            | Hundreds of Language * Currency * RealNumber * Number * Number * Decimals
            | Thousands of Language * Currency * RealNumber * Number * Number * Number * Decimals
            | TenThousands of Language * Currency * RealNumber * Number * Number * Number * Number * Decimals
            | HundredThousands of Language * Currency * RealNumber * Number * Number * Number * Number * Number * Decimals
            | Millions of Language * Currency * RealNumber * Number * Number * Number * Number * Number * Number * Decimals
    
            member amountType.Language =
                match amountType with
                | Ones (language, _, _, _) -> language
                | Tens (language, _, _, _, _) -> language
                | Hundreds (language, _, _, _, _, _) -> language
                | Thousands (language, _, _, _, _, _, _) -> language
                | TenThousands (language, _, _, _, _, _, _, _) -> language
                | HundredThousands (language, _, _, _, _, _, _, _, _) -> language
                | Millions (language, _, _, _, _, _, _, _, _, _) -> language
    
            member amountType.Currency =
                match amountType with
                | Ones (_, currency, _, _) -> currency
                | Tens (_, currency, _, _, _) -> currency
                | Hundreds (_, currency, _, _, _, _) -> currency
                | Thousands (_, currency, _, _, _, _, _) -> currency
                | TenThousands (_, currency, _, _, _, _, _, _) -> currency
                | HundredThousands (_, currency, _, _, _, _, _, _, _) -> currency
                | Millions (_, currency, _, _, _, _, _, _, _, _) -> currency
    
            member amountType.CurrencyDescription =
                (Jargon.WordForCurrencyDescription (amountType.Language, amountType.Currency)).Word
    
            member amountType.Decimals =
                match amountType with
                | Ones (_, _, _, decimals) -> decimals
                | Tens (_, _, _, _, decimals) -> decimals
                | Hundreds (_, _, _, _, _, decimals) -> decimals
                | Thousands (_, _, _, _, _, _, decimals) -> decimals
                | TenThousands (_, _, _, _, _, _, _,decimals) -> decimals
                | HundredThousands (_, _, _, _, _, _, _, _, decimals) -> decimals
                | Millions (_, _, _, _, _, _, _, _, _, decimals) -> decimals
    
            member amountType.StringForNumberOfTensWithinLargerNumber (x,y)  =
                let defaultString (a,b)  =
                    let stringOnes = (Jargon.WordForSingles (amountType.Language,b)).Word
                    let stringTens = (Jargon.WordForTens (amountType.Language,a)).Word
                    let textConcat = (Jargon.WordForConcatText amountType.Language).Word
                    $"%s{stringOnes}%s{textConcat}%s{stringTens}"
    
                match amountType.Language with
                | Dutch ->
                    match (x,y) with
                    | (Number.Zero,Number.Zero) -> ""
                    | (Number.Zero,b) ->
                        (Jargon.WordForSingles (amountType.Language,b)).Word
                    | (a,Number.Zero) ->
                        (Jargon.WordForTens (amountType.Language,a)).Word
                    | (Number.Real RealNumber.One,Number.Real RealNumber.One) ->
                        (Jargon.WordForEleven amountType.Language).Word
                    | (Number.Real RealNumber.One,Number.Real RealNumber.Two) ->
                        (Jargon.WordForTwelve amountType.Language).Word
                    | (Number.Real RealNumber.One,Number.Real RealNumber.Three) ->
                        (Jargon.WordForThirteen amountType.Language).Word
                    | (Number.Real RealNumber.One,Number.Real RealNumber.Four) ->
                        (Jargon.WordForFourteen amountType.Language).Word
                    | (a,b) -> (a,b) |> defaultString
    
            member amountType.StringForNumberOfMillions x =
                let defaultString a  =
                    let stringNumberOfMillions = (Jargon.WordForSingles (amountType.Language,a)).Word
                    let stringMillions = (Jargon.WordForMillion amountType.Language).Word
                    $"%s{stringNumberOfMillions} %s{stringMillions}"
    
                match amountType.Language with
                | Dutch -> x |> defaultString
    
            member amountType.StringForNumberOfThousandsWithinLargerNumber (x,y,z) =
                let defaultString (a,b,c) =
                    let stringNumberOfHundreds = (Jargon.WordForSingles (amountType.Language,a)).Word
                        
                    let stringNumberofTens = (b,c) |> amountType.StringForNumberOfTensWithinLargerNumber
                    $"%s{stringNumberOfHundreds}%s{((Jargon.WordForHundred amountType.Language).Word)}%s{stringNumberofTens}%s{((Jargon.WordForThousand amountType.Language).Word)}"
    
                match amountType.Language with
                | Dutch ->
                    match (x,y,z) with
                    | (Number.Zero,Number.Zero,c) ->
                        let stringNumberOfOnes = (Jargon.WordForSingles (amountType.Language,c)).Word
                        $"%s{stringNumberOfOnes}"
    
                    | (Number.Zero,b,c)  ->
                        let stringNumberOfTens = (b,c) |> amountType.StringForNumberOfTensWithinLargerNumber
                        $"%s{stringNumberOfTens}%s{((Jargon.WordForThousand amountType.Language).Word)}"
    
                    | (Number.Real RealNumber.One, b, c) ->
                        let stringNumberofTens = (b,c) |> amountType.StringForNumberOfTensWithinLargerNumber
                        $"%s{((Jargon.WordForHundred amountType.Language).Word)}%s{stringNumberofTens}%s{((Jargon.WordForThousand amountType.Language).Word)}"
    
                    | (a,b,c) ->
                        (a,b,c) |> defaultString
    
            member amountType.StringForNumberOfHundredsWithinLargerNumber (x,y,z) =
                let defaultString (a,b,c) =
                    let stringNumberOfHundreds = (Jargon.WordForSingles (amountType.Language,a)).Word
                    let stringNumberofTens =
                        (b,c)
                        |> amountType.StringForNumberOfTensWithinLargerNumber
    
                    $"%s{stringNumberOfHundreds}%s{((Jargon.WordForHundred amountType.Language).Word)}%s{stringNumberofTens}"
    
                match amountType.Language with
                | Dutch ->
                    match (x,y,z) with
                    | (Number.Zero,Number.Zero,c) ->
                        let stringNumberOfOnes = (Jargon.WordForSingles (amountType.Language,c)).Word
                        $"%s{stringNumberOfOnes}"
    
                    | (Number.Zero,b,c) ->
                        let stringNumberofTens = (b,c)  |> amountType.StringForNumberOfTensWithinLargerNumber
                        $"%s{stringNumberofTens}"
    
                    | (Number.Real RealNumber.One,Number.Zero,Number.Zero) ->
                        $"%s{((Jargon.WordForHundred amountType.Language).Word)}"
    
                    | (a,b,c) -> (a,b,c) |> defaultString
    
            member amountType.Cents =
                let (x,y) = match amountType.Decimals with | (Decimals (x,y)) -> (x,y)
                let textCents = (Jargon.WordForCurrencyCentsDescription (amountType.Language, amountType.Currency)).Word
    
                let addConcatText =
                    match amountType with
                    | Ones (_, _,number,_) when number = Number.Zero -> ""
                    | _ -> (Jargon.WordForConcatText amountType.Language).Word
    
                let addTextCents stringNumber = 
                    $"%s{addConcatText} %s{stringNumber} %s{textCents}"
    
                match (x,y) with
                | (Number.Zero,Number.Zero) -> ""
                | (a,Number.Zero) -> (Jargon.WordForTens (amountType.Language,a)).Word |> addTextCents
                | (a,b) -> (a,b) |> amountType.StringForNumberOfTensWithinLargerNumber |> addTextCents
    
            member amountType.ToFullText () =
            
                    let add stringInput stringSource =
                        match stringInput with
                        | "" -> stringSource
                        | s -> stringSource + " " + s
            
                    let addCurrency = add amountType.CurrencyDescription
                    let addCents = add amountType.Cents
            
                    match amountType with
                    | Ones (_, currency, numberA, decimals) -> 
                        
                        match numberA, amountType.Decimals with
                        | Number.Zero, Decimals (Number.Zero,Number.Zero) ->
                            let isZero = (Jargon.WordForZero amountType.Language).Word
            
                            isZero
                            |> addCurrency
            
                        | Number.Zero, _ ->
                            amountType.Cents
            
                        | _,_ ->
            
                            let partOnes = (Jargon.WordForSingles (amountType.Language,numberA)).Word
                                
            
                            partOnes
                            |> addCurrency
                            |> addCents
            
                    | Tens (_, currency, realNumber, numberB, decimals) ->
                        
                        let partTens =
                            (Number.Real realNumber, numberB)
                            |> amountType.StringForNumberOfTensWithinLargerNumber
            
                        partTens
                        |> addCurrency
                        |> addCents
            
                    | Hundreds (_, currency, realNumber, numberB, numberC, decimals) ->
            
                        let partHundreds =
                            (Number.Real realNumber, numberB, numberC)
                            |> amountType.StringForNumberOfHundredsWithinLargerNumber
            
                        partHundreds
                        |> addCurrency
                        |> addCents
            
                    | Thousands (_, currency, realNumber, numberB, numberC, numberD, decimals) -> 
                        let stringThousands = (Jargon.WordForThousand amountType.Language).Word
            
                        let partThousands =
                            let stringNumber =
                                match amountType.Language with
                                | Dutch ->
                                    match realNumber with
                                    | RealNumber.One -> ""
                                    | _ -> (Jargon.WordForSingles (amountType.Language,(Number.Real realNumber))).Word
                    
                            $"%s{stringNumber}%s{stringThousands}"
                    
                        let partHundreds =
                            (numberB, numberC, numberD)
                            |> amountType.StringForNumberOfHundredsWithinLargerNumber
            
                        partThousands
                        |> add partHundreds
                        |> addCurrency
                        |> addCents
                                
                    | TenThousands (_, currency, realNumber, numberB, numberC, numberD, numberE, decimals) -> 
                        let stringThousands = (Jargon.WordForThousand amountType.Language).Word
            
                        let partTenThousands =
                            let stringNumber =
                                (Number.Real realNumber, numberB)
                                |> amountType.StringForNumberOfTensWithinLargerNumber
                            $"%s{stringNumber}%s{stringThousands}"
            
                        let partHundreds =
                            (numberC, numberD, numberE)
                            |> amountType.StringForNumberOfHundredsWithinLargerNumber
            
                        partTenThousands
                        |> add partHundreds
                        |> addCurrency
                        |> addCents
            
                    | HundredThousands (_, currency, realNumber, numberB, numberC, numberD, numberE, numberF, decimals) ->
                    
                        let partThousands =
                            (Number.Real realNumber, numberB, numberC)
                            |> amountType.StringForNumberOfThousandsWithinLargerNumber
                             
                        let partHundreds =
                            (numberD, numberE, numberF)
                            |> amountType.StringForNumberOfHundredsWithinLargerNumber
            
                        partThousands
                        |> add partHundreds
                        |> addCurrency
                        |> addCents
            
                    | Millions (_, currency, realNumber, numberB, numberC, numberD, numberE, numberF, numberG, decimals) -> 
                    
                        let partMillions = 
                            realNumber
                            |> Number.Real
                            |> amountType.StringForNumberOfMillions
                    
                        let partThousands =
                            (numberB, numberC, numberD)
                            |> amountType.StringForNumberOfThousandsWithinLargerNumber
            
                        let partHundreds =
                            (numberE, numberF, numberG)
                            |> amountType.StringForNumberOfHundredsWithinLargerNumber
            
                        partMillions
                        |> add partThousands
                        |> add partHundreds
                        |> addCurrency
                        |> addCents
    
        module AmountType =
    
            module WholeIntegers =
                let tryCreate language currency decimals charsWholeInteger =
                    match charsWholeInteger with
                    | [] -> 
                        AmountType.Ones (language, currency, Number.Zero, decimals) |> Ok
    
                    | [a] -> 
                        match a |> Number.tryCreate with
                        | Error error -> Error error
                        | Ok aOk -> AmountType.Ones (language, currency, aOk, decimals) |> Ok
    
                    | [a;b] -> 
                        match a |> RealNumber.tryCreate with
                        | Error error -> Error error
                        | Ok realNumber ->
                            match b |> Number.tryCreate with
                            | Error error -> Error error
                            | Ok bOk ->
                                AmountType.Tens (language, currency, realNumber, bOk, decimals) |> Ok
    
                    | [a;b;c] -> 
                        match a |> RealNumber.tryCreate with
                        | Error error -> Error error
                        | Ok realNumber ->
                            match b |> Number.tryCreate with
                            | Error error -> Error error
                            | Ok bOk ->
                                match c |> Number.tryCreate with
                                | Error error -> Error error
                                | Ok cOk ->
                                AmountType.Hundreds (language, currency, realNumber, bOk, cOk, decimals) |> Ok
    
                    | [a;b;c;d] -> 
                        match a |> RealNumber.tryCreate with
                        | Error error -> Error error
                        | Ok realNumber ->
                            match b |> Number.tryCreate with
                            | Error error -> Error error
                            | Ok bOk ->
                                match c |> Number.tryCreate with
                                | Error error -> Error error
                                | Ok cOk ->
                                    match d |> Number.tryCreate with
                                    | Error error -> Error error
                                    | Ok dOk ->
                                        AmountType.Thousands (language, currency, realNumber, bOk, cOk, dOk, decimals) |> Ok
    
                    | [a;b;c;d;e] -> 
                        match a |> RealNumber.tryCreate with
                        | Error error -> Error error
                        | Ok realNumber ->
                            match b |> Number.tryCreate with
                            | Error error -> Error error
                            | Ok bOk ->
                                match c |> Number.tryCreate with
                                | Error error -> Error error
                                | Ok cOk ->
                                    match d |> Number.tryCreate with
                                    | Error error -> Error error
                                    | Ok dOk ->
                                        match e |> Number.tryCreate with
                                        | Error error -> Error error
                                        | Ok eOk ->
                                            AmountType.TenThousands (language, currency, realNumber, bOk, cOk, dOk, eOk, decimals) |> Ok
    
                    | [a;b;c;d;e;f] -> 
                        match a |> RealNumber.tryCreate with
                        | Error error -> Error error
                        | Ok realNumber ->
                            match b |> Number.tryCreate with
                            | Error error -> Error error
                            | Ok bOk ->
                                match c |> Number.tryCreate with
                                | Error error -> Error error
                                | Ok cOk ->
                                    match d |> Number.tryCreate with
                                    | Error error -> Error error
                                    | Ok dOk ->
                                        match e |> Number.tryCreate with
                                        | Error error -> Error error
                                        | Ok eOk ->
                                            match f |> Number.tryCreate with
                                            | Error error -> Error error
                                            | Ok fOk ->
                                            AmountType.HundredThousands (language, currency, realNumber, bOk, cOk, dOk, eOk, fOk, decimals) |> Ok
    
                    | [a;b;c;d;e;f;g] -> 
                        match a |> RealNumber.tryCreate with
                        | Error error -> Error error
                        | Ok realNumber ->
                            match b |> Number.tryCreate with
                            | Error error -> Error error
                            | Ok bOk ->
                                match c |> Number.tryCreate with
                                | Error error -> Error error
                                | Ok cOk ->
                                    match d |> Number.tryCreate with
                                    | Error error -> Error error
                                    | Ok dOk ->
                                        match e |> Number.tryCreate with
                                        | Error error -> Error error
                                        | Ok eOk ->
                                            match f |> Number.tryCreate with
                                            | Error error -> Error error
                                            | Ok fOk ->
                                                match g |> Number.tryCreate with
                                                | Error error -> Error error
                                                | Ok gOk ->
                                                    AmountType.Millions (language, currency, realNumber, bOk, cOk, dOk, eOk, fOk, gOk, decimals) |> Ok
                
                    | _ -> Error Error.AmountToBig
    
            let create (language:Language) (currency:Currency) (amount:float) : Result<AmountType,Error> =
                let splitAmount = (string amount).Split('.')
                let charsWholeInteger = splitAmount.[0].ToCharArray() |> Array.toList
                let charsDecimals = (try splitAmount.[1].ToCharArray() |> Array.toList with | _ -> [])
    
                match charsDecimals |> Decimals.tryCreate with
                | Error error -> error |> Error
                | Ok decimals -> WholeIntegers.tryCreate language currency decimals charsWholeInteger


        module Helper =
            let unsafeSpelledOut (x:Result<AmountType,Error>) = 
                match x with
                | Error fout -> 
                    sprintf "%A" fout
                | Ok s ->
                    sprintf "%s" (s.ToFullText())


            let createDutchEuro x = x |> AmountType.create Language.Dutch Currency.Euro

            let trySpelledOut (x:Result<AmountType,Error>) =
                x |> Result.map (fun amount -> amount.ToFullText())

    open Domain
    open Helper

    let trySpelledOutInDutchForEuro amount =
        amount |> createDutchEuro |> trySpelledOut

    let unsafeSpelledOutInDutchForEuro amount =
        amount |> createDutchEuro |> unsafeSpelledOut

module Language =

    module Dutch =

        [<RequireQualifiedAccessAttribute>]
        module Currency =

            module Euro =

                let trySpelledOut amount =
                    amount |> Amount.trySpelledOutInDutchForEuro

                let unsafeSpelledOut amount =
                    amount |> Amount.trySpelledOutInDutchForEuro

module Omop =
    type FloatMetMaxTweeDecimalen = float

    let zegge (floatMetMaxTweeDecimalen:FloatMetMaxTweeDecimalen) =
        floatMetMaxTweeDecimalen
        |> Language.Dutch.Currency.Euro.unsafeSpelledOut

    let probeerZegge (floatMetMaxTweeDecimalen:FloatMetMaxTweeDecimalen) =
        floatMetMaxTweeDecimalen
        |> Language.Dutch.Currency.Euro.trySpelledOut
