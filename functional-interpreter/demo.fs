// simple demo


namespace functional_interpreter

open interpreter
open programs

module demo =

    [<EntryPoint>]
    let main argv = 
        let result = evaluate prog8While
        let result_string = result.ToString()

        printfn "%s" result_string

        0