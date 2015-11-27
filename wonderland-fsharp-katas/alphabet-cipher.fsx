// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

let matrix =
    let az = ['a'..'z']
    let ln = az.Length
    Array2D.init ln ln
        (fun a b -> az.[(a + b) % ln])

let (!) (c: char) = int (System.Char.ToLower c) - 97

let encode (key:Keyword) (message:Message) : Message =
    Array.init message.Length (fun i -> key.[i % key.Length])
    |> Array.mapi (fun i c -> matrix.[!c, !message.[i]])
    |> System.String

let decode (key:Keyword) (message:Message) : Message =
    Array.init message.Length (fun i -> key.[i % key.Length])
    |> Array.mapi (fun i c ->
        let index1 = !message.[i]
        let index2 = let ln = Array2D.length2 matrix in (ln - !c) % ln
        matrix.[index1, index2])
    |> System.String

module DuplicateFinder =
    let private finder (input: string) =
        let rec loop i c =
            let value = input.[0..i]
            let parsed = input.Split([| value |], System.StringSplitOptions.None)
            let stringFound = parsed |> Array.forall ((=) "")
            if i = input.Length || not stringFound
            then loop (i + 1) (parsed.Length+c)
            else value, c
        loop 0 0

    let find (input: string) =
        let word, max =
            input
            |> Seq.mapi (fun i _ -> finder input.[0..i])
            |> Seq.countBy fst
            |> Seq.maxBy snd
        // if max is 1 then the input was to
        // small to recognize any repetitions
        if max = 1
        then None
        else Some word

let decipher (cipher:Message) (message:Message) : Keyword =
    cipher.ToCharArray()
    |> Array.mapi (fun i c ->
        let ln = Array2D.length2 matrix
        let index1 = let ln = Array2D.length1 matrix in (ln - !message.[i]) % ln
        let index2 = !c
        matrix.[index1, index2])
    |> System.String
    |> DuplicateFinder.find
    |> fun s -> s |> defaultArg <| ""

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
