// Sudoku solver for traditional sudokus
// as well as variants with non-square subgroups (a X b)
// Can also include a sun-rule.
// # Sun rule
// If a field is defined as a sun, it sends rays in all
// 4 straight and all 4 diagonal directions.
// The rays loose their intensity as they travel away
// from the sun, and the fields that they pass through
// need to take strictly lower values accordingly.
// The center of the sun is not part of any ray.

type field = int
type board = field list list
type direction = Forw | Backw | Horiz | Vert

let n = 8               // Size of board
let (a,b) = (4,2)       // the width and height of subgroups (3x3 for normal sudoku)
let sun = Some (3, 3)   // Position of sun. Set to None if no sun-rule
let input = 
    [
        [0; 0; 0; 0;  0; 0; 0; 6]
        [0; 0; 6; 0;  0; 0; 0; 2]

        [0; 0; 0; 0;  0; 0; 0; 0]
        [0; 0; 0; 0;  0; 0; 0; 0]

        [0; 0; 0; 0;  0; 0; 1; 3]
        [0; 3; 0; 0;  0; 0; 0; 0]

        [0; 0; 0; 0;  0; 0; 0; 0]
        [0; 0; 0; 0;  0; 0; 0; 0]
    ]

let findPotentials (inp:board) =



    let w = List.length inp[0]
    let h = List.length inp

    let hGroups = inp

    let vGroups : int list list = 
            [for i in 0..h-1 ->
                [for j in 0..w-1 ->
                    (inp[j][i])] ]// |> List.filter (fun elm -> elm <> 0)]
            

    let subGroups = 
        [for i in 0..h-1 -> 
            [for j in 0..w-1 -> 
                inp[j/a+i/b*b][j%a+a*(i%b)]] |> List.filter (fun elm -> elm <> 0)]

    let indexer ((y,x):int*int) (n:int) (t:direction) =
        if t = Forw then
            let row = x+y
            let bend = max 0 (row - (n-1))
            let col = row - x - bend
            (row,col)
        else if t = Backw then
            let col = min x y
            let row = (n-1)-(x-y)
            (row,col)
        else if t = Horiz then
            let col = x
            let row = y
            (row,col)
        else
            let col = y
            let row = x
            (row,col)

    let getDiagonals (b:board) =
        let diagLength = 2*n-1
        [for row in 0..diagLength - 1 ->
            [for col in 0..(n - 1) ->
                if (row < col) || (col <= row-n) then None else Some (b[col][row - col]) ] |>
                    List.filter (fun x -> not (x = None)) |> 
                    List.map (fun x -> Option.get x) ] 
    let fsGroups = getDiagonals inp
    let bsGroups =
        inp |>
        List.map (fun sub -> List.rev sub) |>
        getDiagonals


    // Done with groups. 
    let fil (filter:int list) (input:int list) : int list =
        input |>
        List.filter (fun x -> not (List.exists (fun y -> y = x) filter))

    let filSun (row:int) (col:int) : int list = //spits out the numbers to filter against

        let rayCheck (theRow:int list) (col:int) (sCol:int) =
            // ALL COORDINATES HERE ARE TRANSPOSED
            if col < sCol then //left side (but reversed, so actually right)
                let minN: int = theRow[col+1..sCol-1]
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s -> List.min (n-(sCol-col)+2::s))
                let maxN:int = theRow[0..col-1]
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s -> List.max (col::s))

                [0..maxN]@[minN..n]
            else if col > sCol then
                let minN: int = theRow[sCol+1..col-1]
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s-> List.min (n-(col-sCol)+2::s))
                let edge = List.length theRow
                let maxN:int = theRow[col+1..]
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s-> List.max (edge-col-1::s))
                [0..maxN]@[minN..n]
            else []



        let (sRow, sCol) = Option.get sun
        let (bsRow,bsCol) = indexer (row,col) n Backw
        let (sBsRow,sBsCol) = indexer (sRow,sCol) n Backw

        if row = sRow && col <> sCol then
            rayCheck inp[row] col sCol 
        else if col = sCol && row <> sRow then
            rayCheck vGroups[col] row sRow

        // fs diagonal
        else if col+row = sCol+sRow then
            let (diagRow,diagCol) = indexer (row,col) n Forw
            let (sDiagRow,sDiagCol) = indexer (sRow,sCol) n Forw
            rayCheck fsGroups[diagRow] diagCol sDiagCol

        else if bsRow = sBsRow && bsCol <> sBsCol then
            rayCheck bsGroups[bsRow] bsCol sBsCol
        else []


    let filters (row:int) (col:int) : int list =
        let normalSudokuRules = [1..n] |>
                                fil hGroups[row] |>
                                fil vGroups[col] |>
                                fil subGroups[col/a+(row/b)*b]
        match sun with
            None -> normalSudokuRules
            | Some coord ->
                normalSudokuRules |> fil (filSun row col)

    let rec getEmptyFields board =
        let h, w = List.length board, List.length board.[0]
        let rec helper row col =
            if row = h then []
            else if col = w then helper (row + 1) 0
            else if board.[row].[col] = 0 then
                let possibleValues = filters row col
                ((row, col), possibleValues) :: helper row (col + 1)
            else helper row (col + 1)
        helper 0 0
    //filSun 4 6 
    getEmptyFields inp |> List.sortBy (fun (_,poss) -> List.length poss)
let printBoard board =
    for i in board do
        printfn "%A" i

let mutable solNo = 0
let rec solver (brd: int list list) =
    let poss = findPotentials brd
    if List.length poss = 0 then
        solNo <- solNo + 1
        printfn "SOLUTION %A" solNo
        printBoard brd
        printfn ""
    else
        let ((x,y),p) = poss[0]
        for number in p do
            let thisBoard : int list list= 
                List.mapi (fun i (lst:int list) -> 
                    if i = x then
                        lst.[0..y - 1] @ [number] @ lst.[y + 1..]
                    else lst) (brd:int list list)
            solver thisBoard

do solver input


