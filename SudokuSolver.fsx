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
                    (inp[j][i])] ]

    let subGroups = 
        [for i in 0..h-1 -> 
            [for j in 0..w-1 -> 
                inp[j/a+b*(i/b)][j%a+a*(i%b)]] |> List.filter (fun elm -> elm <> 0)]
                // ... don't ask.

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
        else if t = Vert then
            let col = y
            let row = x
            (row,col)
        else //if t = Horiz then
            let col = x
            let row = y
            (row,col)

(* Method of getting diagonals:
                      1 N N    1
1 2 3    1 2 3 N N    2 4 N    2 4
4 5 6 -> N 4 5 6 N -> 3 5 7 -> 3 5 7
7 8 9    N N 7 8 9    N 6 8    6 8
                      N N 9    9      Reverse input lists for other diagonal
*)
    let getDiagonals (b:board) =
        let diagLength = 2*n-1
        [for row in 0..diagLength - 1 ->
            [for col in 0..(n - 1) -> 
                if (row < col) || (col <= row-n) then None else Some (b[col][row - col]) ] |>
                    List.filter (fun x -> x <> None) |> 
                    List.map (fun x -> Option.get x) ] 
    let fsGroups = getDiagonals inp
    let bsGroups =
        inp |>
        List.map (fun sub -> List.rev sub) |>
        getDiagonals
    // Done with groups. 

    let filterOut (filter:int list) (input:int list) : int list =
        input |>
        List.filter (fun x -> not (List.exists (fun y -> y = x) filter))

    let filSun (row:int) (col:int) : int list = //spits out the numbers to filter against
        let rayCheck (theRow:int list) (col:int) (sCol:int) =
            // consider | A x B SUN C x D | where x are possible field placements
            if col < sCol then 
                let minN: int = theRow[col+1..sCol-1]   // Looks at section B
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s -> List.min (n-(sCol-col)+2::s))
                let maxN:int = theRow[..col-1]         // Looks at section A
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s -> List.max (col::s))

                [0..maxN]@[minN..n]
            else if col > sCol then
                let minN: int = theRow[sCol+1..col-1]   // Looks at section C
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s-> List.min (n-(col-sCol)+2::s))
                let edge = List.length theRow
                let maxN:int = theRow[col+1..]          // Looks at section D
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s-> List.max (edge-col-1::s))
                [0..maxN]@[minN..n]
            else []

        let (sRow, sCol) = Option.get sun
        let (bsRow,bsCol) = indexer (row,col) n Backw
        let (sBsRow,sBsCol) = indexer (sRow,sCol) n Backw

        if row = sRow && col <> sCol then       // Horizontal match
            rayCheck inp[row] col sCol 
        else if col = sCol && row <> sRow then  // Vertical match
            rayCheck vGroups[col] row sRow

        // fs diagonal
        else if col+row = sCol+sRow then        // Forward diagonal match
            let (diagRow,diagCol) = indexer (row,col) n Forw
            let (sDiagRow,sDiagCol) = indexer (sRow,sCol) n Forw
            rayCheck fsGroups[diagRow] diagCol sDiagCol

        else if bsRow = sBsRow && bsCol <> sBsCol then // Backward diagonal match
            rayCheck bsGroups[bsRow] bsCol sBsCol
        else []


    let filters (row:int) (col:int) : int list =
        let normalSudokuRules = [1..n] |>
                                filterOut hGroups[row] |>
                                filterOut vGroups[col] |>
                                filterOut subGroups[col/a+(row/b)*b]
        match sun with
            None -> normalSudokuRules
            | Some coord ->
                normalSudokuRules |> filterOut (filSun row col)

    let getEmptyFields board =
        let (h, w) = List.length board, List.length board.[0]
        let rec helper row col =
            if row = h then []
            else if col = w then helper (row + 1) 0
            else if board.[row].[col] = 0 then
                let possibleValues = filters row col
                ((row, col), possibleValues) :: helper row (col + 1)
            else helper row (col + 1)
        helper 0 0

    getEmptyFields inp |> List.sortBy (fun (_,poss) -> List.length poss)


let printBoard board =
    for i in board do
        printfn "%A" i


let mutable solNo = 0
let rec solver (brd: int list list) =
    let poss = findPotentials brd
    if List.length poss = 0 then    // all fields occupied.
        solNo <- solNo + 1          // I must have found a solution.
        printfn "SOLUTION %A" solNo
        printBoard brd
        printfn ""
    else
        let ((x,y),p) = poss[0]
        for number in p do // silently passes in case of dead end
            let thisBoard : int list list= 
                List.mapi (fun i (lst:int list) -> 
                    if i = x then
                        lst.[0..y - 1] @ [number] @ lst.[y + 1..]
                    else lst) (brd:int list list)
            solver thisBoard

do solver input


