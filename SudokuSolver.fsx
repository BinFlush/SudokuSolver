type direction = Forw | Backw

let (b,a) = (2,4)       // the height and width (respectively) of subgroups (3x3 for normal sudoku)
let sun = Some (4, 4)   // Position of sun (row, column). Set to None if no sun-rule
let input = 
    [
        [0; 0; 0; 0;  2; 0; 0; 7]
        [0; 0; 6; 0;  0; 0; 0; 0]

        [0; 0; 0; 0;  0; 0; 5; 0]
        [0; 0; 0; 4;  0; 0; 0; 0]

        [3; 0; 0; 0;  0; 0; 0; 0]
        [0; 0; 0; 0;  0; 0; 0; 0]

        [2; 0; 0; 0;  0; 0; 0; 0]
        [0; 0; 0; 7;  0; 0; 0; 0]
    ]

let n = List.length input 
let findPotentials (inp:int list list) =
    let hGroups = inp
    let vGroups : int list list = 
            [for i in 0..n-1 ->
                [for j in 0..n-1 ->
                    (inp[j][i])] ]

    let subGroups = 
        [for i in 0..n-1 -> 
            [for j in 0..n-1 -> 
                inp[j/a+b*(i/b)][j%a+a*(i%b)]] |> List.filter (fun elm -> elm <> 0)]

    let indexer ((y,x):int*int) (n:int) (t:direction) =
        if t = Forw then
            let row = x+y
            let bend = max 0 (row - (n-1))
            let col = row - x - bend
            (row,col)
        else// t = Backw then
            let col = min x y
            let row = (n-1)-(x-y)
            (row,col)

    let getDiagonals (b:int list list) =
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
            // consider | A x B SUN B x A | where x are possible field placements
            let helper (theRow:int list) (col:int) (sCol:int) : int list =
                let maxN:int = theRow[..col-1]         // Looks at section A*
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s -> List.max (col::s))
                let minN: int = theRow[col+1..sCol-1]   // Looks at section B*
                                |> List.filter (fun elm -> elm <> 0)
                                |> (fun s -> List.min (n-(sCol-col)+2::s))
                [0..maxN]@[minN..n]

            if col < sCol then 
                helper theRow col sCol
            else if col > sCol then 
                let len = List.length theRow
                helper (List.rev theRow) (len-1-col) (len-1-sCol)
            else []

        let (sRow, sCol) = Option.get sun
        let (bsRow,bsCol) = indexer (row,col) n Backw
        let (sBsRow,sBsCol) = indexer (sRow,sCol) n Backw

        if row = sRow && col <> sCol then       // Horizontal match
            rayCheck inp[row] col sCol 
        else if col = sCol && row <> sRow then  // Vertical match
            rayCheck vGroups[col] row sRow

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

    let getEmptyFields (board:int list list) =
        let rec helper (row:int) (col:int) =
            if row = n then []
            else if col = n then helper (row + 1) 0
            else if board.[row].[col] = 0 then
                let possibleValues = filters row col
                ((row, col), possibleValues) :: helper row (col + 1)
            else helper row (col + 1)
        helper 0 0
    getEmptyFields inp |> List.sortBy (fun (_,poss) -> List.length poss)

let printBoard board n (a,b) (sun:(int*int) option) =
    let colGroups = n/a
    let rowGroups = n/b
    let rowSeperator = "".PadRight(2*n+1,'-')
    let st = 
        board |>
        List.map (fun l -> List.map (fun elm -> elm.ToString()) l)
    printfn "%s" rowSeperator 
    for i in 0..n-1 do
        for j in 0..n-1 do
            if sun.IsSome && (i,j) = sun.Value then printf "<"
            else if sun.IsSome && sun.Value = (i,j-1) then printf ">"
            else if (j)%a = 0 then printf "|"
            else printf " "
            printf "%s" <| st[i][j]

        if sun.IsSome && sun.Value = (i,n-1) then printf ">"
        else printf "|"
        let rowNumber = i+1
        if rowNumber%b = 0 && rowNumber < n then 
            printfn ""
            printf "%s" rowSeperator 
        printfn ""
    printfn "%s" rowSeperator 

let mutable solNo = 0
let rec solver (brd: int list list) =
    let poss = findPotentials brd
    if List.length poss = 0 then    // all fields occupied.
        solNo <- solNo + 1          // I must have found a solution.
        if solNo > 1 then
            printfn ""
        printfn "SOLUTION %A" solNo
        printBoard brd n (a,b) sun
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
