# Sudoku solver 
* for traditional sudokus as well as variants with non-square subgroups (a X b)
* Can also include a sun-rule.
# Sun rule
* If a field is defined as a sun, it sends rays in all
4 straight and all 4 diagonal directions.
* The rays loose their intensity as they travel away
from the sun
* The fields that they pass through
need to take strictly lower values accordingly.
* The center of the sun is not part of any ray.\
run with `dotnet fsi SudokuSolver.fsx`
