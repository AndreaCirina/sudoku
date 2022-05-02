(* ::Package:: *)

(* :Title: Sudoku*)
(* :Context: Sudoku'*)
(* :Author: Luca Asunis, Mauro Barbieri, Andrea Cirina, Chiara Manca*)
(* :Summary:*)
(* :Copyright: Alma Mater Studiorum - Bologna 2021 *)
(* :Package Version: 1*)
(* :Mathematica Version: 13*)
(* :History: \\ *)
(* :Keywords: sudoku*)
(* :Sources: \\*)
(* :Discussion: \\*)


BeginPackage["Sudoku`"]

ShowSudoku::usage="mostra sudoku"
CreateSudoku::usage="Crea sudoku 3"
Begin["`Private`"]


(* ::InheritFromParent:: *)
(*"Crea sudoku 2*)


ShowSudoku[board_, dim_, color_]:= Module[
{length = Length[board], n, divs},
dd=Table[color,3];
dd[[1]]={color, Thickness[3]};
Grid[board, ItemSize->Full,BaseStyle->dim, Spacings->{Offset[0.9],Offset[0.6]}, Dividers->{{dd}, {dd}}]
]


CreateSudoku[dim_,nEl_]:=Module[
{fullBoard, sudokuPuzzle},
{fullBoard, sudokuPuzzle} = ResourceFunction["GenerateSudokuPuzzle"][3, 0.4];
(*ResourceFunction["DisplaySudokuPuzzle"][#] & /@ {fullBoard, 
  sudokuPuzzle}*/*);
Return[<|"fullBoard" -> fullBoard, "sudokuPuzzle" -> sudokuPuzzle|>]
]


End[]
EndPackage[]
