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

SudokuGame::usage="Gioco del sudoku"
ShowSudoku::usage="mostra"
CreateSudoku::usage=" "
Begin["`Private`"]


(* ::InheritFromParent:: *)
(*"Crea sudoku 2*)


ShowSudoku[board_, dim_, cursor_:{0,0}]:= Module[{},
Grid[board,ItemSize->Full,Frame->If[Equal[cursor,{0,0}],All,{All,All,cursor->{Blue}}],BaseStyle->dim,Spacings->{Offset[0.9],Offset[0.6]}]]


CreateSudoku[dim_,nEl_]:=Module[
{fullBoard, sudokuPuzzle},
{fullBoard, sudokuPuzzle} = ResourceFunction["GenerateSudokuPuzzle"][dim, nEl];
(*ResourceFunction["DisplaySudokuPuzzle"][#] & /@ {fullBoard, 
  sudokuPuzzle}*/*);
Return[<|"fullBoard" -> fullBoard, "sudokuPuzzle" -> sudokuPuzzle|>]
]


(* Reset *)
(*reset[timer_, aiuto_, mostraSoluzione_] := Module[{}];*)


convert[x_] := UnitConvert[Quantity[x, "Seconds"], MixedUnit[{"Hours", "Minutes", "Seconds"}]];
avviaTimer[] := Dynamic[Refresh[timer = timer + 1; convert[timer], TrackedSymbols :> {}, UpdateInterval -> 1]];
loc2[{x_,y_}] := {Floor[9(1-y)]+1, Floor[9x]+1}


SudokuGame[] := DynamicModule[
{(* Timer *)
	timer = 0, 
(* Difficolt\[AGrave] *)
	difficolta = "Tutorial",
	listaDifficolta = {"Tutorial", "Facile", "Medio", "Difficile"},
	popupDifficolta = PopupMenu[Dynamic[difficolta], listaDifficolta, FieldSize -> Small],
(* Aiuto *)
	aiuto = False,
	aiutoCheckbox = Checkbox[Dynamic[aiuto]],
(* Mostra Soluzione *)
	mostraSoluzione = False,
	mostraSoluzioneCheckbox = Checkbox[Dynamic[mostraSoluzione]],
(* Manipulate *)
	dimensioneManipulate = {larghezza = 650, altezza = 100}
 },


Manipulate[{solution, ControlType -> None},
 {{puzzle, (solution = CreateSudoku[3,0.5]; createPuzzle[solution])}, ControlType -> None},
 {{cursor, 0}, ControlType -> None},
 Row[{Column[{Grid[Transpose[{{"Aiuto", "Mostra soluzione"},
 {aiutoCheckbox, mostraSoluzioneCheckbox}}]],
 Button["Ricomincia", (timer = -1; aiuto = False; mostraSoluzione = False; puzzle =puzzleVuoto;)&]}],
 Spacer[(183)],
 Column[{"Tempo trascorso:", avviaTimer[]}, Alignment -> Center], 
 Spacer[(183)],
 Column[{Grid[Transpose[{{"Difficolt\[AGrave]"}, {popupDifficolta}}]],
 Button["Nuovo Sudoku", (Print[difficolta]; Print[aiuto]; Print[mostraSoluzione]; cursor = 0; solution = randFill[]; puzzle = createPuzzle[solution]; timer = -1; aiuto = False; mostraSoluzione = False;)&]}]}],
 SaveDefinitions -> True, 
 ContentSize -> dimensioneManipulate,
 ControlPlacement -> Top]]


End[]
EndPackage[]
