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


convert[x_] := elemStyle[UnitConvert[Quantity[x, "Seconds"], MixedUnit[{"Hours", "Minutes", "Seconds"}]]];
avviaTimer[] := Dynamic[Refresh[timer = timer + 1; convert[timer], TrackedSymbols :> {}, UpdateInterval -> 1]];
titleElemStyle[s_]:= Style[s, FontSize->16, Bold];
titleMainStyle[s_]:= Style[s, FontSize->16];
elemStyle[s_]:= Style[s, FontSize->14, Bold];
mainStyle[s_]:= Style[s, FontSize->14];
generaNuovoSeed[]:=RandomInteger[{1,1000}];

loc2[{x_,y_}] := {Floor[9(1-y)]+1, Floor[9x]+1};


SudokuGame[] := DynamicModule[
{
(*Sudoku*)
	numSudoku = RandomInteger[],
	caricaSudoku = 0,
	caricaSudokuInput = InputField[Dynamic[caricaSudoku], FieldSize->Small],
(* Timer *)
	timer = 0, 
(* Difficolt\[AGrave] *)
	difficolta = "Tutorial",
	difficoltaInCorso = "Tutorial",
	popupDifficolta = PopupMenu[Dynamic[difficolta], {"Tutorial", "Facile", "Medio", "Difficile"}, FieldSize -> Small],
	difficoltaCarica = "Tutorial",
	popupDifficoltaCarica = PopupMenu[Dynamic[difficoltaCarica], {"Tutorial", "Facile", "Medio", "Difficile"}, FieldSize -> Small],
	
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
 (*{{puzzle, (solution = CreateSudoku[3,0.5]; createPuzzle[solution])}, ControlType -> None},
 {{cursor, 0}, ControlType -> None},*)
Control[
Row[{Spacer[170],
     titleMainStyle["Difficolt\[AGrave]:  "], titleElemStyle[Dynamic[difficoltaInCorso]], 
     Spacer[20],
     titleMainStyle["Numero Sudoku:  "], titleElemStyle[Dynamic[numSudoku]]     
    }]
 ],
Control[
Row[{
     Spacer[{20, 50}],
     Column[{
         Grid[Transpose[{{mainStyle["Aiuto: "], mainStyle["Mostra soluzione: "]},{aiutoCheckbox, mostraSoluzioneCheckbox}}]],
         Button["Ricomincia", (timer = -1; aiuto = False; mostraSoluzione = False;)&]
     }],
     Spacer[{350, 0}],
     Column[{
         Row[{mainStyle["Tempo:  "], avviaTimer[]}]
     }]
 }]],
 Control[
 Row[{
 Spacer[{100,0}],
 Column[{
     elemStyle["Nuovo Sudoku:"],
     Panel[
     Column[{
         Grid[Transpose[{{mainStyle["Difficolt\[AGrave]: "]}, {popupDifficolta}}]],
         Button["Nuovo Sudoku", (
         timer = -1;
         aiuto = False;
         mostraSoluzione = False;
         difficoltaInCorso = difficolta;
         numSudoku = generaNuovoSeed[])&]}]]
     }],
     Spacer[{50,0}],
     Column[{
     Spacer[{0,5}],
     elemStyle["Carica Sudoku:"],
     Panel[
     Column[{
         Grid[Transpose[{{mainStyle["Numero sudoku:"], mainStyle["Difficolt\[AGrave]: "]}, {caricaSudokuInput, popupDifficoltaCarica}}]],
         Button["Carica Sudoku", (
         timer = -1;
         aiuto = False;
         mostraSoluzione = False;
         difficoltaInCorso = difficoltaCarica;
         numSudoku = caricaSudoku)&]}]]
     }]}]],
 SaveDefinitions -> True, 
 ContentSize -> dimensioneManipulate,
 ControlPlacement -> {Top, Top, Bottom}]]


End[]
EndPackage[]
