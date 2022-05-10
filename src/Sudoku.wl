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


BeginPackage["Sudoku`"];

SudokuGame::usage="Gioco del sudoku";
ShowSudoku::usage="mostra";
CreateSudoku::usage=" ";
Begin["`Private`"];


(* ::InheritFromParent:: *)
(*"Crea sudoku 2*)


row[r_, index_ ]:=Module[{norm,dupl, coord, aux},
norm  = Normal[r];
dupl=Cases[Tally[Cases[norm, _Integer]], {x_,y_}/;y >1];
coord = If[Length[dupl]>0, Flatten[Map[Position[norm,First[#]]&, dupl]], Nothing];
Map[{index,#}&,coord]];

checkErrors[puzzle_]:=Module[{aux},
	aux = Flatten[Table[row[puzzle[[x]],x],{x,1,9}], 1];
	Map[#-> Darker[Red]&,aux]
]

ShowSudoku[board_, dim_, cursor_:{0,0}]:= Module[{background, err, un},
background=Flatten[Table[{i,j}->If[EvenQ[Plus@@Floor[{i-1,j-1}/3]],Darker[White],White],{i,9},{j,9}]];
err = checkErrors[board];
un = Join[background,err];
Grid[board,
ItemSize->Full,
Frame->If[Equal[cursor,{0,0}],All,{All,All,cursor->{Blue}}],
BaseStyle->dim,
Spacings->{Offset[0.9],Offset[0.6]},
Background -> {Automatic, Automatic,
  un}]]


CreateSudoku[dim_, seed_]:=
Module[
{fullBoard, sudokuPuzzle, nEl, diffic, startPosition},
SeedRandom[seed];
diffic = getDifficoltaCarica[seed];
nEl = Switch[diffic, "Tutorial", 0.9, "Facile", 0.7, "Medio", 0.5, "Difficile", 0.2];
{fullBoard, sudokuPuzzle} = ResourceFunction["GenerateSudokuPuzzle"][dim, nEl];
startPosition = Position[Normal[sudokuPuzzle], _Integer];
Return[<|"fullBoard" -> fullBoard, "sudokuPuzzle" -> sudokuPuzzle, "startPosition"-> startPosition|>]
]


avviaTimer[] := Dynamic[Refresh[timer = timer + 1; convert[timer], TrackedSymbols :> {}, UpdateInterval -> 1]];
titleElemStyle[s_]:= Style[s, FontSize->16, Bold];
titleMainStyle[s_]:= Style[s, FontSize->16];
elemStyle[s_]:= Style[s, FontSize->14, Bold];
mainStyle[s_]:= Style[s, FontSize->14];
convert[x_] := elemStyle[UnitConvert[Quantity[x, "Seconds"], MixedUnit[{"Hours", "Minutes", "Seconds"}]]];
getDifficoltaCarica[seed_] := Switch[Mod[seed, 4], 0, "Tutorial", 1, "Facile", 2, "Medio", _, "Difficile"];
loc2[{x_,y_}, startPosition_] := Module[{coord},
coord={Floor[9(1-y)]+1, Floor[9x]+1};
If[!MemberQ[startPosition, coord], coord,{0,0}]
];



generaNuovoSeed[difficolta_]:=Module[{},
	newSeed = IntegerPart[RandomInteger[{10,100000}]*4];
	Switch[difficolta, "Tutorial", newSeed, "Facile", newSeed + 1, "Medio", newSeed + 2, _, newSeed + 3]
]


SudokuGame[] := DynamicModule[
{
(* Difficolt\[AGrave] *)
	difficolta = "Tutorial",
	difficoltaInCorso = "Tutorial",
	popupDifficolta = PopupMenu[Dynamic[difficolta], {"Tutorial", "Facile", "Medio", "Difficile"}, FieldSize -> Small],
	difficoltaCarica = "Tutorial",
	popupDifficoltaCarica = PopupMenu[Dynamic[difficoltaCarica], {"Tutorial", "Facile", "Medio", "Difficile"}, FieldSize -> Small],
(*Sudoku*)
	caricaSudoku = 0,
	caricaSudokuInput = InputField[Dynamic[caricaSudoku], FieldSize->Small],
	sudoku,
	fullBoard,
	puzzle,
	startPosition,
	cursor = {0,0},
	inputValue,
(* Timer *)
	timer = 0, 
(* Aiuto *)
	aiuto = False,
	aiutoCheckbox = Checkbox[Dynamic[aiuto]],
(* Mostra Soluzione *)
	mostraSoluzione = False,
	mostraSoluzioneCheckbox = Checkbox[Dynamic[mostraSoluzione]],
(* Manipulate *)
	dimensioneManipulate = {larghezza = 700, altezza = 500}
 },
 
 numSudoku = generaNuovoSeed[difficolta];
 sudoku = CreateSudoku[3, numSudoku];
 fullBoard = sudoku[["fullBoard"]];
 puzzle = sudoku[["sudokuPuzzle"]];
 startPosition = sudoku[["startPosition"]]; 

Manipulate[
	Grid[{
		{
		Column[{
			EventHandler[
				Dynamic[ShowSudoku[puzzle, 22, cursor]], 
				{"MouseClicked":> (cursor = loc2[MousePosition["EventHandlerScaled"]])}
			],"  ",
			Column[{
				Style["Seleziona il valore che vuoi inserire:","Label", 15],
				EventHandler[
					Grid[
					{{" ",1,2,3,4,5,6,7,8,9}},Frame -> All, BaseStyle->Large],
					"MouseClicked" :> Module[
						{num = Floor[10First@MousePosition["EventHandlerScaled"]]},
					     If[num == 0,, puzzle[[cursor[[1]]]][[cursor[[2]]]] = num]
					     ]
				]
			}],
		}],
		"\t",
		If[mostraSoluzione,Dynamic[ShowSudoku[fullBoard, 15, cursor]],""]
		}
	}],
 (*{{puzzle, (solution = CreateSudoku[3,difficoltaInCorso]; createPuzzle[solution]))}, ControlType -> None},
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
         Button["Ricomincia", (
         timer = -1;
         aiuto = False;
         mostraSoluzione = False;
         sudoku = CreateSudoku[3, numSudoku];
         fullBoard = sudoku[["fullBoard"]];
         puzzle = sudoku[["sudokuPuzzle"]];
         startPosition = sudoku[["startPosition"]]; 
         )&]
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
         cursor = {0,0};
         numSudoku = generaNuovoSeed[difficoltaInCorso];
         sudoku = CreateSudoku[3, numSudoku];
         fullBoard = sudoku[["fullBoard"]];
         puzzle = sudoku[["sudokuPuzzle"]];
         startPosition = sudoku[["startPosition"]]; 
         )&]}]]
     }],
     Spacer[{50,0}],
     Column[{
     Spacer[{0,5}],
     elemStyle["Carica Sudoku:"],
     Panel[
     Column[{
         Grid[Transpose[{{mainStyle["Numero sudoku:"]}, {caricaSudokuInput}}]],
         Button["Carica Sudoku", (
         timer = -1;
         aiuto = False;
         mostraSoluzione = False;
         numSudoku = caricaSudoku;
         sudoku = CreateSudoku[3, numSudoku];
         difficoltaInCorso = getDifficoltaCarica[numSudoku];
         fullBoard = sudoku[["fullBoard"]];
         puzzle = sudoku[["sudokuPuzzle"]];
         startPosition = sudoku[["startPosition"]]; )&]}]]
     }]}]],
 SaveDefinitions -> True, 
 ContentSize -> dimensioneManipulate,
 ControlPlacement -> {Top, Top, Bottom}]]


End[];
EndPackage[];
