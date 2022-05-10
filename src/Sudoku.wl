(* ::Package:: *)

(* :Title: Sudoku*)
(* :Context: Sudoku'*)
(* :Author: Luca Asunis, Mauro Barbieri, Andrea Cirina, Chiara Manca*)
(* :Summary:*)
(* :Copyright: Alma Mater Studiorum - Bologna 2022 *)
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


row[r_, index_ ]:= Module[
 {norm,dupl, coord, aux},
 norm  = Normal[r];
 dupl=Cases[Tally[Cases[norm, _Integer]], {x_,y_}/;y >1];
 coord = If[Length[dupl]>0, Flatten[Map[Position[norm,First[#]]&, dupl]], Nothing];
 Map[{index,#}&,coord]];

checkErrors[puzzle_]:= Module[
 {aux},
 aux = Flatten[Table[row[puzzle[[x]],x],{x,1,9}], 1];
 Map[#-> Darker[Red]&,aux]
]

(* Metodo per visualizzare il sudoku. *)
ShowSudoku[board_, dim_, cursor_:{0,0}]:= Module[{background, err, un},
background=Flatten[Table[{i,j}->If[EvenQ[Plus@@Floor[{i-1,j-1}/3]],Darker[White],White],{i,9},{j,9}]];
err = checkErrors[board];
un = Join[background,err];
Grid[board,
ItemSize->Full,
Frame->If[Equal[cursor,{0,0}],All,{All,All,cursor->{Blue}}],
BaseStyle->dim,
Spacings->{Offset[0.9],Offset[0.6]},
Background -> {Automatic, Automatic, un}]]


(*Metodo per creare il sudoku. Prende come parametro la dimensione del sudoku ed il seed. *)
CreateSudoku[dim_, seed_]:= Module[
  {sudokuCompleto, sudokuDaCompletare, percentualeDifficolta, difficolta, posizioniIniziali},
  SeedRandom[seed];                                                         (*Setta il seed per generare sempre lo stesso sudoku. *)
  difficolta = getDifficoltaCarica[seed];                                   (*Prendiamo la difficolt\[AGrave] del sudoku in base al seed. *)
  (*A seconda della difficolta verra settata la variabile della percentuale delle caselle che saranno piene. *)
  percentualeDifficolta = Switch[difficolta, "Tutorial", 0.9, "Facile", 0.7, "Medio", 0.5, "Difficile", 0.2];
  (*Generiamo il sudoku completo (soluzione) ed il sudoku da completare grazie alla funzione pre-esistente. *)
  {sudokuCompleto, sudokuDaCompletare} = ResourceFunction["GenerateSudokuPuzzle"][dim, percentualeDifficolta];
  posizioniIniziali = Position[Normal[sudokuDaCompletare], _Integer]; (*Salviamo le posizioni occupate dai numeri fissi nel sudoku da completare. *)
  (*Restituiamo il sudoku completo, il sudoku da completare ed i numeri fissi del nostro sudoku. *)
  Return[<|"fullBoard" -> sudokuCompleto, "sudokuPuzzle" -> sudokuDaCompletare, "startPosition"-> posizioniIniziali|>]
]


(*Funzione che permette di far partire il timer del men\[UGrave]. Verr\[AGrave] sommato al timer ogni secondo il valore 1 e grazie alla funzione "convert" convertiamo i 
secondi in ore,minuti e secondi. *)
avviaTimer[] := Dynamic[Refresh[timer = timer + 1; convert[timer], TrackedSymbols :> {}, UpdateInterval -> 1]];
(*Funzioni per lo stile delle label nel men\[UGrave]. *)
titleElemStyle[s_]:= Style[s, FontSize->16, Bold];
titleMainStyle[s_]:= Style[s, FontSize->16];
elemStyle[s_]:= Style[s, FontSize->14, Bold];
mainStyle[s_]:= Style[s, FontSize->14];
(*Funzione per creare un PopupMenu di base data la variabile dinamica da collegare ad esso. *)
creaPopup[var_]:=PopupMenu[Dynamic[var], {"Tutorial", "Facile", "Medio", "Difficile"}, FieldSize -> Small];
(*Funzione per creare una CheckBox di base data la variabile dinamica da collegare ad essa. *)
creaCheckBox[var_]:= Checkbox[Dynamic[var]];
(*Funzione che converte i secondi in ore, minuti e secondi. *)
convert[x_] := elemStyle[UnitConvert[Quantity[x, "Seconds"], MixedUnit[{"Hours", "Minutes", "Seconds"}]]];
(*Funzione che dato un seed ci fa sapere la difficolt\[AGrave] del sudoku dato il suo seed. Questo \[EGrave] possibile perch\[EGrave] abbiamo fatto in modo che dato il modulo
in base 4 del seed, se da resto 0 significa che \[EGrave] un tutorial, 1 \[EGrave] difficolt\[AGrave] facile e cos\[IGrave] via.  *)
getDifficoltaCarica[seed_] := Switch[Mod[seed, 4], 0, "Tutorial", 1, "Facile", 2, "Medio", _, "Difficile"];


loc2[{x_,y_}, startPosition_] := Module[
 {coord},
 coord={Floor[9(1-y)]+1, Floor[9x]+1};
 If[!MemberQ[startPosition, coord], coord,{0,0}]
];


(*Metodo che data una difficolt\[AGrave] permette di generare un seed per essa. Facciamo in modo che il seed generato sia un multiplo di 4 se vogliamo generare
un Tutorial oppure che abbia resto 1 se Facile e cos\[IGrave] via. *)
generaNuovoSeed[difficolta_]:= Module[
 {seedDiBase},
 seedDiBase = IntegerPart[RandomInteger[{10,100000}]*4];  (*Seed multiplo di 4*)
 (*Aggiungiamo "il resto" a seconda della difficolt\[AGrave] scelta. *)
 Switch[difficolta, "Tutorial", seedDiBase, "Facile", seedDiBase + 1, "Medio", seedDiBase + 2, _, seedDiBase + 3] 
]


SudokuGame[] := DynamicModule[
{
(* Difficolt\[AGrave] *)
 difficolta = "Tutorial",                                (*Difficolt\[AGrave] della dropDown di "Nuova Partita". *)
 difficoltaInCorso = "Tutorial",                         (*Difficolt\[AGrave] del sudoku che si sta eseguendo. *)
 difficoltaCarica = "Tutorial",                          (*Difficolt\[AGrave] della dropDown di "Carica partita". *)
 popupDifficolta = creaPopup[difficolta],                (*Popup della difficolt\[AGrave] di "Nuova Partita". *)
 popupDifficoltaCarica = creaPopup[difficoltaCarica],    (*Popup della difficolt\[AGrave] di "Carica Partita". *)
(*Sudoku*)
 grandezzaGrigliaSudoku = 22,
 caricaSudoku = 0,                                         (*Variabile dell'inputField del Seed. *)
 caricaSudokuInput = InputField[Dynamic[caricaSudoku], FieldSize->Small], (*InputField del Seed. *)
 numSudoku,      (*Seed del sudoku che stiamo completando. *)
 sudoku,         (*Variabile utile per prendere i risultati di "Create Sudoku". *)
 fullBoard,      (*Schema del sudoku completo. *)
 puzzle,         (*Schema del sudoku da completare. *)
 startPosition,  (*Posizioni non modificabili nel sudoku. *)
 cursor = {0,0}, (*Cursore che ci dar\[AGrave] la posizione del mouse all'interno del nostro sudoku. *)
 inputValue,     (**) 
 grigliaSelezionaNumeri = {{" ",1,2,3,4,5,6,7,8,9}}, 
(* Timer *)
 timer = 0, 
(* Aiuto *)
 aiuto = False,
 aiutoCheckbox = creaCheckBox[aiuto],
(* Mostra Soluzione *)
 mostraSoluzione = False,
 mostraSoluzioneCheckbox = creaCheckBox[mostraSoluzione],
(* Manipulate *)
 dimensioneManipulate = {larghezza = 700, altezza = 430}
},
 
 numSudoku = generaNuovoSeed[difficoltaInCorso];
 sudoku = CreateSudoku[3, numSudoku];
 fullBoard = sudoku[["fullBoard"]];
 puzzle = sudoku[["sudokuPuzzle"]];
 startPosition = sudoku[["startPosition"]]; 

Manipulate[
(*Contenuto principale della manipulate. *)
 Grid[{{
  (*Griglia sudoku*)
  Column[{
  EventHandler[
   Dynamic[ShowSudoku[puzzle, grandezzaGrigliaSudoku, cursor]],
   {"MouseClicked":> (cursor = loc2[MousePosition["EventHandlerScaled"]])}
   ],
   "  ",
   (*Griglia numeri da selezionare*)
   Column[{
    Style["Seleziona il valore che vuoi inserire:","Label", 15],
    EventHandler[
     Grid[
      grigliaSelezionaNumeri,
      Frame -> All,
      BaseStyle->Large],
      "MouseClicked" :> Module[
	  {num = Floor[10First@MousePosition["EventHandlerScaled"]]},
	  If[num == 0,, puzzle[[cursor[[1]]]][[cursor[[2]]]] = num]]]}]}
	],
	"\t",
	(*Griglia soluzione. *)
	If[mostraSoluzione, Dynamic[ShowSudoku[fullBoard, 15, cursor]], ""]
  }}],
 (*Intestazione manipulate con scritta della difficolt\[AGrave] e del seed. *)
 Control[
  Row[{
   Spacer[170],
   titleMainStyle["Difficolt\[AGrave]:  "], 
   titleElemStyle[Dynamic[difficoltaInCorso]],
   Spacer[20],
   titleMainStyle["Numero Sudoku:  "],
   titleElemStyle[Dynamic[numSudoku]]
  }]
 ],
 (*Insieme delle checkbox e del bottone Ricomincia. *)
 Control[
  Row[{
   Spacer[{20, 50}],
   Column[{
    Grid[Transpose[{{mainStyle["Aiuto: "], mainStyle["Mostra soluzione: "]},{aiutoCheckbox, mostraSoluzioneCheckbox}}]],
    Button["Ricomincia", (
     timer = -1;                                 (*Resetta timer. *)
     aiuto = False;                              (*Resetta aiuto. *)
     mostraSoluzione = False;                    (*Resetta Checkbox mostra soluzione. *)
     sudoku = CreateSudoku[3, numSudoku];        (*Creiamo lo stesso sudoku che avevamo all'inizio. *)
     fullBoard = sudoku[["fullBoard"]];            (*Resettiamo la griglia piena. FORSE POSSIAMO EVITARLO*)
     puzzle = sudoku[["sudokuPuzzle"]];            (*Resettiamo la griglia da riempire. *)
     startPosition = sudoku[["startPosition"]];)&] (*Resettiamo le posizioni fisse. FORSE POSSIAMO EVITARLO*)
    }],
    Spacer[{350, 0}],
    (*Timer. *)
    Column[{
     Row[{mainStyle["Tempo:  "], avviaTimer[]}]
    }]
 }]],
 (*Controlli che stanno sotto la griglia. "Nuovo Sudoku" e "Carica Sudoku". *)
 Control[
  Row[{
   Spacer[{100,0}],
   Column[{
    elemStyle["Nuovo Sudoku:"],
    Panel[
    (*Sezione "Nuovo Sudoku". *)
    Column[{
     Grid[Transpose[{{mainStyle["Difficolt\[AGrave]: "]}, {popupDifficolta}}]],
     Button["Nuovo Sudoku", (
      timer = -1;                                        (*Resetta timer. *)
      aiuto = False;                                     (*Resetta aiuto. *)
      mostraSoluzione = False;                           (*Resetta Checkbox mostra soluzione. *)
      difficoltaInCorso = difficolta;                    (*Settiamo la nuova difficolt\[AGrave] come quella selezionata nella dropdown. *)
      cursor = {0,0};                                    (*Resettiamo il cursore. *)
      numSudoku = generaNuovoSeed[difficoltaInCorso];    (*Generiamo il nuovo seed con la difficolt\[AGrave] scelta. *)
      sudoku = CreateSudoku[3, numSudoku];               (*Generiamo il sudoku con il seed appena generato. *)
      fullBoard = sudoku[["fullBoard"]];                   (*Prendiamo le nuove griglie. *)
      puzzle = sudoku[["sudokuPuzzle"]];
      startPosition = sudoku[["startPosition"]]; )&]}]]    (*Settiamo le nuove posizioni fisse. *)
     }],
     Spacer[{50,0}],
     (*Sezione "Carica Sudoku". *)
     Column[{
      Spacer[{0,5}],
      elemStyle["Carica Sudoku:"],
      Panel[
       Column[{
        Grid[Transpose[{{mainStyle["Numero sudoku:"]}, {caricaSudokuInput}}]],
        Button["Carica Sudoku", (
        timer = -1;                                         (*Resetta timer. *)
        aiuto = False;                                      (*Resetta aiuto. *)
        mostraSoluzione = False;                            (*Resetta Checkbox mostra soluzione. *)
        numSudoku = caricaSudoku;                           (*Il seed del nuovo sudoku sar\[AGrave] quello scritto nell'InputField. *)
        sudoku = CreateSudoku[3, numSudoku];                (*Creiamo il sudoku con tale seed. *)
        difficoltaInCorso = getDifficoltaCarica[numSudoku]; (*Ricaviamoci la difficolt\[AGrave] del sudoku che andremo a risolvere per scriverla in alto. *)
        fullBoard = sudoku[["fullBoard"]];                    (*Prendiamoci le griglie corrette. *)
        puzzle = sudoku[["sudokuPuzzle"]];
        startPosition = sudoku[["startPosition"]]; )&]}]]     (*Settiamo le nuove posizioni fisse. *)
     }]}]],
    SaveDefinitions -> True, 
    ContentSize -> dimensioneManipulate,
    ControlPlacement -> {Top, Top, Bottom}
   ]]


End[];
EndPackage[];
