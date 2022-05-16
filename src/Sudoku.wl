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


calcErrors[struct_, lFlat_, listPar_]:= Module[{norm,auxNorm,cnt, dupl, coord, aux},
 norm  = Normal[struct];
 auxNorm = If[Length[listPar]==1, Flatten[norm], norm];
 cnt = Tally[Cases[auxNorm, _Integer]];
 dupl = Cases[cnt, {x_,y_}/;y>1];
 coord = If[Length[dupl]>0, Flatten[Map[Position[norm,First[#]]&, dupl], lFlat], Nothing];
 Switch[Length[listPar], 
  1, Map[{First[#]+listPar[[1,1]]-1,#[[2]]+listPar[[1,2]]-1}&,coord],
  2, If[listPar[[2]],Map[{listPar[[1]],#}&,coord], Map[{#,listPar[[1]]}&,coord]]]
];

checkErrors[puzzle_, startPosition_, maxInd_]:=Module[{auxRow, auxColumn, maxSq, incr, auxSquare, union, final},
	auxRow = Flatten[Table[calcErrors[puzzle[[x]],2,{x, True}],{x,1,maxInd}], 1];
	auxColumn =Flatten[Table[calcErrors[puzzle[[All,x]],2,{x,False}],{x,1,maxInd}], 1];
	maxSq = If[maxInd==9, 7, 3];
	incr = If[maxInd==9, 3, 2];
	auxSquare = Flatten[Table[calcErrors[puzzle[[x;;x+(incr-1),y;;y+(incr-1)]],1, {{x,y}}],{x,1,maxSq,incr},{y,1,maxSq,incr}],2];
	union = Union[auxRow,auxColumn,auxSquare];
	final = DeleteCases[union, {x_,y_}/;MemberQ[startPosition,{x,y}]];
	Map[#-> LightRed&, final]
]

(* Metodo per visualizzare il sudoku. *)
ShowSudoku[board_, dim_, dimQ_, cursor_:{0,0}, startPosition_:{}, aiuti_:False]:= Module[
 {background, err, un, numberColor, maxIndex},
 maxIndex= If[dimQ == 2, 4, 9];
 background= Flatten[Table[{i,j}->If[EvenQ[Plus@@Floor[{i-1,j-1}/dimQ]],Darker[White],White],{i,maxIndex},{j,maxIndex}]];
 numberColor = If[startPosition != {},
 Flatten[Table[If[MemberQ[startPosition, {x,y}], {x,y} -> {Black},{x,y} -> {Blue} ], {x, 1,maxIndex},{y, 1,maxIndex}]], Black];
 un = If[aiuti,
	  err = checkErrors[board, startPosition, maxIndex];
	  Join[background,err],
	  background
	 ];
 Grid[board,
  ItemSize->Full,
  ItemStyle->{Automatic, Automatic, numberColor},
  Frame-> If[Equal[cursor,{0,0}], All, {All, All, cursor->{Blue, Thickness->1.5}}],
  BaseStyle->dim,
  Spacings->{Offset[1.2],Offset[0.9]},
  Background -> {Automatic, Automatic, un}]
]


(*Metodo per creare il sudoku. Prende come parametro la dimensione del sudoku ed il seed. *)
CreateSudoku[dim_, seed_]:= Module[
  {sudokuCompleto, sudokuDaCompletare, percentualeDifficolta, difficolta, posizioniIniziali},
  SeedRandom[seed];                                                         (*Setta il seed per generare sempre lo stesso sudoku. *)
  difficolta = getDifficoltaCarica[seed];                                   (*Prendiamo la difficolt\[AGrave] del sudoku in base al seed. *)
  (*A seconda della difficolta verra settata la variabile della percentuale delle caselle che saranno piene. *)
  percentualeDifficolta = Switch[difficolta, "Tutorial", 0.90, "Facile", 0.7, "Medio", 0.5, "Difficile", 0.2];
  (*Generiamo il sudoku completo (soluzione) ed il sudoku da completare grazie alla funzione pre-esistente. *)
  {sudokuCompleto, sudokuDaCompletare} = ResourceFunction["GenerateSudokuPuzzle"][dim, percentualeDifficolta];
  posizioniIniziali = Position[Normal[sudokuDaCompletare], _Integer]; (*Salviamo le posizioni occupate dai numeri fissi nel sudoku da completare. *)
  (*Restituiamo il sudoku completo, il sudoku da completare ed i numeri fissi del nostro sudoku. *)
  Return[<|"fullBoard" -> sudokuCompleto, "sudokuPuzzle" -> sudokuDaCompletare, "startPosition"-> posizioniIniziali|>]
]


(*Funzioni per lo stile delle label nel men\[UGrave]. *)
titleElemStyle[s_]:= Style[s, FontSize->16, Bold];
titleMainStyle[s_]:= Style[s, FontSize->16];
elemStyle[s_]:= Style[s, FontSize->14, Bold, FontFamily->"Arial"];
mainStyle[s_]:= Style[s, FontSize->14];
vittoriaStyle[s_] := Style[s, FontSize->30, Bold, RGBColor[0,0.64,0.36], FontFamily->"Arial", TextAlignment->Center];
(*Funzione per creare un PopupMenu di base data la variabile dinamica da collegare ad esso. *)
creaPopup[var_]:=PopupMenu[Dynamic[var], {"Tutorial", "Facile", "Medio", "Difficile"}, FieldSize -> Small];
(*Funzione per creare una CheckBox di base data la variabile dinamica da collegare ad essa. *)
creaCheckBox[var_]:= Checkbox[Dynamic[var], Enabled->Dynamic[controlliAttivi]];
(*Funzione che converte i secondi in ore, minuti e secondi. *)
convert[x_] := elemStyle[UnitConvert[Quantity[x, "Seconds"], MixedUnit[{"Hours", "Minutes", "Seconds"}]]];
(*Funzione che permette di far partire il timer del men\[UGrave]. Verr\[AGrave] sommato al timer ogni secondo il valore 1 e grazie alla funzione "convert" convertiamo i 
secondi in ore,minuti e secondi. *)
avviaTimer[] := Dynamic[Refresh[If[refreshTimer, timer = timer + 1, timer]; convert[timer], TrackedSymbols :> {}, UpdateInterval -> 1]];
(*Funzione che dato un seed ci fa sapere la difficolt\[AGrave] del sudoku dato il suo seed. Questo \[EGrave] possibile perch\[EGrave] abbiamo fatto in modo che dato il modulo
in base 4 del seed, se da resto 0 significa che \[EGrave] un tutorial, 1 \[EGrave] difficolt\[AGrave] facile e cos\[IGrave] via.  *)
getDifficoltaCarica[seed_] := Switch[Mod[seed, 4], 0, "Tutorial", 1, "Facile", 2, "Medio", _, "Difficile"];
(*Stampa del timer nella manipulate. *)
stampaTimerManipulate[] := Column[{Row[{mainStyle["Tempo:  "], avviaTimer[]}]}];
(*Stampa messaggio di vittoria. *)
stampaVittoria[time_] := 
 Row[{Column[{
 vittoriaStyle["\n\n\n\n\t        Complimenti! Hai vinto!"], elemStyle[StringJoin["\n\n\n\t\t\t\t\t         Tempo trascorso:\n\t\t\t\t\t", ToString[convert[time]]]]}]}];
(*Stampa la griglia del sudoku nella manipulate. *)
stampaSudokuManipulate[puzzle_, grandezzaGrigliaSudoku_, dimQuadratoSudoku_, cursor_, startPosition_, aiuto_] :=
 Dynamic[ShowSudoku[puzzle, grandezzaGrigliaSudoku,dimQuadratoSudoku, cursor, startPosition, aiuto]];
(*Stampa la griglia per sscrivere i numeri nel sudoku nell amanipulate. *) 
stampaGrigliaSelezioneNumeri[difficolta_]:= Module[
 {grigliaSelezionaNumeri, grigliaNumeriTutorial, griglia},
 grigliaSelezionaNumeri = {{"C",1,2,3,4,5,6,7,8,9}};       (*Griglia di selezione dei numeri. *)
 grigliaNumeriTutorial = {{"C", 1,2,3,4}};                 (*Griglia di selezione dei numeri per il tutorial. *)
 griglia = If[difficolta === "Tutorial", grigliaNumeriTutorial, grigliaSelezionaNumeri];
 Grid[
   griglia,
   Frame -> All,
   Background->RGBColor[0.5,0.74,0.5,0.4],
   BaseStyle->Large]];
(*Restituisce True se il sudoku \[EGrave] stato completato correttamente, False altrimenti. *)
checkVittoria[fullBoard_, puzzle_]:= Normal[fullBoard] === Normal[puzzle];


loc2[{x_,y_}, startPosition_, dimQ_] := Module[
 {coord},
 coord={Floor[dimQ(1-y)]+1, Floor[dimQ x]+1};
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


getDimSudoku[diff_]:= If[diff == "Tutorial", 2, 3]


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
 controlliAttivi = True,
(* Timer *)
 timer = 0,                                                (*Tempo di esecuzione del sudoku. *)
 refreshTimer = True,                                      (*Flag per fermare il tempo. *)
(* Aiuto *)
 aiuto = False,                                            (*Valore della checkbox dell'aiuto. *)
 aiutoCheckbox = creaCheckBox[aiuto],                      (*Checkbox dell'aiuto. *)
(* Mostra Soluzione *)
 mostraSoluzione = False,                                  (*Valore della checkbox di mostra soluzione. *)
 mostraSoluzioneCheckbox = creaCheckBox[mostraSoluzione],  (*Checkbox di mostra soluzione. *)
(* Manipulate *)
 dimensioneManipulate = {larghezza = 700, altezza = 480},   (*Dimensione dell'intera manipulate. *)
 dimQuadratoSudoku
},

 (*Generazione del seed del sudoku iniziale. Sar\[AGrave] la variabile che conterr\[AGrave] il seed ogni volta. *)
 numSudoku = generaNuovoSeed[difficoltaInCorso];            
 (*Conterr\[AGrave] di volta in volta le 3 variabili che vediamo sotto. *)
 dimQuadratoSudoku = getDimSudoku[difficoltaInCorso];
 sudoku = CreateSudoku[dimQuadratoSudoku, numSudoku];
 fullBoard = sudoku[["fullBoard"]];              (*Griglia completa del Sudoku. *)
 puzzle = sudoku[["sudokuPuzzle"]];              (*Griglia da completare del Sudoku. *)
 startPosition = sudoku[["startPosition"]];      (*Posizioni fisse del Sudoku. *)

Manipulate[
(*Contenuto principale della manipulate. *)
 Grid[{{
  (*Griglia sudoku, la visualizziamo solo se l'utente non ha ancora vinto. *)
  Column[{
  If[checkVittoria[fullBoard, puzzle], refreshTimer = False; mostraSoluzione = False; controlliAttivi = False; stampaVittoria[timer],
  EventHandler[
   stampaSudokuManipulate[puzzle, grandezzaGrigliaSudoku, dimQuadratoSudoku, cursor, startPosition, aiuto],
   {"MouseClicked":> (cursor = loc2[MousePosition["EventHandlerScaled"], startPosition, dimQuadratoSudoku^2])}
  ]],
  (* Griglia numeri da selezionare, appare solo quando c'\[EGrave] una cella selezionata per evitare errori e quando l'utente
  non ha ancora vinto. *)
  If[checkVittoria[fullBoard, puzzle] || cursor === {0,0}, "",
   Column[{
    elemStyle["\nSeleziona il valore che vuoi inserire:"],
    EventHandler[
     stampaGrigliaSelezioneNumeri[difficoltaInCorso],
      "MouseClicked" :> Module[
      (*Se \[EGrave] il tutorial cambia il mappaggio delle celle per la selezione dei numeri. *)
	  {num = If[difficoltaInCorso === "Tutorial", Floor[5First@MousePosition["EventHandlerScaled"]], Floor[10First@MousePosition["EventHandlerScaled"]]]},
	  (*Se \[EGrave] stata selezionata la caseslla per cancellare mettiamo un trattino al suo interno, altrimenti il numero selezionato. *)
	  If[num == 0, puzzle[[cursor[[1]]]][[cursor[[2]]]] = _, puzzle[[cursor[[1]]]][[cursor[[2]]]] = num]]]
    }]]
   }],
   "\t",
	(*Griglia soluzione. *)
	If[mostraSoluzione, Dynamic[ShowSudoku[fullBoard, 15, dimQuadratoSudoku, cursor]], ""]
  }}, Editable->False],
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
     controlliAttivi = True;                     (*Riattiva checkbox. *)
     refreshTimer = True;                        (*Ricomincio a contare il tempo*)
     mostraSoluzione = False;                    (*Resetta Checkbox mostra soluzione. *)
     sudoku = CreateSudoku[dimQuadratoSudoku, numSudoku];        (*Creiamo lo stesso sudoku che avevamo all'inizio. *)
     fullBoard = sudoku[["fullBoard"]];            (*Resettiamo la griglia piena. FORSE POSSIAMO EVITARLO*)
     puzzle = sudoku[["sudokuPuzzle"]];            (*Resettiamo la griglia da riempire. *)
     startPosition = sudoku[["startPosition"]];)&] (*Resettiamo le posizioni fisse. FORSE POSSIAMO EVITARLO*)
    }],
    Spacer[{350, 0}],
    (*Timer. *)
    stampaTimerManipulate[]
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
      controlliAttivi = True;                            (*Riattiva checkbox. *)
      refreshTimer = True;                               (*Ricomincio a contare il tempo*)
      mostraSoluzione = False;                           (*Resetta Checkbox mostra soluzione. *)
      difficoltaInCorso = difficolta;                    (*Settiamo la nuova difficolt\[AGrave] come quella selezionata nella dropdown. *)
      cursor = {0,0};                                    (*Resettiamo il cursore. *)
      numSudoku = generaNuovoSeed[difficoltaInCorso];    (*Generiamo il nuovo seed con la difficolt\[AGrave] scelta. *)
      dimQuadratoSudoku = getDimSudoku[difficoltaInCorso];
      sudoku = CreateSudoku[dimQuadratoSudoku, numSudoku];
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
        controlliAttivi = True;                             (*Riattiva checkbox. *)
        refreshTimer = True;                                (*Ricomincio a contare il tempo*)
        mostraSoluzione = False;                            (*Resetta Checkbox mostra soluzione. *)
        numSudoku = caricaSudoku;                           (*Il seed del nuovo sudoku sar\[AGrave] quello scritto nell'InputField. *)
        difficoltaInCorso = getDifficoltaCarica[numSudoku]; (*Ricaviamoci la difficolt\[AGrave] del sudoku che andremo a risolvere per scriverla in alto. *)
        dimQuadratoSudoku = getDimSudoku[difficoltaInCorso];
        sudoku = CreateSudoku[dimQuadratoSudoku, numSudoku]; (*Creiamo il sudoku con tale seed. *)
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
