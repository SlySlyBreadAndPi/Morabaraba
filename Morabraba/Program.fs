open System
type Color =
|Dark
|Light
|Nothing
type Node={
    Index: int
    Color: Color
    InAMill:bool
}
type State=
|Flying 
|Moving 
|Placing
|Stationary
|Loser//for when initialising the player object
type Turn=
|P1
|P2
type Player={
    PiecesToPlace: int
    PiecesOnBoard: int
    MoveState: State
    TypeofCow:Color
} 
let Places =["A1";"A4";"A7";"B2";"B4";"B6";"C3";"C4";"C5";"D1";"D2";"D3";"D5";"D6";"D7";"E3";"E4";"E5";"F2";"F4";"F6";"G1";"G4";"G7"]
let listString =
  sprintf "A1%c----------A4%c----------A7%c\n
   |   \       |         /  |\n
   |    B2%c-----B4%c------B6%c   |\n
   |    | \    |     /  |   |\n
   |    |  C3%c--C4%c--C5%c   |   |\n
   D1%c--D2%c--D3%c       D5%c--D6%c--D7%c\n             
   |    |  E3%c--E4%c--E5%c   |   |\n
   |    | /    |     \  |   |\n
   |    F2%c-----F4%c------F6%c   |\n
   |   /       |         \  |\n
   G1%c----------G4%c----------G7%c" 
type Board={
    Board: Node list
}
let CreateBoard = 
   let rec create t arr = match t=24 with
                          |true -> arr
                          |false -> create (t+1) (arr@[Nothing])
   create 0 []
let initPlayer color={PiecesToPlace=4;PiecesOnBoard=0;MoveState=Placing;TypeofCow= match color with //player will be prompterd to choose 1 for dark 2 for light
                                                                         |1-> Dark
                                                                         |_-> Light
                                                                         }
let ConvertRowColumnToIndex index = let index =(string index).ToUpper()
                                    match index with
                                    |"A1"-> 0
                                    |"A4"->1
                                    |"A7"->2
                                    |"B2"->3
                                    |"B4"->4
                                    |"B6"->5
                                    |"C3"->6
                                    |"C4"->7
                                    |"C5"->8
                                    |"D1"->9
                                    |"D2"->10
                                    |"D3"->11
                                    |"D5"->12
                                    |"D6"->13
                                    |"D7"->14
                                    |"E3"->15
                                    |"E4"->16
                                    |"E5"->17
                                    |"F2"->18
                                    |"F4"->19
                                    |"F6"->20
                                    |"G1"->21
                                    |"G4"->22
                                    |"G7"->23
                                    |_-> -1 
let updateboard list index color= match list with 
                                  |[] -> []
                                  |A::B -> list|>List.mapi(fun i item-> match index=i with
                                                                        |true -> color
                                                                        |false->item)
let countpieces board cow = 
  board |> List.fold( fun acc item-> match item=cow with
                                     |true->acc+1
                                     |false-> acc) 0 
let MakeMove place board player = let nodeIndex = ConvertRowColumnToIndex place
                                  let rec tryPlay index boardInner newB player  board=
                                   let {PiecesToPlace=count;PiecesOnBoard=pieces;MoveState=state;TypeofCow=cow}=player
                                   match boardInner with
                                   |[] ->newB
                                   |A::B -> let list,Playr=newB
                                            match A=Nothing with
                                            |true -> let ncount= count-1
                                                     Console.WriteLine(sprintf "player count =%d" ncount) 
                                                     let x = (countpieces board cow)+1
                                                     Console.WriteLine(""+string x)

                                                     match ncount with
                                                        |0->
                                                            ((updateboard board index cow),{PiecesToPlace=ncount;PiecesOnBoard=pieces+1;MoveState=Moving;TypeofCow=cow})
                                                        |_->((updateboard board index cow),{PiecesToPlace=ncount;PiecesOnBoard=pieces+1;MoveState=Placing;TypeofCow=cow})
                                            |false -> tryPlay index B (list@[A],player) player board
                                  tryPlay nodeIndex board ([],player) player board


let translate item = match item with 
                     |Nothing ->'N'
                     |Light ->'L'
                     |Dark ->'D'
let reversetranslate item = match item with 
                            |'L'->Light 
                            |'D' ->Dark
                            |_->Nothing 
let appendToBoard t b = b (translate t)     
let printBoard list =
                 let rec printBoard list out=
                                   match list with
                                   |[]-> out
                                   |a::rest-> printBoard rest ((translate a)::out)
                 let [a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;q;r;s;t;u;v;w;x] = printBoard list []
                 
                 sprintf "
                            1  2  3     4    5  6  7
                          A %c----------%c----------%c
                            | \        |        / |
                          B |  %c-------%c-------%c  |
                            |  |\      |      /|  |
                            |  | \     |     / |  |
                          C |  |  %c----%c----%c  |  |
                            |  |  |         |  |  |
                          D %c--%c--%c         %c--%c--%c
                            |  |  |         |  |  |
                          E |  |  %c----%c----%c  |  |
                            |  | /     |     \ |  |
                            |  |/      |      \|  |
                          F |  %c-------%c-------%c  |
                            | /        |        \ |
                          G %c----------%c----------%c"  
                    x w v u t s r q p o n m l k j i h g f e d c b a
let rec getUserInput num  = Console.WriteLine("Select Which set of cows you want to use input 1 or 2 \n 1. Dark Cows \n 2. Light Cows");
                            match Console.ReadLine() with 
                            |"1"-> 1
                            |"2" -> 2
                            |_-> Console.ForegroundColor<-ConsoleColor.Red  
                                 Console.WriteLine("Please select a valid Number")
                                 Console.ForegroundColor<-ConsoleColor.White
                                 getUserInput num 
let printPlayOptions options = let rec print options string count=
                                                                 match options with 
                                                                 |[] -> string+"\n Select one of the options to make a move"
                                                                 |A::B ->match count=4 with
                                                                         |true -> print B (string+" \n"+A) 0
                                                                         |false ->print B (string+" "+A) (count+1)
                               print options "" 0
let getAdjacent index= match index with
                       |0->[("A4","A7","A1");("B2","C3","A1");("D1","G1","A1")]
                       |1->[("A1","A7","A4");("B4","C4","A4")]
                       |2->[("A1","A4","A7");("B6","C5","A7");("D7","G7","A7")]
                       |3->[("A1","C3","B2");("B4","B6","B2");("F2","D2","B2")]
                       |4->[("A4","C4","B4");("B2","B6","B4")]
                       |5->[("A7","C5","B6");("B4","B2","B6");("D6","F6","B6")]
                       |6->[("C4","C5","C3");("B2","A1","C3");("D3","E3","C3")]
                       |7->[("C3","C5","C4");("B4","A4","C4")]
                       |8->[("D5","E5","C5");("B6","A7","C5");("C3","C4","C5")]
                       |9->[("D2","D3","D1");("A1","G1","D1")]
                       |10->[("D3","D1","D2");("B2","F2","D2")]
                       |11->[("D1","D2","D3");("C3","E3","D3")]
                       |12->[("C5","E5","D5");("D6","D7","D5")]
                       |13->[("D5","D7","D6");("B6","F6","D6")]
                       |14->[("D5","D6","D7");("A7","G7","D7")]
                       |15->[("E5","E4","E3");("D3","C3","E3");("F2","G1","E3")]
                       |16->[("E3","E5","E4");("F4","G4","E4")]
                       |17->[("E3","E4","E5");("D5","C5","E5");("F6","G7","E5")]
                       |18->[("F4","F6","F2");("D2","B2","F2");("G1","E3","F2")]
                       |19->[("F2","F6","F4");("E4","G4","F4")]
                       |20->[("F4","F2","F6");("G7","E5","F6");("D6","B6","F6")]
                       |21->[("G4","G7","G1");("D1","A1","G1");("F2","E3","G1")]
                       |22->[("G1","G7","G4");("F4","E4","G4")]
                       |23->[("G1","G4","G7");("F6","E5","G7");("D7","A7","G7")]
                       |_ -> []
let GetMills adjacentMills board cow=
   let rec GetMills adjacentM board newList found=
      match adjacentM with
      |[]->newList,found
      |A::B ->
         let a,b,c=A
         let index = ConvertRowColumnToIndex a
         let index1 = ConvertRowColumnToIndex b
         let index2 = ConvertRowColumnToIndex c
         match index <> -1 && index1<> -1&&index2<> -1 with
         |true ->
            let onboard = board|> List.item(index)
            let onboard2 = board|>List.item(index1)
            let onboard3 = board|>List.item(index2)
            //Console.WriteLine(sprintf "%s      %s      %s\n%A   %A   %A" a b c onboard onboard2 onboard3)
                                                                                            
            match onboard=cow && onboard2=cow && onboard3=cow with
            |true ->
               let nList =newList@[({Index=index;Color=cow;InAMill=true},{Index=index1;Color=cow;InAMill=true},{Index=index2;Color=cow;InAMill=true})]
               GetMills B board nList true
            |false ->
               //Console.WriteLine(sprintf "%b    %b    %b" (onboard=cow) (onboard2=cow) (onboard3=cow)) 
               GetMills B board newList found
         |false->
            GetMills B board newList found
   GetMills adjacentMills board [] false                                                                   
let CheckForMills board player index=
    let {PiecesToPlace=count;PiecesOnBoard=pieces;MoveState=mState;TypeofCow=cow}= player
    let adjacent =getAdjacent index
    let pMills = GetMills adjacent board cow
    let pmill,found =pMills
    {PiecesToPlace=count;PiecesOnBoard=pieces;MoveState=mState;TypeofCow=cow},found

let updateShotCowinAMill input mills2 =
                                       let rec updateShotCowinAMill mills2 input nlist=
                                                                                        match mills2 with
                                                                                        |[] -> nlist
                                                                                        |A::B -> let a,b,c=A
                                                                                                 match a.Index<>input with
                                                                                                 |true-> updateShotCowinAMill B input (A::nlist)
                                                                                                 |false -> match b.Index<>input with
                                                                                                           |true-> updateShotCowinAMill B input (A::nlist)
                                                                                                           |false ->(({InAMill=a.InAMill;Color=a.Color;Index=a.Index},{InAMill=b.InAMill;Color=b.Color;Index=b.Index},{InAMill=false;Color=Nothing;Index=c.Index})::nlist)
                                       updateShotCowinAMill mills2 input [] 
let updateoptions input options = options|> List.filter(fun t -> t<>input)
let Addoptions input options = input::options
let GetFoundMillToRemove list =let rec GetFoundMillToRemove list orignalList status=
                                                                                     match list with 
                                                                                     |[]-> (orignalList|>List.map(fun t-> let a,b=t
                                                                                                                          a)),status
                                                                                     |A::B -> let a,b=A 
                                                                                              match b=true with
                                                                                              |true -> GetFoundMillToRemove [] orignalList true
                                                                                              |false -> GetFoundMillToRemove B orignalList status
                               GetFoundMillToRemove list list false
let removeCow input list playerCow =let input=ConvertRowColumnToIndex input
                                    let newBoard = list|>List.mapi(fun indx item-> match indx=input && item.InAMill=false && item.Color <> Nothing && item.Color <> playerCow with //we ensure that the player doesnt try to remove a spot on the board that has no cow placed or remove a cow they have placed
                                                                                   |true -> {InAMill=false;Color=Nothing;Index=input},true
                                                                                   |false ->item,false)|>GetFoundMillToRemove
                                    newBoard    
let removeCowMills input list playerCow =
                               let input=ConvertRowColumnToIndex input
                               let newBoard = list|>List.mapi(fun indx item-> match indx=input && item.Color <> Nothing && item.Color <> playerCow with
                                                                              |true -> {InAMill=false;Color=Nothing;Index=input},true
                                                                              |false ->item,false)|>GetFoundMillToRemove
                               newBoard

let allCowsInMill board player=
    let boardC =
        (board|>List.map(fun t -> reversetranslate (translate t.Color)))
    let check  = 
        board |>List.exists(fun item->   
                                match item.Color=player.TypeofCow with
                                |true->
                                    let _,boolz = CheckForMills boardC player item.Index
                                    match boolz with
                                    |true->false
                                    |false->true
                                |false->false)
    check       
let ShootCow player optionslist player2 board = 
    let {PiecesToPlace=p1;MoveState=state;TypeofCow=cow}=player
    let {PiecesToPlace=p2;PiecesOnBoard=p2on;MoveState=state2;TypeofCow=cow2}=player2 
    let rec SelectMilltoremove board optionsList=
        Console.WriteLine("Please enter a cow position on the board to removed from the opponents cow set")
        let input =Console.ReadLine().ToUpper()
        let converted = ConvertRowColumnToIndex input
        match (String.IsNullOrEmpty(input))=false&&converted <> -1 with
            |true->
                let check = allCowsInMill board player2
                match check with //when both players still have cows that they can still place on the board
                |true -> 
                    let boardUpdated = removeCow input board 
                    let boardUpdated1,didFindValidMill=boardUpdated cow // did findValidMill is a variable that we can use to check if a cow which was not in a mill indeed removed if yes the value will be true else false we ask the user to enter a valid cow to remove
                    match didFindValidMill with
                    |false -> 
                        Console.ForegroundColor<-ConsoleColor.Red
                        Console.WriteLine("Cannot shoot cow in a mill  or place on the board that has no cow placed or remove a cow that you have placed please select a different cow to shoot!")
                        Console.ForegroundColor<-ConsoleColor.White
                        SelectMilltoremove board optionsList
                    |true-> 
                        let updatedOps= Addoptions input optionslist
                        let count = p2on-1
                        match count with
                        |2->(updatedOps,boardUpdated1,player,{PiecesToPlace=p2;PiecesOnBoard=count;MoveState=Loser;TypeofCow=cow2})
                        |3->(updatedOps,boardUpdated1,player,{PiecesToPlace=p2;PiecesOnBoard=count;MoveState=Flying;TypeofCow=cow2})
                        |_->(updatedOps,boardUpdated1,player,{PiecesToPlace=p2;PiecesOnBoard=count;MoveState=state2;TypeofCow=cow2})
                |false->
                    let boardUpdated = removeCowMills input board 
                    let boardUpdated1,didFindValidMill=boardUpdated cow // did findValidMill is a variable that we can use to check if a cow which was not in a mill indeed removed if yes the value will be true else false we ask the user to enter a valid cow to remove
                    let updatedOps= Addoptions input optionslist
                    
                    let count = p2on-1
                    match count with
                    |2->(updatedOps,boardUpdated1,player,{PiecesToPlace=p2;PiecesOnBoard=count;MoveState=Loser;TypeofCow=cow2})
                    |3->(updatedOps,boardUpdated1,player,{PiecesToPlace=p2;PiecesOnBoard=count;MoveState=Flying;TypeofCow=cow2})
                    |_->(updatedOps,boardUpdated1,player,{PiecesToPlace=p2;PiecesOnBoard=count;MoveState=state2;TypeofCow=cow2})
            |false-> 
                Console.ForegroundColor<-ConsoleColor.Red
                Console.WriteLine("Please enter a valid cow to remove!")
                Console.ForegroundColor<-ConsoleColor.White
                SelectMilltoremove board optionsList                    
    SelectMilltoremove board optionslist                                                                  



let map p1 p2 nboard=
        let boardc= (nboard|>List.map(fun item -> (reversetranslate item)))
        let n1 = nboard|>List.mapi(fun index item ->let _,x = CheckForMills boardc p1 index 
                                                    {Index=index;Color=(reversetranslate item);InAMill=x})
        let n1 = nboard|>List.mapi(fun index item ->let _,x = CheckForMills boardc p2 index 
                                                    {Index=index;Color=(reversetranslate item);InAMill=x})
        n1                                                                                                 
    
let PlayerColor color =match color with
                       |Dark ->Console.ForegroundColor<-ConsoleColor.DarkYellow
                       |Light -> Console.ForegroundColor<-ConsoleColor.Blue
                       |_-> Console.ForegroundColor<-ConsoleColor.White
let ValidMove input options =options|>List.exists(fun t -> t=input)
let isPlayerCow index turn board=
    let x = board|>List.item index
    x=turn


let isEmptyAdjacent adjacent board=
     let rec isemptyadjacent adjacent= match adjacent with
                                                        |[]->false
                                                        |A::B->
                                                            let a,b,_=A
                                                            let index = ConvertRowColumnToIndex a
                                                            let index1 = ConvertRowColumnToIndex b
                                                            let onboard = board|> List.item(index)
                                                            let onboard2 = board|>List.item(index1)
                                                            match onboard=Nothing||onboard2=Nothing with
                                                                |true->true
                                                                |false-> isemptyadjacent B
     isemptyadjacent adjacent

let PlayerTurn optionslist player otherplayer board turn= 
    PlayerColor player.TypeofCow
    Console.WriteLine("Player {0}'s turn",match turn with 
                                          |P1-> 1
                                          |P2->2)
    PlayerColor Nothing
    match player.MoveState with
    |Placing->    
        let input = Console.ReadLine().ToUpper()
        let x = (ValidMove (input.ToUpper()) optionslist)
        let index = ((ConvertRowColumnToIndex input))
        match (index<>(-1)) && x with
        |false -> Console.ForegroundColor<-ConsoleColor.Red
                  Console.WriteLine("Please select a valid Position")
                  Console.ForegroundColor<-ConsoleColor.White
                  (player,otherplayer,board,optionslist,turn)
        |true ->let newBoard= MakeMove input board player
                let nboard,newplayer=newBoard
                let updatedPlayer= CheckForMills nboard newplayer index
                let updatedoptions= updateoptions input optionslist
                let updatePlayer,status=updatedPlayer
                match status with
                |false ->(updatePlayer,otherplayer,nboard,updatedoptions,match turn with
                                                                         |P1-> P2
                                                                         |P2->P1)
                |true-> let shotCow = ShootCow updatePlayer updatedoptions otherplayer (map updatePlayer otherplayer (nboard|>List.map(fun item -> (translate item))))
                        let options,newBoard,p1,p2=shotCow
                        let nboard = newBoard|>List.map(fun t -> reversetranslate (translate t.Color))
                        (p1,p2,nboard,options,match turn with
                                              |P1-> P2
                                              |P2->P1)
    |Moving->
        let {PiecesToPlace=p1;PiecesOnBoard=p1on;MoveState=state;TypeofCow=cow}=player
        let rec moveCow player options board=
            Console.WriteLine("Choose Cow to Move")
            let input = Console.ReadLine().ToUpper()
            let from = (ConvertRowColumnToIndex input)
            let adjacent = getAdjacent from                                                         
            match from<>(-1)&&isEmptyAdjacent adjacent board&&(isPlayerCow from cow board) with
                |false->
                    Console.WriteLine("A cow of yours must be chosen and have an empty adjacent position")
                    moveCow player options board
                |true->
                    let rec removecow player options board=
                        Console.WriteLine("Choose where to move cow")
                        let input2 = Console.ReadLine().ToUpper()
                        let too = (ConvertRowColumnToIndex input2)
                        let b = too<>(-1)
                        let c = input2<>input
                        let d =(adjacent|>List.exists (fun x->
                                                            let a,b,_ =x
                                                            a=input2||b=input2 ))
                        match b&& isPlayerCow too Nothing board&&c&&d with
                            |false->Console.WriteLine("Must choose empty adjacent Position")
                                    removecow player options board
                            |true-> let nboard = board |> List.mapi (fun index item ->match index=from||index=too with 
                                                                                        |true  -> match index=from with
                                                                                                    |true-> Nothing
                                                                                                    |false-> player.TypeofCow
                                                                                        |false -> item)
                                    let options= Addoptions input options
                                    let options= updateoptions input2 options
                                    let nplayer = {PiecesToPlace=p1;PiecesOnBoard=p1on;MoveState=state;TypeofCow=cow}
                                    let updatedPlayer= CheckForMills nboard nplayer too
                                    let updatePlayer,status=updatedPlayer
                                    match status with
                                    |false ->(updatePlayer,otherplayer,nboard,options,match turn with
                                                                         |P1-> P2
                                                                         |P2->P1)
                                    |true-> let shotCow = ShootCow updatePlayer options otherplayer (map updatePlayer otherplayer (nboard|>List.map(fun item -> (translate item))))
                                            let options,newBoard,p1,p2=shotCow
                                            let nboard = newBoard|>List.map(fun t -> reversetranslate (translate t.Color))
                                            (p1,p2,nboard,options,match turn with
                                                                  |P1-> P2
                                                                  |P2->P1)
                    removecow player options board
        moveCow player optionslist board
    |Flying->
            let rec moveCow player options board=
                Console.WriteLine("Choose Cow to Fly")
                let input = Console.ReadLine().ToUpper()
                let from = (ConvertRowColumnToIndex input)
                                                    
                                                                                     
                match (isPlayerCow from player.TypeofCow board)&&from<>(-1) with
                    |false->
                        Console.WriteLine("A cow of yours must be chosen")
                        moveCow player options board
                    |true->
                        let rec removecow player options board=
                            Console.WriteLine("Choose where to fly cow")
                            let input2 = Console.ReadLine().ToUpper()
                            let too = (ConvertRowColumnToIndex input)
                            match too<>(-1)&&(isPlayerCow too Nothing board)&&input2<>input with
                                |false->Console.WriteLine("Must choose empty adjacent Position")
                                        removecow player options board
                                |true-> let nboard = board |> List.mapi (fun index item ->match index=from||index=too with 
                                                                                            |true  -> match index=from with
                                                                                                        |true-> Nothing
                                                                                                        |false-> player.TypeofCow
                                                                                            |false -> item)
                                        let options= Addoptions input options
                                        let options= updateoptions input2 options
                                        (player,otherplayer,nboard,options,match turn with
                                                  |P1-> P2
                                                  |P2->P1)
                        removecow player options board
            moveCow player optionslist board
        

let Game = 
    let board = CreateBoard
    let input =getUserInput 0
    let p1 =initPlayer input 
    let p2 =initPlayer (match input =1 with
                        |true ->2
                        |false->1)
    let rec Game p1 p2 board optionslist turn= 
            //Console.Clear()
            let prnt =printBoard board
            Console.WriteLine(prnt)
            let printO =printPlayOptions optionslist
            Console.WriteLine(printO)
            
            match p1.MoveState=Loser||p2.MoveState=Loser with
            |true->Console.Write("A winner has been decided. You know who you are... you know... it'll keep up at night.")
                   Console.ReadLine()
                   ()
            |false->
                match turn with
                |P1-> let data =PlayerTurn optionslist p1 p2 board turn
                      let p1,p2,nboard,options,turn=data
                      Game p1 p2 nboard options turn
                |_->  let data =PlayerTurn optionslist p2 p1 board turn
                      let p2,p1,nboard,options,turn=data
                      Game p1 p2 nboard options turn

    Game p1 p2 board Places P1 
    ()   
[<EntryPoint>]
let main argv = 
  Game
  0
  //To Add to Game
  //Add checking for winner
//Flying the cows
//When a player has onl
//3y three cows remaining, desperate measures are called for. This player's cows are allowed to "fly" to any empty intersection, not just adjacent ones.
//If one player has three cows and the other player has more than three cows, only the player with three cows is allowed to fly
//Finishing the game
//A win occurs if one opponent has no moves
//A win occurs if a player has just two cows
//If either player has only three cows and neither player shoots a cow within ten moves, the game is drawn
//If one person cheats, then the other one wins by default

//To fix: options not appearing as they should after being removed by shooting
//indentation 
//merging from two game functions into one
//a1L d1D g1L d2D a7L d3D elimating a1L  a4L b5L causes a mill