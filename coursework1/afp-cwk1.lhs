> import Data.List
> import Data.Char
> import System.Random

--------------------------------------------------------------------------------

G52AFP Coursework 1 - Noughts and Crosses

Barnabas Forgo, Roshan Hunjan
psybf@nottingham.ac.uk, psyrh3@nottingham.ac.uk

--------------------------------------------------------------------------------

The 3x3 board is represented as a list of list of player values:

> type Board                    =  [[Player]]


This can be changed to play with a different board size, but then the key mapping
for the human player wouldn't provide mapping for moves greater than 8, use the
human' function instead.

> boardSize                     =  4

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a space on the board that is not yet occupied:

> data Player                   =  Nought | Blank | Cross
>                                  deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard                     :: Board -> IO ()
> showBoard                     =  putStrLn . ("\n" ++) . unlines . concat . interleave bar . map showRow
>                                  where
>                                     bar = [replicate (boardSize * 6 -1) '-']
>
> showRow                       :: [Player] -> [String]
> showRow                       =  beside . interleave bar . map showPlayer
>                                  where
>                                     beside = foldr1 (zipWith (++))
>                                     bar    = replicate 5 "|"
>
> showPlayer                    :: Player -> [String]
> showPlayer Nought             =  ["     ", " +-+ ", " | | ", " +-+ ", "     "]
> showPlayer Blank              =  ["     ", "     ", "     ", "     ", "     "]
> showPlayer Cross              =  ["     ", " \\ / ", "  X  ", " / \\ ", "     "]
>
> interleave                    :: a -> [a] -> [a]
> interleave x []               =  []
> interleave x [y]              =  [y]
> interleave x (y:ys)           =  y : x : interleave x ys



--------------------------------------------------------------------------------

Just a blank board for starting the game

> blank                         :: Board
> blank                         =  replicate boardSize $ replicate boardSize Blank


GAME RULES & HELPER FUNCTIONS --------------------------------------------------


*** DEFIINING THE BOARD

isFull when applied on a Board returns whether the Board is full.

> isFull                        :: Board -> Bool
> isFull                        =  not . elem Blank . concat


rows when applied on a Board just returns it, as the Board is defined to be a list of rows of Players.

> rows                          :: Board -> [[Player]]
> rows                          =  id


cols transposes the Board to retorn a list of columns containing Player.

> cols                          :: Board -> [[Player]]
> cols                          =  transpose


diagonals returns the diagonals in the Board.

> diagonals                     :: Board -> [[Player]]
> diagonals board               =  [
>                                   [board !! x !! y | (x,y) <- xys, x == y],
>                                   [board !! x !! y | (x,y) <- xys, x + y == boardSize-1]
>                                  ]
>                                  where xys = [(x,y) | x <- [0..boardSize-1], y <- [0..boardSize-1]]


*** TAKING TURNS

whoseTurn returns the Player who has to make a turn next or Blank if th Board is full.

> whoseTurn                     :: Board -> Player
> whoseTurn board               |  isFull board = Blank
>                               |  countPlayer Cross board > countPlayer Nought board = Nought
>                               |  otherwise = Cross


countPlayer returns how many moves has one Player made.

> countPlayer                   :: Player -> Board -> Int
> countPlayer player            =  length . filter (== player) . concat



*** MAKING MOVES

Some syntactic sugar for making moves.

> type Move = Int


isFree returns whether a position on the board is free.

> isFree                        :: Move -> Board -> Bool
> isFree pos board              =  concat board !! pos == Blank


isValidMove checks if pos is in the indexing range of the board
and checks if the pos is free on the Board

> isValidMove                   :: Move -> Board -> Bool
> isValidMove pos board         =  0 <= pos && pos <= (boardSize^2 -1) && isFree pos board


move is a state transformer, that places the given Player to the position given by Move.
The logic is the following:
if the move is valid
  concat the Board to [Player] instead of [[Player]],
  replace the Player at pos, using replaceAt,
  revert the concatenation using tacnoc (reverse of concat)
otherwise
  return the original board.


> move                          :: Move -> Player -> Board -> Board
> move pos player board         |  isValidMove pos board = tacnoc $ replaceAt pos player $ concat board
>                               |  otherwise = board
>                                  where replaceAt pos newElement xs = take pos xs ++ [newElement] ++ drop (pos + 1) xs
>                                        tacnoc []    = []
>                                        tacnoc board = (take boardSize board) : (tacnoc (drop boardSize board))


*** WINNNING CONDITIONS

winSequences ties together rows, cols and diagonals. In this list of list of Players
if any one list contains only one type of Player, then that Player has won.

> winSequences                  :: Board -> [[Player]]
> winSequences board            =  rows board ++ cols board ++ diagonals board


hasWon checks if the Player passed in has won the game on the Board,
using the helper function allThis.

allThis checks whether all the elements in the list match 'this'.

> hasWon                        :: Player -> Board -> Bool
> hasWon player                 =  any (allThis player) . winSequences
>   where allThis this xs       =  all (== this) xs


anyoneWon checks whether any player has won the game.

> anyoneWon                     :: Board -> Bool
> anyoneWon board               =  hasWon Cross board || hasWon Nought board


whoWon takes a Board and returns the Player who has won or Blank if no one.

> whoWon                        :: Board -> Player
> whoWon board
>   | hasWon Cross board        =  Cross
>   | hasWon Nought board       =  Nought
>   | otherwise                 =  Blank





AI - MINIMAX ALGORITHM ---------------------------------------------------------


*** GAMETREE GENERATION

New data type for representing a gametree. Each Node has a label (a)
and a list of children Nodes.

> data Tree a                   = Node a [Tree a] deriving (Show)


Syntactic sugar for a GameTree where we don't yet know the winning player.

> type GameTree                 = Tree (Move, Board)


Syntactic sugar for a GameTree where we know the winning player.

> type MinimaxGameTree          = Tree (Player, Move, Board)


generateGameTree generates the root of the gametree. Move is -1 because
we don't need to know what move was taken to get to this state, but keeping track
of moves is needed later on, for the AI to make a move.

> generateGameTree              :: Board -> GameTree
> generateGameTree b            =  Node (-1, b) (generateGameTrees (moves b))

> generateGameTree'              :: Board -> GameTree
> generateGameTree' b            =  Node (-1, b) (generateGameTrees' 17 (moves b))


generateGameTrees recursively generates the children of a Node in a GameTree
from all the possible moves.

> generateGameTrees             :: [(Move, Board)] -> [GameTree]
> generateGameTrees []          =  []
> generateGameTrees ((m,b):mbs) =  Node (m,b) (generateGameTrees $ moves b) : generateGameTrees mbs

> generateGameTrees'             :: Int -> [(Move, Board)] -> [GameTree]
> generateGameTrees' 0 _          =  []
> generateGameTrees' _ []          =  []
> generateGameTrees' l ((m,b):mbs)  =  Node (m,b) (generateGameTrees' (l-1) (moves b)) : generateGameTrees' (l-1) mbs


moves generates all the possible moves from a Board State.

The list comprehension explained:
not $ anyoneWon board
    -> Don't generate any new states if there is a winner on the board.

x <- [0..8], isFree x board,
    -> Generate all moves where the position is free.

newBoard <- [move x (whoseTurn board) board]]
    -> Generate newBoard by making the moves generated above.

> moves                         :: Board -> [(Move, Board)]
> moves board                   =  [(x, newBoard) | not $ anyoneWon board,
>                                                   x <- [0..(boardSize^2 -1)], isFree x board,
>                                                   newBoard <- [move x (whoseTurn board) board]]


*** MINIMAX ALGORITHM

minimax relabels the whole tree, so that each Node is labeled with the player
who would win in that state (given that the player plays perfectly).
The way it does that is players have an ordering (Nought < Blank < Cross)
so minimum [Nought, Blank, Cross] = Nought and maximum [Nought, Blank, Cross] = Cross.
Then each Node is labelled recursively so that the node itself is labeled with
the minimum or maximum player of its children, and on each level of the tree
minimum and maximum is alternated, starting with minimum if the top level player
is Nought, maximum otherwise.


Leaf Node, label it with the winner.

minimax (Node (m,b) []) = (Node (whoWon b, m, b) [])


Node with children, convert it to a relabeled Node where the new label contains
the winner in that state.

minimax (Node (m,b) children) = (Node (minOrMax (whoseTurn b) newChildren, m, b) newChildren)

        newChildren is generated by calling minimax on the original Node's children
  where newChildren     = [minimax child | child <- children]

        This returns the minimum or maximum of a list of Players.
        minOrMax :: Player -> [MinimaxGameTree] -> Player
        minOrMax Nought = minimum . players
        minOrMax _      = maximum . players

        List of Players extracted from a list of GameTrees.
        players ts = [player | (Node (player, _, _) _) <- ts]

> minimax                       :: GameTree -> MinimaxGameTree
> minimax (Node (m,b) [])       =  Node (whoWon b, m, b) []
> minimax (Node (m,b) children) =  Node (minOrMax (whoseTurn b) newChildren, m, b) newChildren
>   where newChildren           =  [minimax child | child <- children]
>         minOrMax              :: Player -> [MinimaxGameTree] -> Player
>         minOrMax Nought       =  minimum . players
>         minOrMax _            =  maximum . players
>         players ts            =  [player | (Node (player, _, _) _) <- ts]



minimaxMove determines what Move to make when applied to the board using the minimax
algorithm. How it works is it generates the full gametree, applies minimax on it,
then it chooses the first move that matches the label of the root.

sameLabels generates a list of children that has the same label as the passed in Node.
getLabel extracts the Player label from a Node.
getMove extracts the Move from a Node.

> minimaxMove                   :: Board -> Move
> minimaxMove                   =  getMove . head . sameLabels . minimax . generateGameTree
>                                  where sameLabels (Node (p, _, _) ts) = [t | t <- ts, p == getLabel t]
>                                        getLabel (Node (p, _, _) _) = p
>                                        getMove (Node (_, m, _) _) = m

minimaxPlayer is just an IO monad wrapper for the AI so that it matches the
human player's type signature.

> minimaxPlayer                 :: Board -> IO Move
> minimaxPlayer board           =  do discard <- putStrLn "Thinking..."
>                                     return $ minimaxMove board





GAME LOOP, HUMAN PLAYER AND IO -------------------------------------------------


turn takes two functions (that transform a board to an IO monadic move),
the current board state and it loops until the game is finished or someone has won.
The two functions that are passed in could be the AI or the human player in any order,
so that you can play the game human vs human, AI vs AI or AI vs human modes.

anyoneWon board
  -> The game is finished, print who is the winner.

isFull board
  -> No winners, but the Board is full, it's a draw.

otherwise
  -> make a player make a move

  newMove <- currentPlayer board
    -> newMove is drawn from applying one of the player functions,
       the function to use is determined by currentPlayer
  let newBoard = move newMove (whoseTurn board) board
    -> The move is applied to the board. Note that the function 'move'
       checks for validity, here we are only checking to inform the player.
  showBoard newBoard
    -> Show the new board.
  turn player1 player2 newBoard
    -> Go to the next turn.

> turn                          :: (Board -> IO Move) -> (Board -> IO Move) -> Board -> IO ()
> turn player1 player2 board
>    | anyoneWon board          = putStrLn $ show (whoWon board) ++ " has won the game."
>    | isFull board             = putStrLn "It's a draw."
>    | otherwise                = do newMove <- currentPlayer board
>                                    let newBoard = move newMove (whoseTurn board) board
>                                    if newBoard == board then
>                                        putStrLn "Invalid move, try again." >> help
>                                    else
>                                        showBoard newBoard
>                                    turn player1 player2 newBoard
>                                        where currentPlayer = case whoseTurn board of
>                                                                   Cross     -> player1
>                                                                   otherwise -> player2


human just reads one character from stdio and maps it to a move. Here the board
parameter is just a placeholder, so that it matches the AI's type signature.
The 'otherwise' case produces an invalid move but the function 'move' takes care of that.

> human                         :: Board -> IO Move
> human board                   =  do putStr "Make your move: "
>                                     s <- getLine
>                                     return $ charToMove (s!!0)
>                                     where charToMove 'q' =  0
>                                           charToMove 'w' =  1
>                                           charToMove 'e' =  2
>                                           charToMove 'a' =  3
>                                           charToMove 's' =  4
>                                           charToMove 'd' =  5
>                                           charToMove 'z' =  6
>                                           charToMove 'x' =  7
>                                           charToMove 'c' =  8
>                                           charToMove  _  = -1


main is the entry point to the program, it explains the game rules
and starts the first turn in human vs AI mode.

> main                          :: IO ()
> main                          =  do help
>                                     showBoard blank
>                                     turn human minimaxPlayer blank

help just displays information about how to play the game. Shown at the start
and when an invalid move is made.

> help                          :: IO ()
> help                          =  do putStrLn "\nYou can make a move by typing one character and hitting enter."
>                                     putStrLn "The positions on the board are mapped to the keyboard like this:"
>                                     putStrLn "\nq|w|e"
>                                     putStrLn "-----"
>                                     putStrLn "a|s|d"
>                                     putStrLn "-----"
>                                     putStrLn "z|x|c\n"





EXTRA BITS ---------------------------------------------------------------------


human' is a human player where a move can be made by entering the Int
corresponding to a Move. Handy for smaller or bigger than default boards.

> human'                        :: Board -> IO Move
> human' board                  =  do putStr "Make your move: "
>                                     s <- getLine
>                                     return $ read s


randomPlayer just chooses a move randomly, so you can win against someone.

> randomPlayer                  :: Board -> IO Move
> randomPlayer board            =  do m <- randomRIO(0,(boardSize^2 -1))
>                                     if isFree m board then
>                                        return m
>                                     else
>                                        randomPlayer board
