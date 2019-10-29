module Main where

--import Tui
import Checkers
 
main :: IO ()
main = human apply_move initialGameState

moves::GameState -> [Move]
moves s 
  | jump /= [] = jump
  | otherwise = simple
  where 
    jump = jump_moves s 
    simple = simple_moves s


apply_move :: Move -> GameState -> GameState
apply_move [] s 
  | (allRed == []) = s {_status = GameOver, _message = "Black won!"} 
  | (allBlack == []) = s {_status = GameOver, _message = "Red won!"}
    where
      allRed = (_redPieces s) ++ (_redKings s) 
      allBlack = (_blackPieces s) ++ (_blackKings s)
apply_move m s
  | (member m validMoves) == False = s
  | otherwise = case (isSimpleMove) of 
      True -> make_simple_move m s
      False -> (make_jump_move m s) {_status = change_player s 
                                     ,_message = case (_status s) of 
                                            Red -> "Black Turn"
                                            Black -> "Red Turn"  
                                    }
    where 
      start = m !! 0 
      end = m !! 1
      validMoves = moves s
      isSimpleMove = ((jump_moves s) == [])
      allRed = (_redPieces s) ++ (_redKings s) 
      allBlack = (_blackPieces s) ++ (_blackKings s)


make_simple_move :: Move -> GameState -> GameState
make_simple_move [] s = s
make_simple_move [start,end] s
  | (_status s) == Red = case (memberc start (_redKings s)) of 
                      True -> s {_redKings = replace start end (_redKings s)
                                ,_status = change_player s
                                ,_message = "Black Turn"}
                      False -> case (memberc end red_end_line) of 
                                True -> s {_redPieces = delete' start (_redPieces s)
                                          ,_redKings = end : (_redKings s)
                                          ,_status = change_player s
                                          ,_message = "Black Turn"}                              
                                False -> s {_redPieces = replace start end (_redPieces s)
                                            ,_status = change_player s
                                            ,_message = "Black Turn"}
  | (_status s) == Black = case (memberc start (_blackKings s)) of 
                      True -> s {_blackKings = replace start end (_blackKings s)
                                ,_status = change_player s
                                ,_message = "Red Turn"}
                      False -> case (memberc end black_end_line) of 
                                True -> s {_blackPieces = delete' start (_blackPieces s)
                                          ,_blackKings = end : (_blackKings s)
                                          ,_status = change_player s
                                          ,_message = "Red Turn"}                              
                                False -> s {_blackPieces = replace start end (_blackPieces s)
                                            ,_status = change_player s
                                            ,_message = "Red Turn"}
    where 
      red_end_line = [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0)]
      black_end_line = [(0,7), (1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7)]

make_jump_move :: Move -> GameState -> GameState
make_jump_move (x:[]) s = s
make_jump_move (start:(next:rest)) s 
  |(_status s) == Red = case (memberc start (_redKings s)) of 
                True -> make_jump_move (next:rest) (s {_blackKings = delete' (jumped start next) (_blackKings s)
                                                       ,_blackPieces = delete' (jumped start next) (_blackPieces s)
                                                       ,_redKings = next : (delete' start (_redKings s))
                                                      })
                False -> case (memberc next red_end_line) of 
                      True -> make_jump_move (next:rest) (s {_blackKings = delete' (jumped start next) (_blackKings s)
                                                            ,_blackPieces = delete' (jumped start next) (_blackPieces s)
                                                            ,_redPieces = delete' start (_redPieces s)
                                                            ,_redKings = next : (_redKings s)
                                                            })
                      False ->  make_jump_move (next:rest) (s {_blackKings = delete' (jumped start next) (_blackKings s)
                                                              ,_blackPieces = delete' (jumped start next) (_blackPieces s)
                                                              ,_redPieces = next : (delete' start (_redPieces s))
                                                              })
  |(_status s) == Black = case (memberc start (_blackKings s)) of 
                True -> make_jump_move (next:rest) (s {_redKings = delete' (jumped start next) (_redKings s)
                                                        ,_redPieces = delete' (jumped start next) (_redPieces s)
                                                        ,_blackKings = next : (delete' start (_blackKings s))
                                                      })
                False -> case (memberc next black_end_line) of 
                      True -> make_jump_move (next:rest) (s {_redKings = delete' (jumped start next) (_redKings s)
                                                            ,_redPieces = delete' (jumped start next) (_redPieces s)
                                                            ,_blackPieces = delete' start (_blackPieces s)
                                                            ,_blackKings = next : (_blackKings s)
                                                            })
                      False ->  make_jump_move (next:rest) (s {_redKings = delete' (jumped start next) (_redKings s)
                                                              ,_redPieces = delete' (jumped start next) (_redPieces s)
                                                              ,_blackPieces = next : (delete' start (_blackPieces s))
                                                              })
  where
    red_end_line = [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0)]
    black_end_line = [(0,7), (1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7)]

jumped :: Coord -> Coord -> Coord 
jumped start next = (x,y)
  where
    x = ((fst start) + (fst next)) `div` 2
    y = ((snd start) + (snd next)) `div` 2
     
simple_moves :: GameState -> [Move]
simple_moves s = case (_status s) of
    Red -> (simple_king (_redKings s)) ++ (simple_piece_red (_redPieces s))
    Black -> (simple_king (_blackKings s)) ++ (simple_piece_black (_blackPieces s))
    _ -> []
  where 
   simple_piece_red sp = [[(x,y),(x',y')]| (x,y) <- sp, (x',y') <- [(x+1,y-1),(x-1,y-1)], onboard (x',y') && (notOccupied (x',y') s)]
   simple_piece_black sp = [[(x,y),(x',y')]| (x,y) <- sp, (x',y') <- [(x-1,y+1),(x+1,y+1)], onboard (x',y') && (notOccupied (x',y') s)]
   simple_king sk = [[(x,y),(x',y')]| (x,y) <- sk, (x',y') <- [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)], onboard (x',y') && (notOccupied (x',y') s)]

jump_moves:: GameState -> [Move]
jump_moves s = (jumpPiece s) ++ (jumpKing s) 

jumpPiece :: GameState -> [Move]
jumpPiece s 
    | (_status s) == Red = [(x,y):ys | (x,y) <- (_redPieces s), ys <- jumpRedPiece' (x,y) [] (x,y) s]
    | (_status s) == Black = [(x,y):ys | (x,y) <- (_blackPieces s), ys <- jumpBlackPiece' (x,y) [] (x,y) s]
    | otherwise = []

jumpRedPiece' :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpRedPiece' start rem (x,y) s = [ (x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                                , not (memberc (x',y') rem) && (opponent_occupied (x',y') s) && (start == (x'',y'') || notOccupied (x'',y'') s) && onboard (x'',y'')
                                , ys <- case (memberc (x'', y'') red_end_line) of 
                                    True -> jump_over (jumpKing' start ((x',y'):rem) (x'',y'') s)
                                    False -> jump_over (jumpRedPiece' start ((x',y'):rem) (x'',y'') s)
                                ]
  where 
    red_end_line = [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0)]                           
                                
jumpBlackPiece' :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpBlackPiece' start rem (x,y) s = [ (x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]
                                , not (memberc (x',y') rem) && (opponent_occupied (x',y') s) && (start == (x'',y'') || notOccupied (x'',y'') s) && onboard (x'',y'')
                                , ys <- case (memberc (x'', y'') black_end_line) of 
                                    True -> jump_over (jumpKing' start ((x',y'):rem) (x'',y'') s)
                                    False -> jump_over (jumpBlackPiece' start ((x',y'):rem) (x'',y'') s) 
                                ]
  where 
    black_end_line = [(0,7), (1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7)] 

jumpKing :: GameState -> [Move]
jumpKing s 
    | (_status s) == Red = [(x,y):ys | (x,y) <- (_redKings s), ys <- jumpKing' (x,y) [] (x,y) s]
    | (_status s) == Black = [(x,y):ys | (x,y) <- (_blackKings s), ys <- jumpKing' (x,y) [] (x,y) s]
    | otherwise = []

jumpKing' :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpKing' start rem (x,y) s = [ (x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                              , not (memberc (x',y') rem) && (opponent_occupied (x',y') s) && (start == (x'',y'') || notOccupied (x'',y'') s) && onboard (x'',y'')
                              , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'') s)]
           
jump_over :: [Move] -> [Move]
jump_over [] = [[]]
jump_over z = z

change_player :: GameState -> Status 
change_player s 
  | (_status s) == Red = Black 
  | (_status s) == Black = Red 
  | otherwise = GameOver

notOccupied :: Coord -> GameState -> Bool
notOccupied p s 
    | memberc p allPiece = False 
    | otherwise = True
    where 
    allPiece = (_redPieces s) ++ (_redKings s) ++ (_blackPieces s) ++ (_blackKings s)

opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied p s = case (_status s) of 
    Red -> memberc p allBlack
    Black -> memberc p allRed
  where 
    allRed = (_redPieces s) ++ (_redKings s) 
    allBlack = (_blackPieces s) ++ (_blackKings s)

onboard :: Coord -> Bool
onboard a 
    | (fst a >= 0) && (fst a <= 7) && (snd a >= 0) && (snd a <= 7) = True 
    | otherwise = False 

member :: Move -> [Move] -> Bool
member x [] = False 
member x (b:bs) 
    | x == b = True 
    | otherwise = member x bs

memberc :: Coord -> [Coord] -> Bool
memberc x [] = False 
memberc x (a:as) 
    | x == a = True 
    | otherwise = memberc x as

replace :: Coord -> Coord -> [Coord] -> [Coord] 
replace start end [] = []
replace start end (a:as) 
    | ((memberc start (a:as)) == False) = (a:as)
    | a == start = end:as 
    | otherwise = a : (replace start end as)

delete' :: Coord -> [Coord] -> [Coord] 
delete' d [] = []
delete' d (a:as) 
    | ((memberc d (a:as)) == False) = (a:as)
    | d == a = as 
    | otherwise = a : (delete' d as)

memberl :: [Move] -> [Move] -> Bool 
memberl x [] = False 
memberl (a:as) (bs) = (member a bs) && memberl as bs