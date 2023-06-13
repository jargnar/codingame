import System.IO
import Control.Monad

movex :: Int -> Int -> (String, Int)
movex a x 
    | a < x = ("E", a+1)
    | a > x = ("W", a-1)
    | otherwise = ("", a)

movey :: Int -> Int -> (String, Int)
movey b y
    | b < y = ("S", b+1)
    | b > y = ("N", b-1)
    | otherwise = ("", b)

moveThor :: Int -> Int -> Int -> Int -> IO ()
moveThor x lightx y lighty = do
    let (xdir, newX) = movex x lightx
        (ydir, newY) = movey y lighty
    putStrLn (ydir ++ xdir)
    unless (null xdir && null ydir && newX == lightx && newY == lighty) $ do
        moveThor newX lightx newY lighty

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
    
    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    
    -- game loop
    forever $ do
        input_line <- getLine
        let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.
        
        -- hPutStrLn stderr "Debug messages..."

        -- A single line providing the move to be made: N NE E SE S SW W or NW        
        moveThor initialtx lightx initialty lighty
