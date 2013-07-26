--
--
-- DIRTY HASKELL CODE AHEAD...
--
--    QCAR
--
--
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import System.IO


data Block = B
    { xpos :: Int
    , ypos :: Int
    , width :: Int
    , height :: Int
    , bitmap :: [[Bool]]
    }
    deriving (Eq, Ord)

type Board = M.Map Int Block

collide :: Block -> Block -> Bool
collide (B x1 y1 w1 h1 bmp1) (B x2 y2 w2 h2 bmp2) =
    overlap x1 w1 x2 w2 && overlap y1 h1 y2 h2 &&
    foldand (sub bmp1 x1 y1 x2 y2 h2 w2)
            (sub bmp2 x2 y2 x1 y1 h1 w1)
  where
    overlap x1 l1 x2 l2 =
        (x2 >= x1 && x2 < x1 + l1) ||
        (x1 >= x2 && x1 < x2 + l2)
    foldand as bs =
        or [ a && b | (a, b) <- concat $ zipWith zip as bs ]
    sub bmp x1 y1 x2 y2 h2 w2 = do
        { (l, y) <- zip bmp [y1..]
        ; guard (y >= y2 && y < y2 + h2)
        ; return $ do
            { (b, x) <- zip l [x1..]
            ; guard (x >= x2 && x < x2 + w2)
            ; return b
            }
        }

type Vec = (Int, Int)

move :: Vec -> Block -> Block
move (dx, dy) b = b { xpos = xpos b + dx, ypos = ypos b + dy }

data Game = Game
    { board :: Board
    , final :: Vec
    , bw :: Int
    , bh :: Int
    , bblocks :: [Block]
    }

step :: Game -> S.Set Board -> Board -> [(Board, (Int, Vec))]
step g seen b = do
    { (i, bi) <- M.assocs b
    ; delta <- [(-1,0), (1,0), (0,-1), (0,1)]
    ; let nbi = move delta bi
    ; guard . not . invalid i $ nbi
    ; return (M.insert i nbi b, (i, delta))
    }
  where
    invalid i bi@(B bx by w h _) =
        bx < 0 || bx + w > bw g ||
        by < 0 || by + h > bh g ||
        S.member (M.insert i bi b) seen ||
        collides i bi
    collides i bi =
        or [ collide bi bj | (j, bj) <- M.assocs b, j /= i ] ||
        or [ collide bi b | b <- bblocks g ]

bffind :: Game -> Maybe [(Int, Vec)]
bffind g = rec S.empty [(board g, [])] 0
  where
    rec seen ((b, p) : q) n
        | isfinal b = Just $ reverse p
        | otherwise =
            let bs = step g seen b
                seen' = foldr S.insert seen (map fst bs) in
            rec seen' (q ++ [(b, d:p) | (b, d) <- bs]) (n+1)
    rec _ [] _ = Nothing
    isfinal b =
        case M.lookup 1 b of
            Nothing -> error "damn it!"
            Just b1 -> (xpos b1, ypos b1) == final g

parseGame :: [String] -> Game
parseGame (final : ltop : grid) =
    Game bs (read final) (length ltop) (length grid) bos
  where
    (bs, bos, _) = foldl (\a -> line a . segs) (M.empty, [], 0) grid
    line (bs, bos, y) ((x, w, c) : ss)
        | Just n <- elemIndex c "0123456789" =
            line (M.alter (altr x y w) n bs, bos, y) ss
        | otherwise =
            line (bs, bos, y) ss
    line (bs, bos, y) [] = (bs, bos, y+1)
    segs l = zipWith (\x s -> (x, length s, head s)) xs ss
      where ss = group l
            xs = map (length . concat) (inits ss)
    altr x y w (Just (B bx by bw bh bbmp)) =
        let nx = min x bx
            nw = max (x-nx + w) (bx-nx + bw)
            nh = if y == by+bh then bh+1 else bh
            bmp = map (extend bx bw nx nw) bbmp
               ++ (if nh /= bh then [replicate nw False] else [])
            ll = zipWith (||)
                (extend x w nx nw (replicate w True))
                (last bmp) in
        Just (B nx by nw nh (init bmp ++ [ll]))
    altr x y w Nothing =
        Just (B x y w 1 [replicate w True])
    extend lx lw x w l =
        let db = lx - x
            de = w - (db + lw) in
        replicate db False ++ l ++ replicate de False

showActions :: Game -> [[(Int, Vec)]] -> IO ()
showActions g = foldM_ perform (board g)
  where
    perform b as =
        let scr = showBoard b ++ "\n"
            nb = foldl (\b (i, d) -> M.adjust (move d) i b) b as in
        putStr scr >> return nb
    showBoard b = do
        { y <- [0..bh g-1]
        ; x <- [0..bw g-1]
        ; let b1 = B x y 1 1 [[True]]
        ; let bo = fmap fst $ find (\(_, b) -> collide b1 b) (M.assocs b)
        ; let nl = if x == bw g-1 then "\n" else ""
        ; case bo of
            Nothing -> blk 7 ++ nl
            Just b -> blk b ++ nl
        }
    blk n = map (\n -> "\x1b[" ++ show (40+n) ++ "m \x1b[0m") [0..7] !! n

main :: IO ()
main = do
    { lines <- fmap (filter (/= "") . lines) (hGetContents stdin)
    ; let g = parseGame lines
    ; case bffind g of
        Nothing -> putStrLn "No solutions found!"
        Just p -> showActions g (map (:[]) p)
    }
