module Risou ( Sudoku
             , solve'
             , solve
             ) where

import Data.List ( transpose
                 , findIndex
                 )
import Data.Maybe ( isNothing
                  )

type Matrix a = [[a]]

row :: Int -> Matrix a -> [a]
row _ [] = []
row 0 (xs : xss) = xs
row n (xs : xss) = row (n-1) xss

column :: Int -> Matrix a -> [a]
column n = map (!! n)

subMatrix :: (Int, Int) -> (Int, Int) -> Matrix a -> Matrix a
subMatrix _ _ [] = []
subMatrix _ (0, _) _ = []
subMatrix (0, j) (m, n) (xs : xss) =
    let xs'  = take n (drop j xs)
        xss' = subMatrix (0, j) (m-1, n) xss
    in  xs' : xss'
subMatrix (i, j) size (xs : xss) = subMatrix (i-1, j) size xss 


type Sudoku = [[Maybe Int]]

solve :: Sudoku -> [Sudoku]
solve = solve' 3

solve' :: Int -> Sudoku -> [Sudoku]
solve' rank m = case nextEmpty m of
                     Nothing -> return m
                     Just (i, j) -> do t <- [1..rank^2]
                                       guard (unique rank i j t m)
                                       let m' = replace i j t m
                                       solve' rank m'
              
             
nextEmpty :: Sudoku -> Maybe (Int, Int)
nextEmpty m = do i <- findIndex (elem Nothing) m
                 j <- findIndex isNothing (m !! i)
                 Just (i, j)

guard :: Bool -> [()]
guard False = []
guard True = [()]

unique :: Int -> Int -> Int -> Int -> Sudoku ->  Bool
unique rank i j t m
    = let r = unique' (row i m)
          c = unique' (column j m)
          sm = uniqueSM
      in  r && c && sm
  where
      unique' :: [Maybe Int] -> Bool
      unique' [] = True
      unique' (Nothing : ns) = unique' ns
      unique' (Just n : ns) = n /= t && unique' ns

      uniqueSM :: Bool
      uniqueSM = let (i', j') = (select i, select j)
                     sm = subMatrix (i', j') (rank, rank) m
                 in  foldl (\b r -> b && unique' r) True sm

      select :: Int -> Int
      select n = div n rank * rank

replace :: Int ->  Int -> Int -> Sudoku -> Sudoku
replace i j t m = let r = m !! i
                      r' = take j r ++ Just t : drop (j+1) r
                  in  take i m ++ r' : drop (i+1) m