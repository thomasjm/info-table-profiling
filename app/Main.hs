{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, forM_)
import Data.List (foldl')


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

buildTree :: Int -> Tree Int
buildTree 0 = Leaf 0
buildTree n = Node (buildTree (n-1)) (buildTree (n-1))

sumTree :: Tree Int -> Int
sumTree (Leaf x) = x
sumTree (Node l r) = sumTree l + sumTree r

allocateLists :: Int -> [Int]
allocateLists n = [1..n]

processLists :: [[Int]] -> Int
processLists lists = foldl' (+) 0 (map (foldl' (+) 0) lists)

main :: IO ()
main = do
  putStrLn "Starting heap profiling demo..."

  putStrLn "Phase 1: Building and processing trees"
  replicateM_ 5 $ do
    let tree = buildTree 15
    let result = sumTree tree
    putStrLn $ "Tree sum: " ++ show result
    threadDelay 500000

  putStrLn "Phase 2: Allocating large lists"
  forM_ [10000, 50000, 100000, 200000] $ \size -> do
    let bigList = allocateLists size
    let result = foldl' (+) 0 bigList
    putStrLn $ "List size " ++ show size ++ ", sum: " ++ show result
    threadDelay 500000

  putStrLn "Phase 3: Multiple concurrent allocations"
  let lists = map allocateLists [10000, 20000, 30000, 40000, 50000]
  let totalSum = processLists lists
  putStrLn $ "Total sum of all lists: " ++ show totalSum

  putStrLn "Phase 4: String allocations"
  replicateM_ 3 $ do
    let strings = map (\i -> "String number " ++ show i ++ " with extra padding") [1..10000]
    let totalLength = sum (map length strings)
    putStrLn $ "Total string length: " ++ show totalLength
    threadDelay 1000000

  putStrLn "Heap profiling demo completed!"
