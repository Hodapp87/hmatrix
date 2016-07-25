

module Main where

import Numeric.LinearAlgebra
import Numeric.LinearProgramming

count = 25
-- Bug is present only with count >= 25
p0 = (2><count) $ repeat 1

-- Bug is not present if I change [Matrix R] and [Double R], which I
-- use only as lists with two elements, to tuples
recurse :: Int -> Int -> [Matrix R] -> IO [Matrix R]
recurse _ 0 m = return m
recurse x iters m = do
  a <- f $ map (\q -> flatten $ q ? [x]) m
  let d = zipWith (\v q -> accum q (\_ _ -> v) [((x, 1), v)]) a m
  recurse x (iters - 1) d

f :: [Vector R] -> IO [Double]
f [p, q] = do
  let obj = zipWith (+) (toList p) (toList q)
            -- Addition above seems necessary for the bug
      constr = Dense $ [ replicate count 1 :==: 1 ]
      bounds = map (:&: (0,1)) [1..count]
  solA <- simplex (Maximize obj) constr bounds
  let tryA = case solA of
        Optimal  (s, l) -> let r = vector l in [p <.> r, q <.> r]
        Feasible (s, l) -> let r = vector l in [p <.> r, q <.> r]
        -- 'Feasible' case must be present above for bug
        t@_ -> error ("Failed at try A: " ++ show t ++ ", obj=" ++ show obj)
  solB <- simplex (Maximize obj) constr bounds
  let tryB = case solB of
        Optimal  (s, l) -> let r = vector l in [p <.> r, q <.> r]
        t@_ -> error ("Failed at try B: " ++ show t ++ ", obj=" ++ show obj)
  return $ seq tryA tryB

main :: IO ()
main = do
  sol <- recurse 1 100 [p0, p0]
  error "fail"
  putStrLn $ show sol
