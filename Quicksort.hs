module Quicksort where
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Data.Bits

--  quicksort with scan primitives according to the paper "Scans as Primitive Parallel Operations" by Blelloch
-- no recursion or iteration with Arrays as arguments

main = do quicksort numbers
  where numbers = generate (index1 10000) $ \ix ->
                                            let (Z :. i) = unlift ix
                                            in A.fromIntegral i

quicksort :: Acc (Vector Double) -> Acc (Vector Double)
quicksort arr = use $ qs (run arr) segment  -- TODO: make "use" and "run" unnecessary
  where segment = run (generate (index1 1) (\_ -> size arr)) -- one big segment of the size of the array

-- should be Acc (Vector Double) -> Acc (Vector Double)
-- unfortunately there is currently no recursion or iteration over arrays
qs :: Vector Double -> Vector Int -> Vector Double   
qs arr segments = run ( the (A.and sorted)
     ?| (ar,
         use $ qs (run newArr) (run (A.filter (>* 0) newSegments))) ) -- zeros have to filtered because of segmentedCopy
  where
        ar = use arr :: Acc (Vector Double)
        pivots = segmentedCopy ar nonzeroSegments --  the first element of a segment is the pivot
        less  = A.zipWith (\x y -> x <* y  ? (1,0) ) ar pivots :: Acc (Vector Int)
        equal = A.zipWith (\x y -> x ==* y ? (1,0) ) ar pivots :: Acc (Vector Int)
        sorted = A.zipWith (<=*) (A.init ar) (A.tail ar)
        nonzeroSegments = A.filter (>* 0) (use segments)
        (newArr,newSegments) = (Prelude.fst s, Prelude.snd s)
        s = segmentedSplit ar less equal nonzeroSegments

-- segments are not allowed to contain zeros
segmentedCopy :: Acc (Vector Double) -> Acc (Vector Int) -> Acc (Vector Double)
segmentedCopy arr segments = -- copy the first element of every segment to the rest of the segment
                             A.scanl1Seg (+)
                            -- pick the starting value of every segment and copy it into a zeroArray
                           (A.scatter segmentStartPositions zerosArr (A.gather segmentStartPositions arr))
                           segments
             where zerosArr = generate (shape arr) (const 0)
                   segmentStartPositions = A.init $ A.scanl (+) 0 segments -- A.init is the reason for non-zero segments

-- arr is the array to be worked on, l = less, e = equal are two flag-arrays
segmentedSplit arr l e segments = (A.scatter index zerosArr arr, newSegments)
  where index = A.zipWith (\le enum -> (A.fst le) ==* 1 ? ((A.fst (A.fst enum),
                                                           (A.snd le) ==* 1 ?  (A.snd (A.fst enum), A.snd enum))))
                                                           (A.zip l e)
                                                           (A.zip (A.zip enum_l enum_e) enum_g)
        enum_l = addOffsets segments empty   $ A.map (\x -> x-1) $ A.scanl1Seg (+) l       segments
        enum_e = addOffsets segments offsetL $ A.map (\x -> x-1) $ A.scanl1Seg (+) e       segments
        enum_g = addOffsets segments offsetE $ A.map (\x -> x-1) $ A.scanl1Seg (+) greater segments
        greater = A.zipWith (\x y -> 1 - (x .|. y) ) l e
        offsetL = A.fold1Seg (+) l segments
        es = A.fold1Seg (+) e segments
        gs = A.fold1Seg (+) greater segments
        offsetE = A.zipWith (+) offsetL es
        zerosArr = generate (shape arr) (const 0)
        empty = generate (shape segments) (const 0) :: Acc (Vector Int)
        zerosArr3 = generate (index1 ((size arr)*3)) (const 0) :: Acc (Vector Int)
        newSegments = A.scatter multiples3plus2
                     (A.scatter multiples3plus1
                     (A.scatter multiples3 zerosArr3 offsetL) es) gs
        multiples3 = A.scanl (+) 0 (generate (index1 ((size arr)*3)) (const 3)) :: Acc (Vector Int)
        multiples3plus1 = A.map (+1) multiples3
        multiples3plus2 = A.map (+2) multiples3

addOffsets segments innerOffsets indices = A.zipWith (+) indices off
  where off =  A.scanl1Seg (+) -- copying the start value over the segment
              -- scatter map                   default  input
              (A.scatter segmentStartPositions zerosArr (A.zipWith (+) segmentStartPositions innerOffsets))
              segments
        zerosArr = generate (shape indices) (const 0)
        segmentStartPositions = A.init $ A.scanl (+) 0 segments

zeros = fromList (Z:.8) [3,0,0,0,0,0,0,0] :: Vector Double

-- some example data for testing (from page 10)
keys2 = fromList (Z:.8) [5,7,3,1,4,2,7,2] :: Vector Double
flags = fromList (Z:.8) [1,1,1,1,0,0,1,0] :: Vector Int

-- examples from page 12
keys   = fromList (Z:.10) [1.1,9.9,6.4,9.2,3.4,1.6,8.7,4.1,9.2,3.4] :: Vector Double
-- seg    = fromList (Z:.1) [8] :: Segments Int
-- pivots = fromList (Z:.8) [6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4] :: Vector Double
-- less   = fromList (Z:.8) [0,0,1,1,0,1,0,1] :: Vector Int
-- equal  = fromList (Z:.8) [1,0,0,0,0,0,0,0] :: Vector Int
-- f      = fromList (Z:.8) [ = , > , < , < , > , < , > , < ] :: Vector Int

-- keys    = fromList (Z:.8) [3.4,1.6,4.1,3.4,6.4,9.2,8.7,9.2] :: Vector Double
-- seg     = fromList (Z:.3) [ 4             , 1 , 3         ] :: Segments Int
-- pivots  = fromList (Z:.8) [3.4,3.4,3.4,3.4,6.4,9.2,9.2,9.2] :: Vector Double
-- less    = fromList (Z:.8) [0,1,0,0,0,0,1,0] :: Vector Int
-- equal   = fromList (Z:.8) [1,0,0,1,1,1,0,1] :: Vector Int
-- f      = fromList (Z:.8) [ = , < , > , = , = , = , < , = ] :: Vector Int

-- keys   = fromList (Z:.8) [1.6,3.4,3.4,4.1,6.4,8.7,9.2,9.2] :: Vector Double
-- seg    = fromList (Z:.6) [ 1 , 2     , 1 , 1 , 1 , 2     ] :: Segments Int
-- less    = fromList (Z:.8) [0,0,0,0,0,0,0,0] :: Vector Int
-- equal   = fromList (Z:.8) [1,1,1,1,1,1,1,1] :: Vector Int


-- older functions to approach the goal

-- split according to one boolean flag array (see page 10 of the blelloch paper)
split arr flags = A.scatter index zerosArr arr
  where index = A.zipWith (\f ebe -> f ==* 1 ? (A.fst ebe, A.snd ebe)) flags (A.zip back_enumerate enumerate)
        f_negated = A.map (1-) flags
        enumerate = A.init $ A.scanl (+) 0 f_negated
        back_enumerate = A.tail $ A.map (\x -> (size arr)-1-x) $ A.scanr (+) 0 flags
        zerosArr = generate (shape arr) (const 0)

-- split with two boolean arrays, one for "less than" and one for "equal",
-- "greater" is the same as not (less than or equal)
split3 arr l e = A.scatter index zerosArr arr
  where index = A.zipWith (\le enum ->
                            (A.fst le) ==* 1 ? ((A.fst (A.fst enum),
                                                (A.snd le) ==* 1 ?  (A.snd (A.fst enum), A.snd enum))))
                                                (A.zip l e) (A.zip (A.zip enum_l enum_e) enum_g)
        enum_l = A.init $ A.scanl (+) 0 l
        enum_e = A.init $ A.map (+innerOffsetL) (A.scanl (+) 0 e)
        enum_g = A.init $ A.map (+innerOffsetE) (A.scanl (+) 0 greater)
        greater = A.zipWith (\x y -> 1 - (x .|. y) ) l e
        innerOffsetL = the (A.fold1 (+) l)
        innerOffsetE = innerOffsetL + (the (A.fold1 (+) e))
        zerosArr = generate (shape arr) (const 0)
