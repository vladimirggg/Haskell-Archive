main :: IO()
main = do
    print $ primesInRangeLC 1 100 == []
    print $ primesInRangeLC 998 1042 == [1009,1013,1019,1021,1031,1033,1039]
    print $ primesInRangeLC 120 666 == [127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661]
    print $ primesInRangeLC 420 240 == [241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419]
    
    print $ primesInRangeHOF 1 100 == []
    print $ primesInRangeHOF 998 1042 == [1009,1013,1019,1021,1031,1033,1039]
    print $ primesInRangeHOF 120 666 == [127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661]
    print $ primesInRangeHOF 420 240 == [241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419]

primesInRangeLC :: Int -> Int -> [Int]
primesInRangeLC x y = [n | n <- [min x y .. max x y], n > 100 &&  null [d | d <- [2 .. n-1], mod n d == 0]]

primesInRangeHOF :: Int -> Int -> [Int] 
primesInRangeHOF x y = filter (\ n -> n > 100 &&  null [d | d <- [2 .. n-1], mod n d == 0]) [min x y .. max x y]