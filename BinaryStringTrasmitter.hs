{-
  Patrick Joseph Persico

  Program that simulates Transmission of a list of characters in a
  low-level list of binary digits

-}

import Data.Char

type Bit = Int

-- A binary number represented as list of bits converted into an integer
-- by evaluating the appropriate weighted sum
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--                     where weights = iterate (*2) 1
-- using foldr
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- Integer to binary devinding by two retrieving reminder
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Ensures all binary numbers are 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Parity bits and checking added from exercises
parity :: [Bit] -> Int
parity bits | odd (sum bits) = 1
            | otherwise      = 0

addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

-- To decode a list of bits using encode we define chop9 that chops the list
-- into eight-bit binaryNumbers plus the parity Bit.
chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity (x:xs) | x == parity xs = xs
                   | otherwise      = error "Invalid parity bits."

-- Encodes a string of chars as list of bits by converting each char into
-- a Unicode number, each number into a eight-bit binary number. And
-- concatenating them to create a list of bits, and adds parity bit.
encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

-- Also checks for parity after channeling
decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id
--channel = tail To make the parity fail

transmit :: String -> String
transmit = decode . channel . encode
