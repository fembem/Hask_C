-----------------------------------------------------------------------------
--
-- Module      :  Lab03_part2
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Lab03_part2 (

) where

data Quaternion = Quaternion Double Double Double Double deriving (Eq)

{-- "1.0 + 2.0i + 3.0j + 4.0k" --}
showQuaternion:: Quaternion -> String
showQuaternion (Quaternion a b c d) = ( (show a) ++ " + " ++ (show b) ++ "i"
                                      ++ " + " ++ (show c) ++ "j"
                                      ++ " + " ++ (show d) ++ "k")

instance Show Quaternion where
    show =  showQuaternion

{--

    i * i = -1
    j * j = -1
    k * k = -1
    i * j =  k
    j * i = -k
    j * k =  i
    k * j = -i
    k * i =  j
    i * k = -j

--}

{--

let [a1, b1, c1, d1, a2, b2, c2, d2] = [1.0:: Double, 1.0:: Double,1.0:: Double,1.0:: Double, 1.0:: Double,1.0:: Double,1.0:: Double,1.0:: Double]

(Quaternion a1 b1 c1 d1) * (Quaternion a2 b2 c2 d2) =
    (Quaternion (a1 * a2 − b1 * b2 − c1 * c2 − d1 * d2)
               (a1 * b2 + b1 * a2 + c1 * d2 − d1 * c2)
               (a1 * c2 − b1 * d2 + c1 * a2 + d1 * b2)
               (a1 * d2 + b1 * c2 − c1 * b2 + d1 * a2) )

fromInteger i = (Quaternion i 0 0 0)

signum (Quaternion a b c d) =
    let val = abs (Quaternion a b c d)
    in Quaternion (a / val) (b / val) (c / val) (d / val)

abs (Quaternion a b c d) = (Quaternion (Math.sqrt ( (a * a) ( b * b) (c * c) (d * d) ) 0.0 0.0 0.0 )
--}


instance Num Quaternion where

    abs (Quaternion a b c d) = Quaternion ( sqrt (a * a + b * b + c * c + d * d)) 0.0 0.0 0.0

    (Quaternion a1 b1 c1 d1) + (Quaternion a2 b2 c2 d2) = Quaternion (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

    negate (Quaternion a b c d) = Quaternion (-a) (-b) (-c) (-d)

    (Quaternion a1 b1 c1 d1) * (Quaternion a2 b2 c2 d2) =
      (Quaternion (a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2)
      (a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2)
      (a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2)
      (a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2) )

    fromInteger i = (Quaternion (fromIntegral i ) 0.0 0.0 0.0)

    signum (Quaternion a b c d) =
        let (Quaternion val _ _ _) = abs (Quaternion a b c d)
            in Quaternion (a / val) (b / val) (c / val) (d / val)

