{- Cash register -> amount -> change for amount
    Convert an integer amount of money into coins where
    the values of the coins are chosen from a cash register.
    The cash register should be sorted in descending order.
    If a value can't be exactly represented with the coins
    provided, the function will return the maximum amount,
    so that out < in.
-}
getChange :: [Int] -> Int -> [Int]
getChange _ 0 = []
getChange [] _ = []
getChange (c: r) amount = 
    if amount - c >= 0
        then c: (getChange (c:r) (amount - c))
        else getChange r amount
