import Data.List

data Direction = Straight_ | Left_ | Right_ deriving (Show, Eq)

data Point a = Point a a deriving (Show, Eq)

invert :: Direction -> Direction
invert Left_ = Right_
invert Right_ = Left_
invert _ = Straight_


toDirection :: (Num a, Ord a) => Point a -> Point a -> Point a -> Direction
toDirection (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | det < 0 = Right_
    | det > 0 = Left_
    | det == 0 = Straight_
    where det = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)


toPath :: (Fractional a, Ord a) => [Point a] -> [Direction]
toPath (p1 : p2 : p3 : ps) = toDirection p1 p2 p3 : toPath (p2:p3:ps)
toPath _ = []


safeEdge :: (Fractional a, Ord a) => Point a -> Point a -> Point a
safeEdge p1@(Point x1 y1) p2@(Point x2 y2)
    | x1 < x2 = p1
    | x2 > x1 = p2
    | y1 < y2 = p1
    | y1 > y2 = p2


sortAngle :: (RealFloat a, Ord a) => [Point a] -> [Point a]
sortAngle l = sortBy f l
    where 
        f (Point x1 y1) (Point x2 y2) = compare t1 t2
            where 
                t1 = fuckNan (tan $ y1/x1)
                t2 = fuckNan (tan $ y2/x2)
                fuckNan x = case x of
                    x | isNaN x -> pi/2
                    x -> x 


grahamScan :: (RealFloat a, Ord a) => [Point a] -> [Point a]
grahamScan [] = []
grahamScan ps 
    | length ps <= 3 = start : sortedSet
    | otherwise = scan start sortedSet
    where
        sortedSet = sortAngle $ delete start ps
        start = foldl1 safeEdge ps
        
        scan p1 (p2:p3:ps) = case toDirection p1 p2 p3 of
            Right_ -> scan p1 (p3:ps) -- reject move, again with same tail and next head
            Left_ -> p1 : scan p2 (p3:ps) -- Save point, onward with next point
            Straight_ -> scan p1 (p3:ps) -- Discard mid point, onward with next point
        scan p1 ps = p1:ps


main = do
    let p1 = Point 0 0
    let p2 = Point 1 1
    let p3 = Point 2 2
    let p4 = Point 3 0
    let p5 = Point 0 3
    let p6 = Point 0.5 1
    let p7 = Point 1 0.5
    let p8 = Point 1 (-1.5)
    let p9 = Point 4 1.5

    putStrLn $ show $ toDirection p1 p2 p3
    putStrLn $ show $ toDirection p1 p2 p4
    putStrLn $ show $ toDirection p1 p2 p5

    putStrLn $ show $ toDirection p3 p2 p1
    putStrLn $ show $ toDirection p4 p2 p1
    putStrLn $ show $ toDirection p5 p2 p1

    putStrLn $ show $ toPath [p1, p2, p5, p3, p2, p4, p1, p2, p3]
    putStrLn "\n"
    let ps = [p1, p2, p3, p4, p5, p6, p7, p8, p9]
    let start = foldl1 safeEdge ps
    let sortedSet = sortAngle $ delete start ps
    putStrLn $ show sortedSet
    putStrLn $ show $ grahamScan ps
    