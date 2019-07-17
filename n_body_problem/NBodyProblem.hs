import           Debug.Trace
import           Vector

type Position = Vec2D Double

type Velocity = Vec2D Double

type Acceleration = Vec2D Double

type Mass = Double

data Body =
  Body
    { bodVelo :: [Velocity]
    , bodPos  :: [Position]
    , bodAcc  :: [Acceleration]
    , bodMass :: Double
    }
  deriving (Show, Eq)

body :: Velocity -> Position -> Mass -> Body
body v r m = Body {bodVelo = [v], bodPos = [r], bodAcc = [], bodMass = m}

-- normalize a list so that: sum list = 1
normalize :: (Fractional a) => [a] -> [a]
normalize a = map (/ sum a) a

-- Get all circular permutations of a list
circPerms :: [a] -> [[a]]
circPerms [] = []
circPerms xs = map (swapAppend . (flip splitAt xs)) [0 .. length xs - 1]
  where
    swapAppend = uncurry . flip $ (++)

-- Calculate the velocity vector for a body
velocity :: Double -> Double -> Position -> Velocity
velocity bigG bigM rs = Vec2D (y, x)
  where
    Vec2D (x, y) = fmap f rs
    f =
      (\v ->
         if not $ isInfinite v
           then v
           else 0) .
      (sqrt . (bigG * bigM /))

-- δt -> t -> [Bodies(t-1)] -> [Bodies(t)]
type Solver = Double -> Double -> [Body] -> [Body]

eulerMethod :: Solver
eulerMethod _ _ [] = []
eulerMethod δt t (b:bs) = b' : eulerMethod δt t bs
  where
    b' = b {bodVelo = v' : vs, bodPos = r' : rs}
    vs@(v:_) = bodVelo b
    rs@(r:_) = bodPos b
    (a:as) = bodAcc b
    v' = v + a `scalMult` δt
    r' = r + v `scalMult` δt

-- G -> Bodies(t) -> t -> Bodies(t) with acc
acceleration :: Double -> [Body] -> Double -> [Body]
acceleration bigG bs t = map acceleration' (circPerms bs) -- calculate acceleration for all circular permutations of the list
  where
    acceleration' :: [Body] -> Body -- Calculate acceleration of first body in the list
    acceleration' (b:bs) = b'
      where
        b' = b {bodAcc = a : bodAcc b}
        a = sum aPart
        ri = head $ bodPos b
        rijs = [rj - ri | rj <- map (head . bodPos) bs]
        aPart =
          [ rij `scalMult` mj `scalDiv` ((norm rij) ** 3)
          | (rij, mj) <- zip rijs (map bodMass bs)
          ]

-- solver -> G -> δt -> bodies -> timestep -> bodies with new timestep
timestep :: Solver -> Double -> Double -> [Body] -> Double -> [Body]
timestep solver bigG δt bs t = bs''
  where
    bs' = acceleration bigG bs t
    bs'' = solver δt t bs'

main
  -- actual main
 = do
  let ts = take (round $ t / δt) $ iterate (+ δt) 0
  let systemEuler = timestep eulerMethod bigG δt
  let solution = foldr (flip systemEuler) bodies ts
  -- process solution
  let steps = map ((++ "\n") . show . reverse . bodPos) solution
  writeFile "n_bod_prob_solution.txt" (concat steps)
  where
    ms = normalize m'
    bigM = sum ms
    vs = map (velocity bigG bigM) rs
    bodies = zipWith3 body vs rs ms
    -- config
    δt = 0.05
    t = 100
    bigG = 1.0
    m' = [1.989e30, 5.972e24, 3.30e23, 6.4219e23, 4.869e24]
    rs = map Vec2D [(0, 0), (1, 0), (0.38, 0), (1.52, 0), (0.72, 0)]
