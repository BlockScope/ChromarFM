module Agent where

data Par
    = V
    | L Int
    deriving (Eq, Show)

data Env = Env
    { psim  :: Double
    , frepr :: Double
    } deriving (Show)

data Attrs = Attrs
  { ind :: Int
  , psi :: Double
  , fi  :: Double
  } deriving (Eq, Ord, Show)

data Agent
    = Seed { mass :: Double
           , attr :: Attrs
           , dg :: Double
           , art :: Double}
    | Leaf { attr :: Attrs
           , i :: Int
           , ta :: Double
           , m :: Double
           , a :: Double}
    | Cell { attr :: Attrs
           , c :: Double
           , s :: Double}
    | Root { attr :: Attrs
           , m :: Double}
    | Plant { thrt :: Double
            , attr :: Attrs
            , dg :: Double
            , wct :: Double}
    | EPlant { sdeg :: Double
             , thrt :: Double
             , attr :: Attrs
             , dg :: Double
             , wct :: Double}
    | FPlant { attr :: Attrs
             , dg :: Double
             , nf :: Int
             , rosM :: Double  
             , fthrt :: Double }
    | VAxis { nv :: Int }
    | LAxis { lid :: Int
            , nl :: Int
            , llta :: Double}
    | INode { im :: Double
            , ita :: Double
            , pin :: Par
            , iid :: Int}
    | LLeaf { lm :: Double
            , la :: Double
            , lta :: Double
            , pll :: Par
            , lid :: Int}
    | Fruit { fm :: Double
             ,fta :: Double
             ,pf :: Par}
    deriving (Eq, Show)

-- instance Eq Agent where
--   (==) Seed{attr=a} Seed{attr=a'} = ind a == ind a'
--   (==) Leaf{attr=a, i=i} Leaf{attr=a', i=i'} = (ind a == ind a') && (i == i')
--   (==) Cell{attr=a} Cell{attr=a'} = ind a == ind a'
--   (==) Root{attr=a} Root{attr=a'} = ind a == ind a'
--   (==) Plant{attr=a} Plant{attr=a'} = ind a == ind a'
--   (==) EPlant{attr=a} EPlant{attr=a'} = ind a == ind a'
--   (==) FPlant{attr=a} FPlant{attr=a'} = ind a == ind a'
--   (==) _ _ = False

--   (/=) a a' = not ((==) a a')

isCell (Cell{c=c}) = True
isCell _ = False

isLeaf :: Agent -> Bool
isLeaf Leaf {} = True
isLeaf _ = False

isPlant :: Agent -> Bool
isPlant (Plant{}) = True
isPlant _ = False

isEPlant :: Agent -> Bool
isEPlant (EPlant{}) = True
isEPlant _ = False

isFPlant :: Agent -> Bool
isFPlant (FPlant{}) = True
isFPlant _ = False

isSeed :: Agent -> Bool
isSeed (Seed{}) = True
isSeed _ = False

isFPlant :: Agent -> Bool
isFPlant (FPlant{}) = True
isFPlant _ = False

isINode :: Agent -> Bool
isINode (INode{}) = True
isINode _ = False

isFruit :: Agent -> Bool
isFruit (Fruit{}) = True
isFruit _ = False

isLLeaf :: Agent -> Bool
isLLeaf (LLeaf{}) = True
isLLeaf _ = False

isVAxis :: Agent -> Bool
isVAxis (VAxis{}) = True
isVAxis _ = False
