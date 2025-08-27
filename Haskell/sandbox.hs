
type Pozice = (Double, Double)

sloupky :: [Pozice]
sloupky = [(-1,2),(2,2),(-2,1),(1,1),(0,0),(3,0),(0,-1),(1,-1),(2,-2),(3,-2)]




vzdalenost :: Pozice -> Pozice -> Double 
vzdalenost (x1,y1) (x2,y2) = sqrt((x1 - x2)**2 + (y1 - y2)**2)
        
sousedni :: Pozice -> [Pozice] -> [Pozice]
sousedni pos poslist = filter (\x -> vzdalenost pos x >= 1 && vzdalenost pos x <= 2) poslist

prodluz :: [a] -> [a] -> [[a]]
prodluz heads list = [(x:list)| x<-heads]



hop :: [Pozice] -> Pozice -> Pozice -> Maybe [Pozice]
hop points start end
    | start == end      = Just []
    | otherwise         = path points [[start]] end
    where 
        

path :: [Pozice] -> [[Pozice]] -> Pozice -> Maybe [Pozice]
path unexplored paths target 
    | length reached > 0  = Just (reverse (head reached))
    | length explorable == 0 = Nothing
    | otherwise = path new extended target
        where 
            reached = filter (\x -> elem target x) paths
            extended = concatMap (\x -> prodluz(sousedni (head x) unexplored) x) paths
            new = filter (\x -> not (elem x (concat extended))) unexplored
            explorable = concatMap (\x -> sousedni (head x) unexplored) paths