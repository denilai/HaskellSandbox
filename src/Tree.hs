module Tree where

replicateDash = flip replicate '_'

baseY =
  [ replicateDash x ++
  ['1'] ++ replicateDash (31 - 2 * x) ++ ['1'] ++ replicateDash x
  | x <- [0 .. 15]
  ] ++
  replicate 16 (replicateDash 16 ++ ['1'] ++ replicateDash 16)

shrink = dropAlternate . map dropAlternate

dropAlternate x = map fst $ filter (even . snd) $ zip x [0 ..]

iterShrink n = iterate shrink baseY !! (n - 1)

getPadding starting n =
  scanl (-) starting [floor $ 8 * 2 ** (-x) | x <- [0 ..]] !! (n - 1)



leftRightPad n x =
  let padding = getPadding 33 n
   in replicateDash padding ++ x ++ replicateDash (padding + 1)

copy n x =
  let padding = getPadding 15 (n - 1)
   in concat (replicate (2 ^ (n - 1) - 1) (x ++ replicateDash padding)) ++ x

y 0 = []
y n = map (leftRightPad n . copy n) (iterShrink n) ++ y (n - 1)

addTopPadding tree = replicate (63 - length tree) (replicateDash 100) ++ tree

makeTree n =
  let tree = y n
   in addTopPadding tree

printTree :: [String] -> IO ()
printTree = mapM_ putStrLn

tree = interact $ unlines . makeTree . read