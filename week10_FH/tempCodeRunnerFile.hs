findUncles :: Tree -> Int -> [Int]
findUncles tree node = case findParent tree node of
    Nothing -> []
    Just parent -> filter (/= parent) $ siblings (findParent tree parent)
  where
    siblings Nothing = []
    siblings (Just parent) = concatMap snd $ filter ((/= node) . fst) treeNodes
      where
        treeNodes = filter ((== parent) . fst) tree
    findParent t n = fmap fst $ find (\(k, v) -> elem n v) t