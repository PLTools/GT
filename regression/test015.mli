@type 'a tree = Node of 'a * 'a tree GT.list deriving show, foldl, foldr
