f = foldr (\(x,y) b->(x,y+1):b) []  
f [("x",1),("y",2)]
