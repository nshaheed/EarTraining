contextGetter :: DeviceContext -> (Float,Float) -> (Int, Int) -> Bool -> (Bool, Canvas a)
contextGetter context (x',y') (a',b') pn = pn 

contextGetterHelper :: DeviceContext -> (Float, Float) -> (Int, Int) -> Bool -> Canvas a


isButtPressed :: DeviceContext -> (Float, Float) -> Bool 
isButtPressed context (x',y') = let buttCoord = [ (x*55,150) | x <- [1..5]]
                                    wdt = width context
                                    hgt = height context
                                    wRat = 3/5
                                    hRat = 3/10
                                    buttIdx = buttonPress buttCoord (x' - wdt * wRat, y' - hgt * hRat)
                                in if isJust then False else True
