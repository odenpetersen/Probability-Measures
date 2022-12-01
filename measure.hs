{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class SigmaAlgebra s where
	union :: s -> s -> s
	intersection :: s -> s -> s

class SigmaAlgebra s => Measurable d s where
	measure :: d -> s -> Float

type Borel = [(Float,Float)]
interval :: Float -> Float -> Borel
interval a b = [(a,b)]

instance SigmaAlgebra Borel where
	union xs []                     = xs
	union [] ys                     = ys
	union ((xl,xu):xs) ((yl,yu):ys) = if (xu<yl) then
	                                  	(xl,xu):(union xs ((yl,yu):ys))
	                                  else if (yu<xl) then 
	                                  	(yl,yu):(union ((xl,xu):xs) ys)
	                                  else
						(min xl yl,max xu yu):(union xs ys)
	intersection _ []                      = []
	intersection [] _                      = []
	intersection ((xl,xu):xs) ((yl,yu):ys) = if (xu<yl) then
	                                         	intersection xs ((yl,yu):ys)
	                                         else if (yu<xl) then
	                                         	intersection ((xl,xu):xs) ys
	                                         else
	                                         	union ([(max xl yl, min xu yu)]) (intersection xs ys)

data Measure = Uniform Float Float | Lebesgue | Dirac Float

instance Measurable Measure Borel where
	measure _ [] = 0
	measure Lebesgue [(xl,xu)] = xu-xl
	measure (Uniform a b) [i] = (measure Lebesgue $ intersection [(a,b)] [i])/(b-a)
	measure (Dirac x) [(xl,xu)] = if (xl<x && x<xu) then 1 else 0
	measure d xs = sum $ map ((measure d).(:[])) xs
