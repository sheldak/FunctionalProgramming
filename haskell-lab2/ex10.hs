fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

sndDivByFst :: Integral a => [a] -> Bool
sndDivByFst (x : y : _) | y `mod` x == 0 = True
sndDivByFst _                            = False

trdDivByFst :: Integral a => [a] -> Bool
trdDivByFst (x : y : z : _) | z `mod` x == 0 = True
trdDivByFst _                                = False