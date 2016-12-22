enc 1
fact := fix (\f.\x.if (iszero x) 1 (mult x (f (pre x))))
