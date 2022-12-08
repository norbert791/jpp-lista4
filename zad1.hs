factorial n = if n < 2 then 1 else factorial (n - 1)

gcdPriv = if b == 0 then a else gcd b (a % b)
gcd a b = gcdPriv abs(a) abs(b)


