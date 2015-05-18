-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

main = print . head $ [a*b*c | a<-[1..998], b<-[a..(999-a)], let c = 1000-b-a, a^2+b^2==c^2]

