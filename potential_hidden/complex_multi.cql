-- Cartesian product of A and B, then filter where A.1=B.1, 
-- then permute to get B.2, A.2 for the result
-- PERMUTE #3, #2 FROM (CARTESIAN PRODUCT A, B) WHERE MATCH #1 #3;
PERMUTE #4, #2 FROM (CARTESIAN PRODUCT A, B) WHERE MATCH #1 #3;