-- Test filtering a cartesian product by matching columns
SELECT * FROM (CARTESIAN PRODUCT A, B) WHERE MATCH #1 #3;