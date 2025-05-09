-- Task 10
-- For each row p1, p2, p3 in P and each row q1, q2, q3 in Q
-- output p3, q1 if p1 = p2 and q2 = q3

CREATE combined AS CARTESIAN PRODUCT S, T;

-- Filter rows where p1 = p2 and q2 = q3, then project p3, q1
PROJECT #3, #4 FROM combined WHERE MATCH #1 #2 AND MATCH #5 #6;