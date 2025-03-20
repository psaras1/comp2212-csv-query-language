-- Task 5: Left merge on first column
-- For each pair of rows p1,p2,p3,p4 in P and q1,q2,q3,q4 in Q where p1=q1,
-- output p1,r2,r3,r4 where ri is qi if pi is empty, and pi otherwise

LEFT MERGE P ON #1 WITH Q;