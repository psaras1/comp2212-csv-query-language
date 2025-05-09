-- Task 7: Paired Composition

-- Temp table FG using cartesian product of F and G(all rows non empty by default)
CREATE FG AS CARTESIAN PRODUCT F, G;
-- Project the first and last columns of FG where the second and third columns are not empty, the second column matches the fourth column and the third column matches the fifth column
PROJECT #1, #6 FROM FG WHERE NOT EMPTY #2 AND NOT EMPTY #3 AND MATCH #2 #4 AND MATCH #3 #5;