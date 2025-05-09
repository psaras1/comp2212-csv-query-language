-- Task 9: Paths of length three

CREATE R_Cartesian_R AS CARTESIAN PRODUCT R, R;
CREATE Path2 AS PROJECT #1, #4 FROM R_Cartesian_R WHERE MATCH #2 #3;
CREATE Path2_Cartesian_R AS CARTESIAN PRODUCT Path2, R;
PROJECT #1, #4 FROM Path2_Cartesian_R WHERE MATCH #2 #3;