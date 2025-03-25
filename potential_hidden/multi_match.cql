-- Permute columns where two pairs of columns match
PERMUTE #1, #3 FROM A WHERE MATCH #1 #2 AND MATCH #3 #4;