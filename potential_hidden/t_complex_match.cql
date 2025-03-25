-- Complex matching with multiple conditions
PERMUTE #3, #1, #2 FROM A WHERE MATCH #1 #2 OR MATCH #2 #3;