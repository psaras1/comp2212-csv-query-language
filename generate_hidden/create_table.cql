-- Create a temporary table containing only employees from HR
CREATE hr_employees AS SELECT #1, #2, #3 FROM A WHERE #4 = "HR";

-- Now use the temporary table 
SELECT * FROM hr_employees;