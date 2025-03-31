-- Do everything in a single query
CREATE final_result AS 
SELECT #1, #2, #4, #5 FROM A 
WHERE #3 = "Electronics" AND #4 > 500 AND #5 > 100;

-- Display the result
SELECT * FROM final_result;