-- PROJECT and PROJECT BY operations


-- Example 1: Simple PROJECT operation
-- selects only the product name and price columns from each row
-- Expected output: A 2-column table with all 10 products' names and prices
PROJECT #2, #4 FROM products;


-- Example 2: PROJECT with WHERE condition
-- This filters rows before projection
-- Only returns products that are in stock (column 5 = "yes")
-- Expected output: A 2-column table with only the 6 in-stock products
PROJECT #2, #4 FROM products WHERE #5 = "yes";


-- Example 3: Basic PROJECT BY operation
-- This groups rows by category and returns one row per unique category
-- The row returned is the FIRST row in each group (no aggregation performed)
-- Expected output: A 1-column table with 3 rows (Electronics, Clothing, Media)
PROJECT #3 FROM products BY #3;


-- Example 4: PROJECT with multiple columns and GROUP BY
-- This groups rows by category and returns one product name and price per category
-- The values come from the FIRST row in each group, no aggregation is performed
-- Expected output: A 2-column table with name and price for the first product in each category
PROJECT #2, #4 FROM products BY #3;


-- Example 5: PROJECT with multiple grouping columns
-- This groups products by both category and in-stock status
-- Expected output: A 1-column table with product names, one for each unique category+stock combination
PROJECT #2 FROM products BY #3, #5;


-- Example 6: Creating a temporary table with PROJECT results
-- This creates a new table called 'electronics' with only the Electronics products
CREATE electronics AS PROJECT #2, #4 FROM products WHERE #3 = "Electronics";

-- Then we can use this table in further operations
PROJECT #1, #2 FROM electronics WHERE #2 > 1000;