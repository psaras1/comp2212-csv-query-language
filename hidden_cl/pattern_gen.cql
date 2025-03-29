-- Create a table with the pattern
CREATE temp AS
  SELECT #1, #1, #1 FROM A;

-- Then use COPY with CONSTANT to interleave constants
COPY #1 WITH CONSTANT "foo" FROM temp;