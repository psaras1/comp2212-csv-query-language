-- Chained left merges with three tables
LEFT MERGE (LEFT MERGE A ON #1 WITH B) ON #1 WITH C;