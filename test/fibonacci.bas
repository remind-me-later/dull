5   REM Compute the Nth Fibonacci number
10  "A" INPUT "Enter N: "; N
20  IF N < 0 THEN PRINT "N must be non-negative": END
30  IF N = 0 THEN PRINT "F(0) = 0": END
40  IF N = 1 THEN PRINT "F(1) = 1": END
50  LET A = 0
60  LET B = 1
70  FOR I = 2 TO N
80  LET C = A + B
90  LET A = B
100 LET B = C
110 NEXT I
120 PRINT "F("; N; ") = "; C
130 END