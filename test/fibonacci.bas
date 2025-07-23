5   REM Compute the Nth Fibonacci number
10  "A" INPUT "Enter N: "; N
50  LET A = 0, B = 1
70  FOR I = 2 TO N
80  LET C = A + B, A = B, B = C
110 NEXT I
120 PRINT "F("; N; ") = "; C
130 END