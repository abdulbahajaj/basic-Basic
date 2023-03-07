0 FOR c 0 TO 74
10 IF 38 > c THEN LET color 16711680 ELSE LET color 255
20 PLOT (75-c) (75-c) color
30 PLOT c (75-c) color
40 PLOT 37 c color
50 PLOT 38 c color
60 PLOT c 37 color
70 PLOT c 38 color
80 NEXT c