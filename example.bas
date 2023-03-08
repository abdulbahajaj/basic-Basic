LET red 16711680 
LET blue 255
FOR c 0 TO 74
IF 38 > c THEN LET color red ELSE LET color blue
PLOT (75-c) (75-c) color
PLOT c (75-c) color
PLOT 37 c color
PLOT 38 c color
PLOT c 37 color
PLOT c 38 color
NEXT c
