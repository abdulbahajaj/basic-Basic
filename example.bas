LET red 16711680 
LET blue 255
LET width 400
LET halfWidth width/2

FOR c 0 TO width-1
IF halfWidth > c THEN LET color red ELSE LET color blue
PLOT (width-c) (width-c) color
PLOT c (width-c) color
PLOT halfWidth c color
PLOT halfWidth+1 c color
PLOT c halfWidth color
PLOT c halfWidth+1 color
NEXT c