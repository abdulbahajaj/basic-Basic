LET red 16711680 
LET blue 7574225 
LET white 16777215
LET SKY 15919822
LET brown 7355966
LET lightbrown 9393750
LET metal 10921638

FOR x 0 TO 74
FOR y 0 TO 74
IF y > 50 THEN LET color blue ELSE LET color white
PLOT x y color
NEXT y
NEXT x

LET len 15
FOR x 0 TO len
FOR y x TO len
FOR f (5+x) TO 75-(5+x)
PLOT f (40+y) brown
NEXT f
PLOT (5+x) (40+y) lightbrown
PLOT 75-(5+x) (40+y) lightbrown
NEXT y
NEXT x

FOR x 20 TO 55 
PLOT x 55 lightbrown
NEXT x

FOR x 5 TO 70
PLOT x 40 lightbrown
NEXT x

FOR y 10 TO 39
PLOT 25 y metal
PLOT 24 y metal
NEXT y

FOR x 26 TO 55
FOR y 10 TO 25
PLOT x y red
NEXT y
NEXT x