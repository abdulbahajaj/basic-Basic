LET red 13583410
LET lightRed 16541523
LET blue 7574225 
LET white 16777215
LET brown 7355966
LET lightbrown 9393750
LET metal 10921638
LET lightGrey 12105912

LET height 400
LET width 400
LET seaLevel (height/4)*3

FOR x 0 TO width-1
FOR y 0 TO height-1
IF y > seaLevel THEN LET color blue ELSE LET color white
PLOT x y color
NEXT y
NEXT x

LET startAt width/20
LET diagonalLen width/6
LET sink 25
LET borderWidth 2

FOR c 0 TO diagonalLen

LET y (seaLevel +  c) - diagonalLen + sink

FOR f startAt + c TO width - (startAt + c) 
PLOT f y brown
NEXT f
LET x startAt + c
FOR depth 0 TO borderWidth
LET ywithDepth y + depth
PLOT x ywithDepth lightbrown
PLOT width - x ywithDepth lightbrown
NEXT depth

NEXT c


LET lowestX (startAt + diagonalLen)
LET y seaLevel + sink
FOR x lowestX TO (width - lowestX)
FOR depth 0 TO borderWidth
PLOT x y + depth   lightbrown
NEXT depth
NEXT x

LET boatTop seaLevel - diagonalLen + sink

FOR x startAt TO (width - startAt)
FOR depth 0 TO borderWidth
PLOT x boatTop-depth lightbrown
NEXT depth
NEXT x

LET poleXStart startAt*4+10
FOR y height/3-50 TO boatTop-3
PLOT poleXStart-1 y lightGrey
PLOT poleXStart-2 y lightGrey

FOR depth 0 TO 4
PLOT poleXStart+depth y metal
NEXT depth
NEXT y

LET flagEnd width/2 + poleXStart 
FOR y height/3-50 TO boatTop-80
FOR x poleXStart + 5 TO flagEnd
PLOT x y red
NEXT x
NEXT y

FOR depth 0 TO 2
FOR x poleXStart + 5 TO flagEnd
PLOT x y+depth lightRed
NEXT x
NEXT depth