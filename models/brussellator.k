#
# Brussellator model
#

eq A -- 1 --> X .
eq 2*X + Y -- 1 --> 3*X .
eq B + X --1--> Y + D .
eq X --1--> E .

init A = 1 .
init B = 1 .
init D = 1 .
init E = 0 .
init Y = 1 . 
init X = 1 .

interval 0-->5:0.05.

grid -20,-20,20,20:40,40-->0.25,0.25 .


