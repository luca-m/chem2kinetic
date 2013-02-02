#
# Basic Chemical System
#
eq X --5--> .
eq -- 3 --> X .

# This make inits the molecule concentration everywhere
init X = 0 .

#
# The following specification are considered only in case of use
# of reaction diffusion system generator (eg. generators.tomatlabdiff) 
#

# Lattice specification
# grid <x0>,<y0>,<x1>,<y1>:<xdivisions>,<ydivisions>--><diffusionRatex>,<diffusionRatey>.
grid 0,0,6,6 : 20,20 --> 0.1,0.15.
# area initialization
# area <molname>,<rectX0>,<rectY0>,<rectX1>,<rectY1>=<value>.
area X:1,1,1,1=7000 .
area X:3,1,3,1=4000 .

