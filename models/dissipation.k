#
# Basic Chemical System
#
eq X --1--> .

# This make inits the molecule concentration everywhere
init X = 0 .

#
# The following specification are considered only in case of use
# of reaction diffusion system generator (eg. generators.tomatlabdiff) 
#

interval 0 --> 3 : 0.05 .

# Lattice specification
# grid <x0>,<y0>,<x1>,<y1>:<xdivisions>,<ydivisions>--><diffusionRatex>,<diffusionRatey>.
grid -0.5, -0.5, 0.5, 0.5 : 40,40 --> 0.05,0.05.
# area initialization
# area <molname>,<rectX0>,<rectY0>,<rectX1>,<rectY1>=<value>.
area X:0,0,0,0=1 .

