Chem2Kinetic
============
This project aims to convert a chemical system expressed with a proper Domain Specific Language (DSL) to a kinetic, 
rate equation based, system wich could be solved using standard numerical methodes and tools.

Author: luca.mella@studio.unibo.it

##### Notes
------------
This project is an academic project developed during a university course, so it is in an early development stage, in other words it's a draft :)

##### Future Work
------------
* Improve performance of HLCeL parser (now it use a test and set approach, extremely bovine, but wickier to implement)
* Implement reaction diffusion translator

##### Architecture
------------
The components involved in this tool are substantially a couple of parser and a couple of translators, 
organized in 2 level of abstraction:
* The basic level involve a parser (based other open source project Kinpy) which is able to parse a set of chemical equations expressed via CRSL (this is how I named this first DSL, its pretty ugly i know) and translate them to kinetic 
equation system (ODEs). At the moment 2 translator/code generators are implemented (scipy and matlab, this way you can also solve the ODE system).
* The higher level involve a compiler (written in Prolog) that can generate lot of chemical equations starting from an higher level specification language (HLCeL) which permit to parametrize chemical equations. 
This level could be useful if your chemical system represent a spatial system, where each node's molecules could be 
molded with different types (eg. N1_1 could means molecule type N in node [1,1] and so on). This way number of molecules and equations could grow fast, 
so an automaic generation of mols and eqs should be useful. 

##### Languages
------------
###### CRSL
Chemnical Reaction Specification Language EBNF:
* `chem ::= chemeq [inits] [interval] [gridspec] [areaspec].`
* `chemeq ::= "eq" eq chemeqrest .`
* `chemeqrest ::= [] | "eq" eq chemeqrest.`
* `eq ::= reagents rate products "." .`
* `reagents ::= molecule reagentsrest .`
* `reagentsrest ::= [] | "+" molecule reagentsrest.`
* `products ::= molecule productsrest .`
* `productsrest ::= [] | "+" molecule productsrest.`
* `rate ::= "--" number "-->" | "-->".`
* `molecule ::= number "*" literal | literal.`
* `interval ::= "intrerval" number "-->" number ":" number "."`
* `inits ::= "initval" literal "=" number "." inits | [] .`
* `areaspec ::= "area" literal "," number "," number "," number "," number "=" number "." areaspec | [].`
* `gridspec ::= "grid" " number "," number "," number "," number ":" " number "," number "." | []`

###### HLCeL
High Level Chemnical Reaction Specification Language EBNF:
* `chemeq ::= [] | eq chemeq .`
* `eq ::= 'eq' reagents rate products '.' conditions .`
* `conditions ::= [] | ranges constraints .`
* `ranges ::= [] | 'where' [lit(X)] 'in' range ranges.`
* `rangesrest ::= [] | [lit(X)] 'in' range rangesrest.`
* `range ::= [num(N)] '.' '.' [num(NN)] .`
* `constraints ::= [] | 'with' constraint constraints .`
* `constraint ::= [lit(X)] constraintrest .`
* `constraintrest ::= ( '!=' |'==' ) value.`
* `value ::= [lit(X)] | [num(Y)] | range | <math-expression> .`
* `reagents ::= [] | molecule reagentsrest .`
* `reagentsrest ::= [] | '+' molecule reagentsrest .`
* `products ::= reagents.`
* `rate ::= '-->' | '--' [num(N)] '-->'.`
* `molecule ::= mol | number '*' mol.`
* `mol ::= [cap(M)] molrest .`
* `molrest::= [] | '(' indexes ')' .`
* `indexes ::= [lit(X)] indexesrest .`
* `indexesrest ::= [] | ',' [lit(X)] indexesrest.`

Correctness:
* Each molecule in Reagents must have a unique name. The same is valid for Products
* For each (unique) index specified into molecules MUST be specified a range.
* Ranges specification of unused indexes are ILLEGAL
* Constraints expressions cannot contain the index used in the left part of the expresstion in the right part. (We do not solve equations at the moment)
* Illegal constraints are i=i (useless) and i!=i (make no sense)

##### Usage
------------
Simple script for automate translation from High Level Chemical Language (HLCeL) to ODEs System
Usage: `./translate.sh <HLCeL-source-file> <output-file> (matlab|scipy) (g|c)`
Where:
* `mablab` and `scipy` are the selectable target language
* `g` means ENABLE GRAPHIC OUTPUT
* `c` means ENABLE CSV OUTPUT (supported only with scipy at the moment)
NOTE: parameters are mandatory!

Othrerwise here there are all information needed for using these two tools..

###### chem2kinetic - CRSL to Solvable ODE System
------------
Translate Chemical Equation to the corresponding Reaction Rate Equations set.
Basically translate a Domain Specific Language (DSL) to a script
that solves the system of ODEs composed of chemical system's
kinetic equations.

Help:
* `-h`, `--help` show this help message and exit
* `-l` `--language=<STR>` Generator to use for code generation (at the moment "matlab" or "scipy")
* `-i <INPUTFILE>`, `--infile=INPUTFILE` [DEFAULT: stdin, "-" for stdin]
* `-o <OUTPUTFILE>`, `--outfile=OUTPUTFILE` [DEFAULT: stdout, "-" for stdoud]
* `-v`, `--verbose` Verbose output
* `-c`, `--csv` Csv output enabled
* `-g`, `--graphics` Graphics output enabled


Eg.

`python chem2kinetic.py -l scipy -i inputscript.k -o outputscript.py -g`

Then run:

`python outputscript.py`

For solving Rate Equation system

###### hlchem2chemeq - HLCeL to CRSL
------------
Translate High Level Chemical Equation to corresponding Chemical Reaction.
High level chEm Language supports molecule parametrization that could be
usefull to emulate spatial nodes through differents molecule types.
Chemical Reaction language can be processed further by 'chem2kinetic' for
obtaining a numerically solvable system of rate equation which is an
approximation of the chemical system.

The main introduced by this High Level chemical equation is the automation
of the generation of the chemical equations.
Roughtly speaking, if we want to model a spatial chemical system with more nodes
and we'd like to model also molecules migration, we probably need to model
nodes neighborhood throug an elevate number of \"point2point\" equation.

High Level Chemical Equation Language supports equation specification such as:

* `eq X(i) + MOL -- 2 --> X(j) . where i in 1..5 j in 1..5 with i!=j`
* `eq --> 2*Y(i,j) . where i in 1..3 j in 4..6`
* `...`
