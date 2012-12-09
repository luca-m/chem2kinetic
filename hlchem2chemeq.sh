#!/bin/bash

if [ "$1" = '--help' ]; then
		echo -e "
SYNOPSIS
	
	Translate High Level Chemical Equation to corresponding Chemical Reaction. 
	High level Ce Language supports molecule parametrization that could be
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
		
		eq X(i) + MOL -- 2 --> X(j) . where i in 1..5 j in 1..5 with i!=j
		eq --> 2*Y(i,j) . where i in 1..3 j in 4..6 
		eq X(i) --> X(j) . where i in 1..3 j in 4..6 with i == j-1 
		init X(i) = 100 .  where i in 1..3 
		...
	
DESCRIPTION
	
	High Level Chemnical Reaction Specification Language EBNF:
	
		chemeq ::= [] | eq  chemeq .
		eq ::= 'eq' reagents rate products '.'  conditions .
		conditions ::= [] | ranges constraints .
		ranges ::= [] | 'where' [lit(X)] 'in' range ranges.
		rangesrest ::= [] |  [lit(X)] 'in' range rangesrest.
		range ::=  [num(N)] '.' '.' [num(NN)] .
		constraints ::= [] | 'with' constraint constraints .
		constraint ::= [lit(X)]  constraintrest .
		constraintrest ::= ( '!=' |'==' ) value.
		value ::= [lit(X)] | [num(Y)] | range | <math-expression> .
		reagents ::= [] | molecule reagentsrest .
		reagentsrest ::= [] | '+' molecule reagentsrest .
		products ::= reagents.
		rate ::= '-->' | '--' [num(N)] '-->'.
		molecule ::= mol | number '*' mol.
		mol ::= [cap(M)] molrest .
		molrest::= [] | '(' indexes ')' .
		indexes ::= [lit(X)] indexesrest .
		indexesrest ::= [] | ',' [lit(X)] indexesrest.
	
	Correctness:
		- Each molecule in Reagents must have a unique name. The same is valid for Products
 		- For each (unique) index specified into molecules MUST be specified a range.
		- Ranges specification of unused indexes are ILLEGAL
		- Constraints expressions cannot contain the index used in the left part of the expresstion 
		  in the right part. (We do not solve equations at the moment)
		- Illegal constraints are i=i (useless) and i!=i (make no sense)

EXAMPLES
	
	echo \"eq X(i) --> X(j). where i in 1..4 j in 1..4 with i!=j\" | $0 
		eq 1*X_1 --1--> 1*X_2 .
		eq 1*X_1 --1--> 1*X_3 .
		eq 1*X_1 --1--> 1*X_4 .
		eq 1*X_2 --1--> 1*X_1 .
		eq 1*X_2 --1--> 1*X_3 .
		eq 1*X_2 --1--> 1*X_4 .
		eq 1*X_3 --1--> 1*X_1 .
		eq 1*X_3 --1--> 1*X_2 .
		eq 1*X_3 --1--> 1*X_4 .
		eq 1*X_4 --1--> 1*X_1 .
		eq 1*X_4 --1--> 1*X_2 .
		eq 1*X_4 --1--> 1*X_3 .
	echo \"eq X(i) --> X(j) .  where i in 5..1 j in 1..5 with i == j-1\" |$0
		eq 1*X_4 --1--> 1*X_5 .
		eq 1*X_3 --1--> 1*X_4 .
		eq 1*X_2 --1--> 1*X_3 .
		eq 1*X_1 --1--> 1*X_2 .
EXIT STATUS

	Exit=0

AUTHOR

	luca.mella@studio.unibo.it

LICENSE

	Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA 3.0)

VERSION

	0.1
"
	exit 0
fi

java -Xms512m -cp 2p.jar alice.tuprolog.Agent HLCeL-Interpreter.pl silentstart.

