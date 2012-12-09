:-load_library('alice.tuprolog.lib.DCGLibrary').

/*
 Tokenizer
 
 TODO: fix number bugs, support floating point number, optimize
*/
tokenize(Text,Tokens) :- 
                         atom_codes(Text,Codes), 
                         tokenizer(t,skip,Codes,[],Tokens).

tokenizer(Symb,Skip,LI,LO,Out):-
	phrase(Skip,LI,LO1),!,
    tokenizer(Symb,Skip,LO1,LO,Out).
tokenizer(Symb,Skip,LI,LO,[A|Out]):-
	P =.. [Symb,A], 
    phrase(P,LI,LO1),!,
    tokenizer(Symb,Skip,LO1,LO,Out).
tokenizer(_,_,L,L,[]).

skip --> [32]. % space
skip --> [13]. % CR \r
skip --> [10]. % NL \n
skip --> [9].  % TAB \t
skip --> [12].  % FF \f formfeed

tokens(['+','*','/','-->','--','-',',','.','init','where','(',')','in','eq','with','!=','==','=']).

t(TOK)        --> {tokens(Ts),member(TOK,Ts),atom_codes(TOK,X)},X.
t(cap(LIT))   --> cliteral(LIT).
t(lit(LIT))   --> literal(LIT).
t(num(NN))   --> t_numb(NUM), {atom_codes(N,NUM),((N='0',num_atom(NN,'00') );(N\='0',num_atom(NN,N)) )/**/}.

normchar(N) --> [N],{ascii_smallchar(N)}. % a..z A..Z
capchar(N) --> [N],{ascii_bigchar(N)}. % a..z A..Z
char(N) --> normchar(N).
char(N) --> capchar(N).

cliteral(A) --> capchar(N),lit2(L),{atom_codes(A,[N|L])}.
literal(A) --> normchar(N),lit(L),{atom_codes(A,[N|L])}.
lit([N|L2]) --> char(N),lit(L2).
lit([]) --> [].
lit2([N|L2]) --> char(N),lit2(L2).
lit2([N|L2]) --> t_digit(N),lit2(L2).
lit2([]) --> [].

ascii_smallchar(N):-N>96, N<123.
ascii_bigchar(N):-N>64, N<91.

t_digit(N) --> [N], {N>47,N<58}.
t_numb([D|L]) --> t_digit(D),t_numbz(L).
t_numbz([D|L]) --> t_digit(D),t_numbz(L).
t_numbz([]) --> [].

/*
Grammar

chemeq ::= [] | eq  chemeq | init chemeq .
eq ::= 'eq' reagents rate products '.'  conditions .
conditions ::= [] | ranges constraints .

ranges ::= [] | 'where' [lit(X)] 'in' range ranges.
rangesrest ::= [] |  [lit(X)] 'in' range rangesrest.
range ::=  [num(N)] '.' '.' [num(NN)] .

constraints ::= [] | 'with' constraint constraints .
constraint ::= [lit(X)]  constraintrest .
constraintrest ::= ( '!=' |'==' ) value.

value ::= [lit(X)] | [num(Y)] | range | <math-expression>.

reagents ::= [] | molecule reagentsrest .
reagentsrest ::= [] | '+' molecule reagentsrest .
products ::= reagents.

rate ::= '-->' | '--' [num(N)] '-->'.

molecule ::= mol | number '*' mol.

mol ::= [cap(M)] molrest .
molrest::= [] | '(' indexes ')' .
indexes ::= [lit(X)] indexesrest .
indexesrest ::= [] | ',' [lit(X)] indexesrest.

init ::= "init" mol "=" number "."  .

*/

% chemeq ::= [] | eq  chemeq .
chemeq([H|T]) --> eq(H) , chemeq(T) .
chemeq([H|T]) --> init(H) , chemeq(T) .
chemeq([]) --> [] .
% eq ::= 'eq' reagents rate products '.'  conditions .
eq( eq(R,RATE,P,CC) ) --> ['eq'],reagents(R),rate(RATE),products(P),['.'],conditions(CC) .
% conditions ::= [] | ranges constraints .
conditions(conditions(ranges([]),constraints([]))) --> [] .
conditions(conditions(ranges(H),constraints(T))) --> ranges(H), constraints(T) .
% ranges ::= [] | 'where' [lit(X)] 'in' range ranges.
% rangesrest ::= [] |  [lit(X)] 'in' range rangesrest.
% range ::=  [num(N)] '.' '.' [num(NN)] .
ranges([]) --> [] .
ranges([range(X,N,NN)|T]) --> ['where'] ,[lit(X)],['in'],range(range(N,NN)),rangeslist(T) .
rangeslist([]) --> [] .
rangeslist([range(X,N,NN)|T]) --> [lit(X)],['in'],range(range(N,NN)), rangeslist(T) .
range(range(N,NN))--> [num(N)] , ['.'], ['.'], [num(NN)].
% constraints ::= [] | 'with' constraint constraints .
constraints([]) --> [] .
constraints([H|T]) --> ['with'] , constraint(H) , constrlist(T) .
constrlist([]) --> [] .
constrlist([H|T]) -->  constraint(H) , constrlist(T) .
% constraint ::= [lit(X)]  constraintrest .
% constraintrest ::= ( '!=' |'==' ) value.
constraint(Z) --> [lit(X)] , constraintrest([X,Z]) .
constraintrest([X,equals(X,V)]) --> ['=='] , value(V) .
constraintrest([X,different(X,V)]) --> ['!='] , value(V) .
% value ::= [lit(X)] | [num(Y)] | range .
value(X) --> range(X) .
value(X) --> [lit(X)] .		% not LL1 here, need optimization
value(range(N,N)) --> [num(N)] .
value(expr(X)) --> expr1(X) .
% expression sublanguage
expr1(Z) --> expr2(X) , expr1rest(X,Z).
expr1rest(X,X) --> [].
expr1rest(X,Z) --> ['+'] , expr2(Y) , expr1rest(plus(X,Y),Z) .
expr1rest(X,Z) --> ['-'] , expr2(Y) , expr1rest(minus(X,Y),Z) .
expr2(Z) --> expr3(X) , expr2rest(X,Z).
expr2rest(X,X) --> [].
expr2rest(X,Z) --> ['*'] , expr3(Y) , expr2rest(mul(X,Y),Z) .
expr2rest(X,Z) --> ['/'] , expr3(Y) , expr2rest(div(X,Y),Z) .
expr3(N)-->[num(N)].
expr3(ind(I))-->[lit(I)].
expr3(X) --> expr1(X).
% reagents ::= [] | molecule reagentsrest .
% reagentsrest ::= [] | '+' molecule reagentsrest .
reagents([]) --> [] .
reagents([H|T]) --> molecule(H), reagentsrest(T) .
reagentsrest([]) --> [] .
reagentsrest([H|T]) --> ['+'] , molecule(H), reagentsrest(T) .
% products ::= reagents.
products(H) --> reagents(H).
% rate ::= '-->' | '--' [num(N)] '-->'.
rate(rate(RATE)) --> ['--'] , [num(RATE)] , ['-->'].
rate(rate(1)) --> ['-->'].
% molecule ::= mol | number '*' mol.
molecule(mol(N,M)) --> [num(N)] , ['*'] , mol(M).
molecule(mol(1,M)) --> mol(M).
% mol ::= [cap(M)] molrest .
% molrest::= [] | '(' indexes ')' .
mol(name(M,I)) --> [cap(M)] , molrest(I).
molrest([]) --> [].
molrest(I) --> ['('] , indexes(I) , [')'] .
% indexes ::= [lit(X)] indexesrest .
% indexesrest ::= [] | ',' [lit(X)] indexesrest.
indexes([X|T]) --> [lit(X)] , indexesrest(T) .
indexesrest([]) --> [].
indexesrest([X|T]) --> [','] , [lit(X)] , indexesrest(T).
% init ::= "init" mol "=" number "."  .
init(init(M,N,CONDS)) --> ['init'] , mol(M) , ['='] , [num(N)] ,['.'], conditions(CONDS)  .
/*
 Parser
*/
parse(S,A):- tokenize(S,T),phrase(chemeq(A),T),!.
%
% Test
%
parseTest(S,SCOPE) :- tokenize(S,T),phrase(SCOPE,T),!.
parseTest(A):- parseTest('eq X+Xas --> Y .',chemeq(A)).
parseTest(A):- parseTest('eq X --> X.',chemeq(A)).
parseTest(A):- parseTest('eq X+Xas --> Y .  where i in 1..2',chemeq(A)).
parseTest(A):- parseTest('eq X+Xas --> Y .  where i in 1..2 j in 3..4',chemeq(A)).
parseTest(A):- parseTest('eq X+Xas --> Y .  where i in 1..2 j in 3..4 with i != k j == i ',chemeq(A)).
parseTest(A):- parseTest('eq X+Xas --> Y .  where i in 1..2 j in 3..4 with i == 1+2*j ',chemeq(A)).
parseTest(A):- parseTest('eq X+Xas --> Y .  where i in 1..2 j in 3..4 with i != k j == i  eq -- 2 --> W.',chemeq(A)).
parseTest(A):- parseTest('eq X --> . eq --> X.',chemeq(A)).
parseTest(A):- parseTest('eq X --> . eq --> X. init X=0 . eq Y --> M.',chemeq(A)).
/*
Checker
 
 Rules:	
 	- Each molecule in Reagents must have a unique name. The same is valid for Products
 	- For each (unique) index specified into molecules MUST be specified a range.
	- Ranges specification of unused indexes are ILLEGAL
	- Range values must be ordered from litte to bigger. Eg.  ... where i in 4..3  is considered ILLEGAL
	- Illegal constraints are i=i (useless) and i!=i (make no sense)
	- Constraints expressions cannot contain the index used in the left part of the expresstion in the right part. (We do not solve equations at the moment)
	- Question: i=j make sense? 
*/
% [ eq([mol(1,name('X',[]))],rate(1),[mol(1,name('Y',[]))],[ranges([range(i,1,2)]),constraints([different(i,k),equals(j,i)])])]

check([]).
check([H|T]):-checkE(H),!,check(T).
% Checking Equations: 
%	check for Reagents correctness
%	check for non negative rates
%	check for Products correctness
% 	check for Conditions correctness
checkE( eq(REACT,rate(R),PROD,COND) ):- R>=0, checkR(REACT,[],[],NMS1,I1), 
					checkR(PROD,[],[],NMS2,I2),
					append(I1,I2,INDXS),
					( (INDXS\=[],setof(X,member(X,INDXS),INDEXES));(INDEXES=[])),
					!,checkC(INDEXES,COND).
% Checking Initial Values: 
%	check for Reagents correctness
%	check for non negative rates
%	check for Products correctness
% 	check for Conditions correctness
checkE( init(name(MOLNAME,IDXS),NUM,COND) ):- 
					( (IDXS\=[],setof(X,member(X,IDXS),INDEXES));(INDEXES=[])),
					!,checkC(INDEXES,COND).
% Checking Reactions/Product: 
%	check for molecule unique name 
%	check for molecule number > 0 
%	determine the set of used indexes
checkR([],NMS,INDXS,NMS,INDXS).
checkR([mol(N,name(NAME,I))|T],NMS,INDXS,NAMES2,INDEXES):- N>0, not(member(NAME,NMS)),append([NAME],NMS,NMSS),
							   append(I,INDXS,II),checkR(T,NMSS,II,NAMES2,INDEXES).
% Checking Conditions: 
%	check for ranges specification correctness
%	check constraints specification correctness
checkC(INDEXES,conditions(ranges(RANGES),constraints(CONDS))):- 
		checkRanges(INDEXES,RANGES), 
		checkConstr(INDEXES,CONDS,RANGES).

% Checking Ranges: 
%	check if all index has an associated range
%	check ranges correctness
checkRanges([],[]).
checkRanges(INDEXES,RANGES):- 
		member(I,INDEXES), member(range(I,N,NN),RANGES), /*N=<NN,*/
		append([I],INDXS,INDEXES)/* its a remove*/, 
		append([range(I,N,NN)], RNGS,RANGES) /*its a remove*/,
		checkRanges(INDXS,RNGS),!.
% Checking Constraints: 
%	check index consistency
%	check ranges correctness
%	TBD: check ranges interval referred to index range interval
checkConstr(_,[],_).
checkConstr(INDEXES,equals(I,J),RANGES) :-
	 	checkValue(INDEXES,I,J).
checkConstr(INDEXES,different(I,J),RANGES):-
		checkValue(INDEXES,I,J).
checkConstr(INDEXES,different(I,range(N,NN)),RANGES):- /*N=<NN,*/
		member(I,INDEXES).
checkConstr(INDEXES,[H|T],RANGES):-
		checkConstr(INDEXES,H,RANGES),!,
		checkConstr(INDEXES,T,RANGES).
% Check value in case of index 
checkValue(INDEXES,I,VAL):-
		functor(VAL,VAL,0), I\=VAL, 
	 	member(VAL,INDEXES),member(I,INDEXES).
checkValue(INDEXES,I,range(N,N)):-member(I,INDEXES).
% Check value in case of expression
checkValue(INDEXES,I,expr(E)):- member(I,INDEXES), checkExpr(INDEXES,I,E).
checkExpr(INDEXES,I,plus(X,Y)):- checkExpr(INDEXES,I,X), checkExpr(INDEXES,I,Y).
checkExpr(INDEXES,I,minus(X,Y)):- checkExpr(INDEXES,I,X), checkExpr(INDEXES,I,Y).
checkExpr(INDEXES,I,mul(X,Y)):- checkExpr(INDEXES,I,X), checkExpr(INDEXES,I,Y).
checkExpr(INDEXES,I,div(X,Y)):- checkExpr(INDEXES,I,X), checkExpr(INDEXES,I,Y).
checkExpr(INDEXES,I,ind(X)):- X\=I, member(X,INDEXES).
checkExpr(INDEXES,I,X):- number(X).
%
% TESTS
%
checkConstrTest:-checkConstr([i,j],[equals(i,j)],[]),not(checkConstr([i],[equals(i,j)],[])),
		 checkConstr([i,j],[different(i,range(3,3))],[]), not(checkConstr([i,j],[different(i,k)],[])),
		 checkConstr([i,j],[equals(i,range(1,1))],[]), not( checkConstr([i,j],[equals(i,range(2,4))],[])).
checkRangesTest:-checkRanges([i,j],[range(i,1,2),range(j,1,2)]), not(checkRanges([i,j],[range(i,1,2),range(i,1,2)])),
		 not(checkRanges([j],[range(i,1,2),range(j,1,2)])) , not(checkRanges([j],[range(j,2,1)])),
		 not(checkRanges([i],[range(i,2,1)],[])).
checkCTest:- checkC([i,j],conditions(ranges([range(i,1,2),range(j,1,2)]),constraints([equals(i,j),different(i,range(1,2))]))).
checkCTest:- checkC([i,j],conditions(ranges([range(i,1,2),range(j,1,2)]),constraints([equals(i,expr(plus(1,mul(2,ind(j))))),different(i,range(1,2))]))).
checkRTest:-checkR([mol(1,name('X',[i]))],[],[],_,_), 
	    not(checkR( [mol(1,name('X',[i])),mol(1,name('X',[j]))],[],[],_,_)),
	    checkR( [mol(1,name('X',[i])),mol(1,name('Y',[j]))],[],[],_,_).
checkEqTest:-checkEq(eq([mol(1,name('X',[i]))],rate(1),[mol(1,name('Y',[]))],conditions(ranges([range(i,1,2)]),constraints([different(i,range(2,2))])))).

/*
 Translator
 
 TODO: optimization, avoid to regenerate valid index each eq. Use memorization support?
*/
translate([]).
translate([H|T]):- translate(H), translate(T).
translate( eq(REAGENTS,rate(R),PRODUCTS,conditions(ranges(RANGES),constraints(CONSTR))) ):-
		expandRanges(RANGES,CONSTR,RAWRANGES), 
		findall(I,member(range(I,_),RAWRANGES),INDEXES),!,
		spawnEqs(REAGENTS,R,PRODUCTS,RAWRANGES,CONSTR,INDEXES).
translate( init(MOL,VAL,conditions(ranges(RANGES),constraints(CONSTR))) ):-
		expandRanges(RANGES,CONSTR,RAWRANGES), 
		findall(I,member(range(I,_),RAWRANGES),INDEXES),!,
		spawnInits(MOL,VAL,RAWRANGES,CONSTR,INDEXES).
% Spawn an equation for each combination of indexes values
spawnEqs(REAGENTS,R,PRODUCTS,RAWRANGES,CONSTR,INDEXES):-
		findall(IV,validcombination(INDEXES,RAWRANGES,IV,CONSTR),INDEXVALS),!,
		printeqs(REAGENTS,R,PRODUCTS,INDEXVALS).
spawnInits(MOL,VALUE,RAWRANGES,CONSTR,INDEXES):-
		findall(IV,validcombination(INDEXES,RAWRANGES,IV,CONSTR),INDEXVALS),!,
		printinit(MOL,VALUE,INDEXVALS).
%
% PRINT AN EQUATION
%
printeqs(REAGENTS,R,PRODUCTS,[]):-!.
printeqs(REAGENTS,R,PRODUCTS,[IVALS|T]):-
	printeq(REAGENTS,R,PRODUCTS,IVALS),
	printeqs(REAGENTS,R,PRODUCTS,T).
printeq(REAGENTS,R,PRODUCTS,IVALS):-
	print("eq "), printmols(REAGENTS,IVALS), 
	print(" --"),print(R),print("--> "),
	printmols(PRODUCTS,IVALS),print(" ."),nl.
printinit( MOLNAME, VALUE, []):-!.
printinit( MOLNAME, VALUE, [IVALS|T]):-
	print("init "), printmolname(MOLNAME,IVALS),
	print(" = "), print(VALUE), print(" ."),nl,
	printinit( MOLNAME, VALUE, T).
printmols([],_).
printmols([mol(N,name(NAME,I))|T],IVALS):- 
	print(N),print("*"), printmolname(name(NAME,I),IVALS), 
	printmols1(T,IVALS).
printmols1([],_).
printmols1([mol(N,name(NAME,I))|T],IVALS):- 
	print(" + "), print(N), print("*"), printmolname(name(NAME,I),IVALS),
	printmols1(T,IVALS).
printmolname(name(NAME,I),IVALS):- print(NAME), printindex(I,IVALS).
printindex([],_).
printindex([I|T],IVALS):- not(member(ind(I,V),IVALS)).
printindex([I|T],IVALS):- member(ind(I,V),IVALS),!,print("_"),/*print(I),*/print(V),printindex(T,IVALS).
%
% INDEX VALUES GENERATION
%
% Obtain a valid combination of index values, check for equals(i,k) and different(i,j)
% TODO: avoid generate and test, it's pretty bovine and performance unfriendly
validcombination(INDXS,RAWRANGES,VALS,CONSTR):- 
		combination(INDXS,RAWRANGES,VALS),
		valid(VALS,CONSTR).
combination([],_,[]).
combination([H|T],RAWRANGES,[ind(H,V)|TT]):- 
		member2(RAWRANGES,range(H,VALUES),RR), 
		member2(VALUES,V,VALS), 
		append(RR,[range(H,VALS)],RAWRANGES1),
		combination(T,RAWRANGES1,TT) .
% Check for equals(i,_) and different(i,_)
% TODO: optimize it
valid(IDXS,[]).
valid(IDXS,[equals(I,expr(E))|T]):- !, 
		member(ind(I,V),IDXS), 
		exprValue(IDXS, E, V), 
		valid(IDXS,T).
valid(IDXS,[equals(I,J)|T]):- !,
		member(ind(I,V),IDXS), 
		member(ind(J,V),IDXS), 
		valid(IDXS,T).
valid(IDXS,[different(I,expr(E))|T]):- !, 
		member(ind(I,V),IDXS), 
		not( exprValue(IDXS, E, V) ), 
		valid(IDXS,T).
valid(IDXS,[different(I,J)|T]):- !,
		member(ind(I,V),IDXS), 
		not(member(ind(J,V),IDXS)),
		valid(IDXS,T).
valid(IDXS,[H|T]):-
		valid(IDXS,T).

exprValue(IDXS , plus(X,Y), VAL):- exprValue(IDXS , X, XX), exprValue(IDXS , Y, YY), VAL is XX + YY.
exprValue(IDXS , minus(X,Y), VAL):- exprValue(IDXS , X, XX), exprValue(IDXS , Y, YY), VAL is XX - YY.
exprValue(IDXS , mul(X,Y), VAL):- exprValue(IDXS , X, XX), exprValue(IDXS , Y, YY), VAL is XX * YY.
exprValue(IDXS , div(X,Y), VAL):- exprValue(IDXS , X, XX), exprValue(IDXS , Y, YY), VAL is XX / YY.
exprValue(IDXS , ind(I), VAL):- member(ind(I,VAL),IDXS),!.
exprValue(IDXS , N, N):- number(N).
%
% RANGE EXPANSION
%
% Get the index ranges in raw mode, also considering range constraints (Eg. index "i" != 3..5 )
expandRanges( RANGES,CONSTR, FINALRANGES ):- 
		findall(I,member(range(I,_,_),RANGES) , INDEXES ),	/*finding indexes names - unique due to previous checking*/
		findall(R,expandRange(RANGES,CONSTR,R),RANGES1),
		expRanges(INDEXES,RANGES1,FINALRANGES).
% Find valid ranges for each indexes
expandRange([H|T],CONDS,OUT):- 
		member(R,[H|T]), 
		expandRange(R,CONDS,OUT).
expandRange(range(IDX,N,NN),CONDS,range(IDX,N,NN)):-
		not( dropFirst( different(IDX,range(N1,NN1)) ,CONDS,CONDSREST) ).
expandRange(range(IDX,N,NN),CONDS,OUT):- 
		dropFirst(different(IDX,range(N1,NN1)),CONDS,CONDSREST),
		( (N>NN,!,NY=NN,NNY=N/*,REVERSE=true*/);(NY=N,NNY=NN/*,REVERSE=false*/) ),
		trimRange(range(IDX,NY,NNY),range(N1,NN1), NEWRANGES),
		member(X,NEWRANGES),
		expandRange(X,CONDSREST,OUT ).
% Trim out the second range to the first one. A list of new range will be calculated
trimRange(range(IDX,N,NN),range(N1,NN1), [range(IDX,N,X)] ):- N<N1,NN=<NN1, NEWN is N1-1, min(NN,NEWN,X).
trimRange(range(IDX,N,NN),range(N1,NN1), [] ):- N>=N1 , NN=<NN1.
trimRange(range(IDX,N,NN),range(N1,NN1), [range(IDX,N,NEWN),range(IDX,NEWNN,NN)] ):- N<N1 , NN>NN1 , NEWN is N1-1,NEWNN is NN1+1.
trimRange(range(IDX,N,NN),range(N1,NN1), [range(IDX,X,NN)] ):- N>=N1,NN>=NN1,NEWNN is NN1+1 , max(N,NEWNN,X).
% Expand index ranges and get the list of number that represent the index values. 
expRanges([],_,[]).
expRanges([I|T],RANGES,[H|TT]):- 
		expRange(I,RANGES,[],H), 
		expRanges(T,RANGES,TT).
expRange(IDX,[],X,range(IDX,X)) /*:-quicksort(X,'<',Y)*/.
expRange(IDX,[range(IDX,N,NN)|RANGES],X,OUT):-!,
		range(N,NN,Y), append(X,Y,Z), 
		expRange(IDX,RANGES,Z,OUT). 
expRange(IDX,[range(I,_,_)|RANGES],X,OUT):-
		IDX\=I, 
		expRange(IDX,RANGES,X,OUT).

%
% Misc and Utils
%
min(X,Y,X):-X<Y.
min(X,Y,Y):-X>=Y.
max(X,Y,X):-X>Y.
max(X,Y,Y):-X=<Y.
% expand a range
range(N,NN,[N]):- N==NN,!.
range(N,NN,[N|T]):- N>NN, N1 is N-1 ,range(N1,NN,T).
range(N,NN,[N|T]):- N<NN, N1 is N+1 ,range(N1,NN,T).
% drop first element
dropFirst(X,[X|T],T):-!.
dropFirst(X,[H|Xs],[H|L]):-dropFirst(X,Xs,L).
% member2(List, Elem, ListWithoutElem)
member2([X|Xs],X,Xs).
member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
%
% TEST
% 	[eq([mol(1,name('X',[i]))],rate(1),[mol(1,name('X',[j]))],conditions(ranges([range(i,1,3),range(j,1,3)]),constraints([different(i,j)])))]
testExpandRanges(R):-expandRanges( [range(i,1,10),range(k,4,7)] , [different(i,range(5,7))] ,R ).

testTranslate:-translate( eq( [mol(1,name('X',[i,j]))] ,rate(1), [mol(1,name('X',[j]))] , conditions( ranges( [ range(i,1,3),range(j,1,3) ] ),constraints([different(i,j)]))) ).

/*
 INTERPRETER
*/
interpreter(STRING):- 
	((parse(STRING,AST),!);(print("Parse Error!"),nl)), 
	((check(AST),!);(print("Semantic Error"),nl)), 
	translate(AST).

testInt :- interpreter('eq X(i) --> X(j) .  where i in 3..1 j in 1..3 with i != j ').
testInt2 :- interpreter('eq X --> X . ').

%
% Start
%
start:- print("Eq specification:"),nl, 
	read_chars('stdin',CHARS),nl,
	atom_chars(CODE,CHARS),
	interpreter(CODE).
silentstart:- read_chars('stdin',CHARS),nl,
	atom_chars(CODE,CHARS),
	interpreter(CODE).

loadfile:-print("Enter filepath: "),
	read_chars('stdin',CHARS),nl,
	atom_chars(STRING,CHARS),
	text_from_file(STRING,CODE), 
	interpreter(CODE).
%
% Read Utils
%
read_chars(FILE,L):- see(FILE), get_chars(L), seen.
get_chars(L):- get(T),!, process(T,L).
process(-1,[]):-!.
process(C,[C|L]):-get_chars(L).
