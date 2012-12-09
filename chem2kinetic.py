#!/usr/bin/python
"""
SYNOPSIS
	
	Translate Chemical Equation  to the corresponding Reaction Rate Equations set. 
	Basically translate a Domain Specific Language (DSL) to a script 
	that solves the system of ODEs composed of chemical system's
	kinetic equations.
	
	Credits:
	This software is a refactoring and an extension of Kinpy 
		site = http://code.google.com/p/kinpy/
		author = akshaysrinivasan@gmail.com
		license = GPLv2
	
DESCRIPTION
	
	Chemnical Reaction Specification Language EBNF:
	
	chem ::= chemeq [inits] [interval] .
	chemeq ::= "eq" eq chemeqrest .
	chemeqrest ::= [] | "eq" eq chemeqrest.
	eq ::= reagents rate products "." .
	reagents ::= molecule reagentsrest .
	reagentsrest ::= [] | "+" molecule reagentsrest.
	products ::= molecule productsrest .
	productsrest ::= [] | "+" molecule productsrest.
	rate ::= "--" number "-->" | "-->".
	molecule ::= number "*" literal | literal.
	interval ::= "intrerval" number "-->" number ":" number "."
	inits ::= "init" literal "=" number "." inits | [] .
	
EXAMPLES
	
	Help:
		-h, --help		show this help message and exit
		-l --language		Generator to use for code generation
		-i INPUT FILE, --infile=INPUT FILE
							Input File (DEFAULT: stdin,, "-" for stdin)
		-o OUTPUT FILE, --outfile=OUTPUT FILE
							Input File (DEFAULT: stdout, "-" for stdoud)
		-v, --verbose		Verbose output
		-c, --csv			Csv output enabled
		-g, --graphics		Graphics output enabled
	
	Eg.
		python chem2kinetic.py -l scipy -i inputscript.k -o outputscript.py -g
	Then run:
		python outputscript.py
	For solving Rate Equation system

EXIT STATUS

	Exit=-2	when cant load a valid code generator
	Exit=-1	when errors detected during translation
	Exit=0	if all ok

AUTHOR

	luca.mella@studio.unibo.it

LICENSE

	Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA 3.0)

VERSION

	0.2
"""
import sys, string, re, os
from optparse import OptionParser
from inspect import stack

import environment

#
# Data Structures
#
env = environment.Environment()
#
# Tokenizer
#
EQ_KEYWORD="eq"
INTERVAL_KEYWORD="interval"
INIT_KEYWORD="init"
COMMENT_LINE_PREFIX="#"
END_LINE="."
PLUS="+"
EQUALS="="
STEPSEPARATOR=":"
MULTIPLY="*"
ARROWBEGIN="--"
ARROWEND="-->"
NUMBER="\d+\.\d+|\d+"
LITERAL="[A-Z]\\w+|[A-Z]"
TOKENIZER_REGEX="("+EQ_KEYWORD+")|("+INIT_KEYWORD+")|("+INTERVAL_KEYWORD+")|("+ARROWEND+")|("+ARROWBEGIN+")|("+LITERAL+")|("+NUMBER+")|(\\"+PLUS+")|(\\"+MULTIPLY+")|("+EQUALS+")|("+COMMENT_LINE_PREFIX+".*)|(\\"+END_LINE+")|("+STEPSEPARATOR+")"
""" 
Tokenize source code
"""
def tokenize(text):
	tokens=list()
	lines = text.split("\n")
	if env.verbose:
		print "Reactions:"
	for linenum in range(1,len(lines)+1):
		tok=tokenizeline(lines[linenum-1],linenum)
		tokens.extend(tok)
	return tokens
""" 
Tokenize a line of code
"""
def tokenizeline(text,linenum):
	tokens=list()
	p=re.compile(TOKENIZER_REGEX)
	l=p.findall(text)
	for group in l:
		tmp=[tok for tok in group if tok != '' and not tok.startswith(COMMENT_LINE_PREFIX) ]
		for tok in tmp:
			tokens.append(tok)
			env.tok_lines.append(linenum)
	if env.verbose:
		if len(tokens)>0:
			print "%i: %s" % (linenum,"".join(tokens))
	return tokens
def skip(tokens):
	return [x for x in tokens if not x.startswith(COMMENT_LINE_PREFIX)]
#
# Parsing and Translating
#
""" 
 Parse Molecule
 molecule ::= number "*" literal | literal.
"""
def parse_molecule(tokens, i , mol_list):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	else:
		mol=None
		stoi=None
		if i+2 < len(tokens) and is_number( tokens[i] ):
			if tokens[i+1]==MULTIPLY and is_literal(tokens[i+2]):
				mol = tokens[i+2]
				stoi = int(tokens[i])
				i+=3
			else:
				env.abort(stack()[0][3]+": Wrong Molecule Specification. @line:"+str(env.tok_lines[i]))
				print >> sys.stderr, env.errormsg
				return i
		elif is_literal(tokens[i]):
			mol = tokens[i]
			stoi = 1
			i+=1
		else:
			env.abort(stack()[0][3]+": Missing molecule. @line:"+str(env.tok_lines[i]))
			print >> sys.stderr, env.errormsg
			return i
			
		if not env.chem_dict.has_key(mol) :
			# Register the new Molecule
			env.inc_mol_counter()
			env.chem_dict.update({mol : env.mol_counter})
			env.reac_dict.update({env.chem_dict[mol] : ""})
			env.chem_init_dict[mol]=0	# initial value = 0 (default)
			
		mol_list.append(stoi)
		mol_list.append(mol)
	return i
""" 
 Parse Rate
 rate ::= "--" [number] "-->" | "-->".
"""
def parse_rate(tokens,i, rate_list):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	else:
		rate=1.0
		if i+2 < len(tokens) and tokens[i]==ARROWBEGIN:
			if is_number(tokens[i+1]) :
				if tokens[i+2]==ARROWEND:
					rate=float(tokens[i+1])
					i+=3
				else:
					env.abort(stack()[0][3]+": Missing '"+ARROWEND+"'  @line:"+str(env.tok_lines[i]))
					print >> sys.stderr, env.errormsg
					return i
			else:
				env.abort(stack()[0][3]+": Missing Reaction Rate @line:"+str(env.tok_lines[i]))
				print >> sys.stderr, env.errormsg
				return i
				
		elif tokens[i]==ARROWEND:
			i+=1
		else:
			env.abort(stack()[0][3]+": Wrong Reaction Rate @line:"+str(env.tok_lines[i]))
			if env.verbose:
				print >> sys.stderr, env.errormsg
			return i
		rate_list.append(rate)
	return i

""" 
 Parse Products
 productsrest ::= [] | "+" molecule productsrest.
"""
def parse_productsrest(tokens, i , prod , mol_prod ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	elif tokens[i]==PLUS:
		mol_list = list()
		i+=1
		i=parse_molecule(tokens, i , mol_list)
		if env.aborted:
			return i
		prod.append(mol_list)
		mol = mol_list[1]  # name
		mol_prod.append(mol)
		i=parse_productsrest(tokens, i, prod , mol_prod )
	else:
		# unknown token
		pass
	return i
""" 
 Parse Products
 products ::= molecule productsrest .
"""
def parse_products(tokens, i , prod , mol_prod ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	else:
		mol_list = list()
		i = parse_molecule(tokens, i , mol_list )
		if env.aborted:
			return i
		prod.append(mol_list)
		mol = mol_list[1]  # name
		mol_prod.append(mol)
		i = parse_productsrest(tokens, i, prod , mol_prod )
	return i
""" 
 Parse Reagents
 reagentsrest ::= [] | "+" molecule reagentsrest.
"""
def parse_regentsrest(tokens, i, reac , mol_reac  ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	elif tokens[i]==PLUS:
		i+=1
		mol_list = list()
		i=parse_molecule(tokens, i, mol_list )
		if env.aborted:
			return i
		reac.append(mol_list)
		mol = mol_list[1] # name
		mol_reac.append(mol)
		i=parse_regentsrest(tokens, i , reac , mol_reac  )
	else:
		# unknown token
		pass
	return i
""" 
 Parse Reagents
 reagents ::= molecule reagentsrest .
"""
def parse_regents(tokens, i , reac , mol_reac  ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	else:
		mol_list = list()
		i = parse_molecule(tokens, i , mol_list)
		if env.aborted:
			return i
		reac.append(mol_list)
		mol = mol_list[1] # name
		mol_reac.append(mol)
		i = parse_regentsrest(tokens, i, reac , mol_reac )
	return i
""" 
 Parse Chemical Equation
 eq ::= reagents rate products "." .
"""
def parse_eq(tokens, i ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	oldi=i # need it for printing complete equation
	reac = list()
	reac_args = list()
	mol_reac = list()
	mol_prod = list()
	prod = list()
	rate_list= list()
	rate_str = ""
	v_str = ""

	if i>=len(tokens):
		env.abort(stack()[0][3]+": Missing chemical equation after keyword 'eq' @line:"+str(env.tok_lines[tokens[i-1]]))
		print >> sys.stderr, env.errormsg
		return len(tokens)
	
	if not tokens[i]==ARROWBEGIN and not tokens[i]==ARROWEND:
		i = parse_regents(tokens, i , reac , mol_reac )
		if env.aborted:
			return i
	i = parse_rate(tokens, i , rate_list )
	if env.aborted:
		return i
	if not tokens[i]==END_LINE:
		i = parse_products(tokens, i , prod , mol_prod )
		if env.aborted:
			return i
	if tokens[i]==END_LINE:
		i+=1
	else:
		env.abort(stack()[0][3]+": missing \".\" at the end of reaction. @line:"+str(env.tok_lines[tokens[i-1]]))
		if env.verbose:
			print >> sys.stderr, env.errormsg
		return i
	
	sourcegen.prepare_eq(env, mol_reac, mol_prod, reac, prod, rate_list, "".join(tokens[oldi:i]) )

	return i
""" 
 Parse Chemical Equations
 chemeqrest ::= [] | eq chemeqrest.
"""
def parse_chemeqrest(tokens , i ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	elif tokens[i]==EQ_KEYWORD :
		i=parse_eq(tokens, i+1)
		if env.aborted:
			return i
		i=parse_chemeqrest(tokens, i)
	elif not tokens[i]==INIT_KEYWORD and not tokens[i]==INTERVAL_KEYWORD:
		env.abort(stack()[0][3]+": Missing '"+EQ_KEYWORD+"' keyword @line:"+str(env.tok_lines[i]))
		print env.errormsg
	return i
""" 
 Parse Chemical Equations
 chemeq ::= eq chemeqrest .
"""
def parse_chemeq(tokens , i ):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	elif tokens[i]==EQ_KEYWORD :
		i=parse_eq(tokens, i+1)
		if env.aborted:
			return i
		i=parse_chemeqrest(tokens, i)
	return i
"""
 Parse Initial Value Specification
 inits ::= "initval" literal "=" number "." inits | [] .
"""
def parse_inits(tokens,i):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	elif i<len(tokens)-4 and tokens[i]==INIT_KEYWORD and \
			is_literal(tokens[i+1]) and tokens[i+2]==EQUALS and \
			is_number(tokens[i+3]) and tokens[i+4]==END_LINE:
		if env.chem_dict.has_key( tokens[i+1] ):
			env.chem_init_dict[ tokens[i+1] ] = float(tokens[i+3])
		else:
			env.abort(stack()[0][3]+": Cant initialize an unknown Molecule. @line:"+str(env.tok_lines[i]))
			print env.errormsg
			return i+1
		i+=5
		i=parse_inits(tokens,i)
	else:
		# Unknown token
		pass
		#env.abort(stack()[0][3]+": interval specification not correct. @line:"+str(env.tok_lines[tokens[i-1]]))
		#if env.verbose:
		#	print env.errormsg
	return i
"""
 Parse Interval Specification
 interval ::= "intrerval" number "-->" number ":" number "."
"""
def parse_interval(tokens,i):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)
	elif i<len(tokens)-6 and tokens[i]==INTERVAL_KEYWORD and \
			is_number(tokens[i+1]) and tokens[i+2]==ARROWEND and \
			is_number(tokens[i+3]) and tokens[i+4]==STEPSEPARATOR and \
			is_number(tokens[i+5]) and tokens[i+6]==END_LINE:
		env.set_tbegin(float(tokens[i+1]))
		env.set_tend(float(tokens[i+3]))
		if env.tbegin>= env.tend :
			env.abort(stack()[0][3]+": tbegin >= tend !!! @line:"+str(env.tok_lines[i+1]))
			print >> sys.stderr,  env.errormsg
			return i+1
		env.set_step(float(tokens[i+5]))
		if env.step > (env.tend - env.tbegin):
			env.abort(stack()[0][3]+": Step is bigger than interval! @line:"+str(env.tok_lines[i+5]))
			print env.errormsg
			return i+5
		i+=7
	else:
		#env.abort(stack()[0][3]+": missing interval specification. @line:"+str(env.tok_lines[tokens[i-1]]))
		print "Info: default interval: %f --> %f : %f " % (env.tbegin,env.tend,env.step)
	return i
"""
 Parse Chemical Language
 chem ::= chemeq [inits] [interval] .
"""
def parse_chem(tokens,i):
	if env.verbose:
		print stack()[0][3]+" with token n."+str(i)+" '"+str(tokens[i])+"'"
	if env.aborted:
		return i
	elif i>=len(tokens):
		return len(tokens)

	i=parse_chemeq(tokens,i)
	i=parse_inits(tokens,i)
	i=parse_interval(tokens,i)
	return i
""" """
def parse( tokens ):
	i=parse_chem(tokens,0)
	if i < len(tokens):
		env.abort(stack()[0][3]+": Error @line:"+str(env.tok_lines[i]))
		print >> sys.stderr, env.errormsg
	return i

def is_number(s):
	try:
		float(s)
		return True
	except ValueError:
		return False

def is_literal(s):
	if not s==INIT_KEYWORD and not s==INTERVAL_KEYWORD and not s==EQ_KEYWORD \
			and len(s)>0 and s[0] in string.uppercase:
		return True
	else:
		return False

""" 
	Translation Procedure
"""
def translate( inf , of ):
	st = inf.read()
	# tokenizing
	tokens = tokenize(st)
	# parsing and checking
	i = parse( tokens )
	if env.aborted:
		print >> sys.stderr, " Parsing Error "
		exit(-1)
	else:
		# translating
		if env.verbose:
			print "Parsing Successfull"
		
		ofstr=sourcegen.generate_source(env,st, env.system_name )
		
		if env.verbose:
			print "Translation Complete"
		of.write(ofstr)
		of.flush()
		of.close()
		exit(0)

if __name__ == "__main__":
	
	generators = list()
	for dirname, dirnames, filenames in os.walk('.'):
		for filename in filenames:
			if filename.startswith('to') and filename.endswith(".py"):
				generators.append(filename[2:-3])
	
	if len(generators) < 1:
		print "No generators found in generators folder.. this migh be a problem."
		exit(-2)
	
	# Parameters parsing
	parser = OptionParser("usage: "+sys.argv[0]+" [OPTIONS] ")
	parser.add_option("-l", "--language",dest="language", action="store", type="string",
						default=generators[0],help="Generator to use for code generation ["+",".join(generators)+"]", metavar="LANGUAGE GENERATOR")
	parser.add_option("-i", "--infile",dest="infile", action="store", type="string",
						default="-",help="Input File (DEFAULT: stdin)", metavar="INPUT FILE")
	parser.add_option("-o", "--outfile",dest="outfile", action="store", type="string",
						default="-",help="Input File (DEFAULT: stdout)", metavar="OUTPUT FILE")
	parser.add_option("-v", "--verbose",dest="verbose", action="store_true",
						default=False,help="Verbose output", metavar="VERBOSE")
	parser.add_option("-c", "--csv",dest="csv", action="store_true",
						default=False,help="Csv output", metavar="CSVOUTPUT")
	parser.add_option("-g", "--graphics",dest="graphics", action="store_true",
						default=False,help="Graphics output", metavar="GRAPHICS")
	#parser.add_option("-d", "--diffusion",dest="diffusion", action="store_true",
	#					default=False,help="Enable reaction diffusion simulation", metavar="DIFFUSION")
	(options, args) = parser.parse_args()

	env.csv=options.csv
	env.graphics=options.graphics
	env.verbose = options.verbose
	# env.diffusion = options.diffusion
	
	if options.language in generators :
		# pretty bovine
		exec('import to%s as sourcegen' % (options.language))
	else:
		exit(-2)

	if options.infile=="-":
		inf = sys.stdin
	else:
		inf = open(options.infile, "r")
	if options.outfile=="-" : 
		of = sys.stdout
	else:
		#ofname = infilepath[0:-1] + "py"
		env.system_name= os.path.basename( options.outfile ).split(".")[0]
		of = open(options.outfile, "w")
	
	translate( inf, of )
	
