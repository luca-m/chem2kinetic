
import string
import environment

def prepare_eq ( env, mol_reac, mol_prod, reac, prod, rate_list, equation='' ):
	env.inc_eq_counter()
	
	reac_args = list(set(mol_reac + mol_prod)) #Remove repeating elements
	
	rate_str = "v_" + str(env.eq_counter) + "("
	
	for term in reac_args:
		rate_str += "y(" + str(env.chem_dict[term]+1) + "), "		
	rate_str = rate_str[0:-2] + ")"
	
	for term in reac:
		env.reac_dict[env.chem_dict[term[1]]] += "-" + str(term[0]) + "*" + rate_str
		
	for term in prod:
		env.reac_dict[env.chem_dict[term[1]]] += "+" + str(term[0]) + "*" + rate_str
	
	v_str = "%" + equation + "\n"
	v_str += "function res = v_" + str(env.eq_counter) + "( "
	for term in reac_args:
		v_str += term + ", "
	v_str = v_str[0:-2] + " )"
	v_str += "\n\tk" + str(env.eq_counter) + " = "+str(rate_list[0]/10)+";"
	v_str += "\n\tk" + str(env.eq_counter) + "r = "+str(rate_list[0]/10)+";"
	
	v_str += "\n\tres = "
	v_str += "k" + str(env.eq_counter) + " * "
	
	for term in reac:
		v_str += term[1] + "**" + str(term[0]) +  " * "
	v_str = v_str[0:-3]
		
	v_str += " - k" + str(env.eq_counter) + "r * "
	for term in prod:
		v_str += term[1] + "**" + str(term[0]) + " * "
	v_str = v_str[0:-3]+";"
	v_str += "\nend"
	
	env.reac_list.append(v_str)

def generate_source ( env , eqstr, name="solveKineticSystem" ):
	if name is None:
		name="kineticSystem"
	ofstr = "%{\n Reaction ##\n\n"
	ofstr += eqstr + "\n\n"
	ofstr += "## Mapping ##\n\n"
	
	chem_dict_r = {}
	for term in env.chem_dict:
		chem_dict_r.update({env.chem_dict[term] : term})
	
	for term in env.reac_dict:
		ofstr += chem_dict_r[term] + "\t" + str(term) + "\t" + env.reac_dict[term] + "\n"
	ofstr += "%}\n\n"
	
	# Generating callable function
	ofstr += "function res = "+name+" \n"
	ofstr += "\tif  ( exist('OCTAVE_VERSION') ~= 0 )\n\t\tpkg load odepkg\n\tend\n"
	ofstr += "\t[t,x]=ode45(@gensystem, linspace("+str(env.tbegin)+", "+str(env.tend)+", 1/"+str(env.step)+"),initVal);\n"
	ofstr += "\tres=[t x];\n"
	if env.graphics:
		ofstr += "\tplot_data(res, size(res)(1))\n"
	ofstr += "end\n\n"

	# Generating rate eq system
	ost = "function yprime = gensystem(t,y) \n"
	ost += "\typrime = [ \n"
	for term in env.reac_dict:
		ost += "\t"+env.reac_dict[term] + ";\n"
	ost = ost[0:-2] + " ];\n"
	ost += "end\n\n"

	# Generating initial values
	ofstr += ost
	ofstr += "%Initial concentrations:\n"
	ofstr += "function y0 = initVal \n"
	ofstr += "\ty0=zeros("+str(env.mol_counter+1)+",1);\n"
	for n in range(0, env.mol_counter + 1):
			ofstr += "\ty0("+str(n+1)+")="+str(env.chem_init_dict[chem_dict_r[n]])+"; %"+str(chem_dict_r[n])+"\n"
	ofstr += "\tif ( exist('OCTAVE_VERSION') ~= 0 )\n\t\ty0=y0';\n\tend\n"
	ofstr += "end\n\n"
	
	# Generating coefficient functions
	for term in env.reac_list:
		ofstr += term + "\n\n"
	
	if env.csv:
		#TBD
		pass
	if env.graphics:
		ofstr += "function plot_data(data, maxpoints)\n"
		ofstr += "\tx=data( :,2:end);\n"
		ofstr += "\tt=data( :,1);\n"
		ofstr += "\t[nsample cols] = size(x);\n"
		ofstr += "\tk=ceil(nsample/maxpoints);\n"
		ofstr += "\txx=zeros(maxpoints,cols);\n"
		ofstr += "\tj=1;\n"
		ofstr += "\tfor i=1:nsample\n"
		ofstr += "\t\tif (mod(i,k))==0,\n"
		ofstr += "\t\t\txx(j,:)=x(i,:);\n"
		ofstr += "\t\t\tj=j+1;\n"
		ofstr += "\t\tend\n"
		ofstr += "\tend\n"
		ofstr += "\tplotColor = 'yrgbkmc'; \n"
		ofstr += "\t[p ncolors]=size(plotColor);\n"
		ofstr += "\tfigure;\n"
		ofstr += "\thold on;\n"
		ofstr += "\tfor i=1:cols\n"
		ofstr += "\t\tplot( t,x(:,i),strcat('-',plotColor(mod(i,ncolors)+1),';x',int2str(i),';'));\n"
		ofstr += "\tend\n"
		ofstr += "\tfigure;\n"
		ofstr += "\tmesh(xx);\n"
		ofstr += "end\n"
	return ofstr
