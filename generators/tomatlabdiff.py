# -*- coding: utf-8 -*-

__author__ = "Luca Mella"
__version__ = "$Revision: 0.3 $"
__date__ = "$Date: 2013/02/01 $"
__copyright__ = "Copyright (c) 2012-2013 Luca Mella"
__license__ = "CC BY-NC-SA"

import string
import environment

def prepare_eq ( env, mol_reac, mol_prod, reac, prod, rate_list, equation='' ):
	"""
	Insert equation data into a global environment variable
	env	--		the environment data srtucture
	mol_reac --	reagents molecule types
	mol_prod --	products molecule types
	reac --		list of (concentration,molname)
	prod --		list of (concentration,molname)
	rate_list --	list of float
	equation --	string representing the equation
	"""
	env.inc_eq_counter()
	reac_args = list(set(mol_reac + mol_prod)) #Remove repeating elements
	rate_str = "v_" + str(env.eq_counter) + "("
	for term in reac_args:
		rate_str += "Grid(x,y).mols." + str(term) + ", "		
	rate_str = rate_str[0:-2] + ")"
	
	for term in reac:
		env.reac_dict[env.chem_dict[term[1]]] += "-" + str(term[0]) + "*" + rate_str
	for term in prod:
		env.reac_dict[env.chem_dict[term[1]]] += "+" + str(term[0]) + "*" + rate_str
	
	"""
	generate something like
	%--3-->1*X_3.
	function res = v_5( X_3 )
		k5 = 3;
		k5r = 3;
		res = k5 - k5r * X_3^1;
	end
	"""
	# Setting up parameters
	v_str = "%" + equation + "\n"
	v_str += "function res = v_" + str(env.eq_counter) + "( "
	for term in reac_args:
		v_str += term + ", "
	v_str = v_str[0:-2] + " )"
	# Setting up rates
	v_str += "\n	k" + str(env.eq_counter) + " = "+str(rate_list[0])+";"
	"""v_str += "\n	k" + str(env.eq_counter) + "r = "+str(rate_list[0])+";"
	"""
	# Create equation using Law of Mass Action
	v_str += "\n	res = "
	v_str += "k" + str(env.eq_counter) + " * "
	for term in reac:
		v_str += term[1] + "^" + str(term[0]) +  " * "
	v_str = v_str[0:-3]
	"""
	v_str += " - k" + str(env.eq_counter) + "r * "
	for term in prod:
		v_str += term[1] + "^" + str(term[0]) + " * "
	v_str = v_str[0:-3]
	"""
	v_str += ";\nend"
	
	env.reac_list.append(v_str)

def generate_source ( env , eqstr, name="diff_system" ):
	"""
	Generate kinetic system source code
	env	--	the environment data srtucture
	eqstr --	string wich contains the chemical equation system (for comment purposes)
	name --		the name of the system (default "KineticSystem")
	"""
	if name is None:
		name="diff_system"
	"""
	Generate comment section
	"""
	ofstr = "%{\n Reaction ##\n\n"
	ofstr += eqstr + "\n\n"
	ofstr += "## Mapping ##\n\n"
	
	chem_dict_r = {}
	for term in env.chem_dict:
		chem_dict_r.update({env.chem_dict[term] : term})
	
	for term in env.reac_dict:
		ofstr += chem_dict_r[term] + "	" + str(term) + "	" + env.reac_dict[term] + "\n"
	ofstr += "%}\n\n"
	
	# Generating Solver Starter
	ofstr+="function "+str(name)+"(nframes)\n"
	ofstr+="	global TimeDiv\n"
	ofstr+="	global Grid\n"
	ofstr+="	global GridXmin\n"
	ofstr+="	global GridYmin\n"
	ofstr+="	global GridXmax\n"
	ofstr+="	global GridYmax\n"
	ofstr+="	global GridXdiv\n"
	ofstr+="	global GridYdiv\n"
	ofstr+="	global Dx\n"
	ofstr+="	global Dy\n"
	ofstr+="	global DUx\n"
	ofstr+="	global DUy\n"
	ofstr+="	global Dt\n"
	ofstr+="	setupParams;\n"
	ofstr+="	Grid=genGrid;\n"
	ofstr+="	initVal;\n"
	ofstr+="	figure('visible','off');\n"
	ofstr+="	tx=linspace(GridXmin,GridXmax,GridXdiv);\n"
	ofstr+="	ty=linspace(GridYmin,GridYmax,GridYdiv);\n"
	ofstr+="	stp=max([round(TimeDiv/nframes) 1]);\n"
	ofstr+="	opath='"+str(name)+"';\n"
	ofstr+="        mkdir(opath);\n"
	for mol in env.chem_dict.keys():
		ofstr+="	opath='"+str(name)+"/"+str(mol)+"';\n"
		ofstr+="        mkdir(opath);\n"
	ofstr+="	pgrid=zeros(GridXdiv,GridYdiv);\n"
	ofstr+="	for t=0:TimeDiv\n"
	ofstr+="		if (mod(t,stp) == 0)\n"
	for mol in env.chem_dict.keys():
		ofstr+="			for y=1:GridXdiv\n"
		ofstr+="				for x=1:GridYdiv\n"
		ofstr+="					pgrid(x,y)=Grid(x,y).mols."+str(mol)+";\n"
		ofstr+="				end\n"
		ofstr+="			end\n"
		ofstr+="			plot_mesh('"+str(name)+"','"+str(mol)+"',t,t*Dt,tx,ty,pgrid);\n"
	ofstr+="		end\n"
	ofstr+="		stepBoundary(DUx,DUy,Dx,Dy,Dt);\n"
	ofstr+="	end\n"
	ofstr+="end\n"
	
	
	# Setup lattice parameters
	ofstr+="function setupParams\n"
	ofstr+="	global GridXmin\n"
	ofstr+="	global GridYmin\n"
	ofstr+="	global GridXmax\n"
	ofstr+="	global GridYmax\n"
	ofstr+="	global GridXdiv\n"
	ofstr+="	global GridYdiv\n"
	ofstr+="	global GridW\n"
	ofstr+="	global GridH\n"
	ofstr+="	global TimeMin\n"
	ofstr+="	global TimeMax\n"
	ofstr+="	global TimeW\n"
	ofstr+="	global TimeDiv\n"
	ofstr+="	global Dx\n"
	ofstr+="	global Dy\n"
	ofstr+="	global DUx\n"
	ofstr+="	global DUy\n"
	ofstr+="	global Dt\n\n"
	ofstr+="	fprintf 'Diffusion Parameters:\\n'\n"
	ofstr+="	GridXmin = "+str(float(env.gridXmin))+"\n"
	ofstr+="	GridYmin = "+str(float(env.gridYmin))+"\n"
	ofstr+="	GridXmax = "+str(float(env.gridXmax))+"\n"
	ofstr+="	GridYmax = "+str(float(env.gridYmax))+"\n"
	ofstr+="	GridXdiv = "+str(float(env.gridXdiv))+"\n"
	ofstr+="	GridYdiv = "+str(float(env.gridYdiv))+"\n"
	ofstr+="	GridW = GridXmax - GridXmin;\n"
	ofstr+="	GridH = GridYmax - GridYmin;\n"
	ofstr+="	TimeMin = "+str(float(env.tbegin))+"\n"
	ofstr+="	TimeMax = "+str(float(env.tend))+"\n"
	ofstr+="	TimeW=TimeMax-TimeMin;\n"
	ofstr+="	TimeDiv=ceil(TimeW/"+str(env.step)+");\n"
	ofstr+="	% Discretization\n"
	ofstr+="	Dx=GridW/GridXdiv\n"
	ofstr+="	Dy=GridH/GridYdiv\n"
	ofstr+="	% Diffusion\n"
	ofstr+="	DUx="+str(float(env.diffusionX))+"\n"
	ofstr+="	DUy="+str(float(env.diffusionY))+"\n"
	ofstr+="	Dt="+str(env.step)+"\n" #0.2 *( min(Dx,Dy)^2 / (2*min(DUx,DUy))) %TimeW/TimeDiv;\n"
	ofstr+="	fprintf '--------------------\\n'\n"
	ofstr+="end\n\n"
	
	# Generating lattice
	ofstr+="function grid=genGrid\n"
	ofstr+="	global GridXdiv\n"
	ofstr+="	global GridYdiv\n"
	ofstr+="	for y=1:GridXdiv\n"
	ofstr+="		for x=1:GridYdiv\n"
	for mol in env.chem_dict.keys():
		ofstr+="			grid(x,y).mols."+str(mol)+"="+str(env.chem_init_dict[mol])+";\n"
	ofstr+="		end\n"
	ofstr+="	end\n"
	ofstr+="end\n"

	# Generate utility functions
	ofstr+="function [gx,gy]=getCell(rx,ry)\n"
	ofstr+="	global GridW\n"
	ofstr+="	global GridH\n"
	ofstr+="	global GridXmin\n"
	ofstr+="	global GridYmin\n"
	ofstr+="	global GridXdiv\n"
	ofstr+="	global GridYdiv\n"
	ofstr+="	gx=ceil( (rx-GridXmin)/GridW*GridXdiv );\n"
	ofstr+="	gy=ceil( (ry-GridYmin)/GridH*GridYdiv );\n"
	ofstr+="end\n"
	ofstr+="function plot_mesh(sysname,molname,index,time,tx,ty,tz)\n"
	ofstr+="	contourf(tx,ty,tz);\n"
	ofstr+="	title(strcat(sysname,'-',molname,'- t: ',num2str(time)));\n"
	ofstr+="	axis square;\n"
	ofstr+="	colorbar;\n"
	ofstr+="	fname=sprintf(strcat(sysname,'/',molname,'/%04d.jpg'),index);\n"
	ofstr+="	if ( exist('OCTAVE_VERSION') ~= 0 )\n"
	ofstr+="		print(fname,'-djpg');\n"
	ofstr+="	else\n"
	ofstr+="		print(fname,'-djpeg');\n"
	ofstr+="	end\n"
	ofstr+="end\n"
	
	# Generating initial values
	ofstr+="function initVal\n"
	ofstr+="	global Grid\n"
	ofstr+="	global Dx\n"
	ofstr+="	global Dy\n"
	# ugly
	for (mol,val,x0,y0,x1,y1) in env.rectareas:
		ofstr+="	for x="+str(x0)+":Dx:"+str(x1)+"\n"
		ofstr+="		for y="+str(y0)+":Dy:"+str(y1)+"\n"
		ofstr+="			[i,j]=getCell(x,y);\n"
		ofstr+="			Grid(i,j).mols."+str(mol)+"="+str(val)+";\n"
		ofstr+="		end\n"
		ofstr+="	end\n"
	ofstr+="end\n"
	ofstr+="\n"
	# Generating finite diff solver
	ofstr+="function pgrid=stepBoundary(DUx,DUy,Dx,Dy,Dt)\n"
	ofstr+="	global Grid;\n"
	ofstr+="	[rows,cols]=size(Grid);\n"
	ofstr+="	Dx2=Dx^2;\n"
	ofstr+="	Dy2=Dy^2;\n"
	ofstr+="	Dx2Dy2=Dx2*Dy2;\n"
	ofstr+="	DtDUx=Dt*DUx;\n"
	ofstr+="	DtDUy=Dt*DUy;\n"
	ofstr+="	DtDUx_Dx2=DtDUx/Dx2;\n"
	ofstr+="	DtDUy_Dy2=DtDUy/Dy2;\n"
	ofstr+="	DtDUxDUy=DtDUx*DtDUy;\n"
	ofstr+="	DtDUxDUy_Dx2Dy2=DtDUxDUy/Dx2Dy2;\n"
	ofstr+="	C=(1-4*DtDUxDUy_Dx2Dy2);\n"
	ofstr+="	for i=1:cols\n"
	ofstr+="		ip= i+1;\n"
	ofstr+="		im= i-1;\n"
	ofstr+="		for j=1:rows\n"
	ofstr+="			jp=j+1;\n"
	ofstr+="			jm=j-1;\n"
	for mol in env.chem_dict.keys():
		# bovine
		ofstr+="			if (ip <= cols)\n"
		ofstr+="				G_ip_j=Grid(ip,j).mols."+str(mol)+";\n"
		ofstr+="			else\n"
		ofstr+="				G_ip_j=0.0;\n"
		ofstr+="			end\n"
		ofstr+="			if (im > 0)\n"
		ofstr+="				G_im_j=Grid(im,j).mols."+str(mol)+";\n"
		ofstr+="			else\n"
		ofstr+="				G_im_j=0.0;\n"
		ofstr+="			end\n"
		ofstr+="			if (jp <= rows)\n"
		ofstr+="				G_i_jp=Grid(i,jp).mols."+str(mol)+";\n"
		ofstr+="			else\n"
		ofstr+="				G_i_jp=0.0;\n"
		ofstr+="			end\n"
		ofstr+="			if (jm > 0)\n"
		ofstr+="				G_i_jm=Grid(i,jm).mols."+str(mol)+";\n"
		ofstr+="			else\n"
		ofstr+="				G_i_jm=0.0;\n"
		ofstr+="			end\n"
		ofstr+="			Grid(i,j).mols."+str(mol)+"=Dt*rate_eq_"+str(mol)+"(i,j)+DtDUx_Dx2*G_ip_j+DtDUx_Dx2*G_im_j+DtDUy_Dy2*G_i_jp+DtDUy_Dy2*G_i_jm+Grid(i,j).mols."+str(mol)+"*C;\n"
	ofstr+="		end\n"
	ofstr+="	end\n"
	ofstr+="end\n"
	
	# Generating rate_equations
	for mol in env.chem_dict.keys():
		ofstr+="function res=rate_eq_"+str(mol)+"(x,y)\n"
		ofstr+="	global Grid\n"
		ofstr+="	res="+str(env.reac_dict[env.chem_dict[mol]])+";\n"
		ofstr+="end\n"
	
	# Generating coefficient functions
	for term in env.reac_list:
		ofstr += term + "\n"
	
	if env.csv:
		pass
	if env.graphics:
		pass
	return ofstr
