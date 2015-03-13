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
  env  --    the environment data srtucture
  mol_reac --  reagents molecule types
  mol_prod --  products molecule types
  reac --    list of (concentration,molname)
  prod --    list of (concentration,molname)
  rate_list --  list of float
  equation --  string representing the equation
  """
  env.inc_eq_counter()
  
  reac_args = list(set(mol_reac + mol_prod)) #Remove repeating elements
  
  rate_str = "v_" + str(env.eq_counter) + "("
  
  for term in reac_args:
    rate_str += "y[" + str(env.chem_dict[term]) + "], "    
  rate_str = rate_str[0:-2] + ")"

  for term in reac:
    env.reac_dict[env.chem_dict[term[1]]] += " -" + str(term[0]) + "*" + rate_str
    
  for term in prod:
    env.reac_dict[env.chem_dict[term[1]]] += " +" + str(term[0]) + "*" + rate_str

  v_str = "#" + equation+ "\n"
  v_str += "v_" + str(env.eq_counter) + " = lambda "
  for term in reac_args:
    v_str += term + ", "
  v_str = v_str[0:-2] + " : "
  
  v_str += "k" + str(env.eq_counter) + " * "

  for term in reac:
    v_str += term[1] + "**" + str(term[0]) +  " * "
  v_str = v_str[0:-3]
  """  
  v_str += " - k" + str(env.eq_counter) + "r * "
  for term in prod:
    v_str += term[1] + "**" + str(term[0]) + " * "
  v_str = v_str[0:-3]
  """
  v_str += "\nk" + str(env.eq_counter) + " = "+str(rate_list[0])
  v_str += "\nk" + str(env.eq_counter) + "r = "+str(rate_list[0])

  env.reac_list.append(v_str)
  
def generate_source( env , eqstr , name="" ):
  """
  Generate kinetic system source code
  env  --  the environment data srtucture
  eqstr --  string wich contains the chemical equation system (for comment purposes)
  name --    the name of the system (default "KineticSystem")
  """
  ofstr = "#!/usr/bin/python\n\nfrom scipy import *\nimport scipy.integrate as itg\nimport string\n"
  if env.graphics:
    ofstr += "import matplotlib.pyplot as plt\n"
  ofstr += "'''\n## Reaction ##\n\n"
  ofstr += eqstr + "\n\n"
  
  ofstr += "## Mapping ##\n\n"
  
  chem_dict_r = {}
  for term in env.chem_dict:
    chem_dict_r.update({env.chem_dict[term] : term})
  
  for term in env.reac_dict:
    ofstr += chem_dict_r[term] + "  " + str(term) + "  " + env.reac_dict[term] + "\n"
  ofstr += "'''\n\n"
  
  ost = "dy = lambda y, t: array([\\\n"

  for term in env.reac_dict:
    ost += env.reac_dict[term] + ",\\\n"
  ost = ost[0:-3] + "\\\n])"

  ofstr += ost
  ofstr += "\n\n#Initial concentrations:\ny0 = array([\\\n"
  for n in range(0, env.mol_counter + 1):
    if n != env.mol_counter:
      ofstr += "#" + chem_dict_r[n] + "\n"
      ofstr += ""+str(env.chem_init_dict[chem_dict_r[n]])+",\\\n"
    else:
      ofstr += "#" + chem_dict_r[n] + "\n"
      ofstr += ""+str(env.chem_init_dict[chem_dict_r[n]])+",\\\n])\n\n"
  #ofstr = ofstr[0:-3] + "\\\n])"
  
  for term in env.reac_list:
    ofstr += term + "\n\n"
  
  ofstr += "t = arange("+str(env.tbegin)+", "+str(env.tend)+", "+str(env.step)+")\n"
  ofstr += "Y = itg.odeint(dy, y0, t)\n"

  ofstr += 'mols=('
  for n in range(0, env.mol_counter + 1):
    if n != env.mol_counter:
      ofstr += "'"+str(chem_dict_r[n])+"',"
    else:
      ofstr += "'"+str(chem_dict_r[n])+"'"
  ofstr += ")\n"

  if env.csv:
    ofstr += "molecules=list()\n"
    ofstr += "for s in mols:\n"
    ofstr += "  molecules.append(s.split('_'))\n\n"
    ofstr += "for row in range(0, len(Y)):\n"
    ofstr += "  st = \"%f,\"% (t[row])\n"
    ofstr += "  for col in range(0, len(Y[row])):\n"
    ofstr += "    if col+1 < len(Y[row]):\n"
    ofstr += "      st += string.join(molecules[col][1:],',')+\",%f,\"% (Y[row][col])\n"
    ofstr += "    else:\n"
    ofstr += "      st += string.join(molecules[col][1:],',')+\",%f\"% (Y[row][col])\n"
    ofstr += "  print st\n"
  if env.graphics:
    ofstr += "plt.plot(t,Y)\n"
    ofstr += "plt.grid(True)\n"
    ofstr += "plt.ylabel('#molecules')\n"
    ofstr += "plt.xlabel('#time')\n"
    ofstr += "plt.legend( mols, 'upper right', shadow=True, fancybox=True)\n"
    ofstr += "plt.show()\n"
  return ofstr
