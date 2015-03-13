# -*- coding: utf-8 -*-

__author__ = "Luca Mella"
__version__ = "$Revision: 0.3 $"
__date__ = "$Date: 2013/02/01 $"
__copyright__ = "Copyright (c) 2012-2013 Luca Mella"
__license__ = "CC BY-NC-SA"

class Environment:
  """
   Class that represent the information needed for kinetic system generation.
   Once created, it will be filled by the parser and then red from the generator 
  """
  def __init__(self):
    self.tok_lines=list()    # List of line numbers [ 1,2,2,3,3,3,4] , means the fourth token is in line 3 etc..
    self.chem_dict=dict()    # molecules dictionary  {molname:molecule_index}
    self.chem_init_dict=dict()  # initial concentration {molname:initial_concentration}
    self.reac_dict=dict()    # inverse of molecules dictionary {molecule_index:"<rate equation>"}
    self.reac_list=list()    # list of strings that represent the parsed reactions (part of the generated code)
    self.mol_counter=-1      # count the number of distinct molecules of the system
    self.eq_counter=-1      # count the number of equations of the system
    self.tbegin=0        # t0 of the simulation
    self.tend=10        # end time of the simulation
    self.step=0.01        # time step of the simulation
    self.verbose=False      # verbose output
    self.csv=False        # enable csv output
    self.graphics=False      # enable graphical output
    self.aborted=False      # flag that indicate whenever something has gone wrong
    self.errormsg="None"    # error message
    self.system_name=None    # Name of the equation system
    self.diffusion=False    # ?
    self.gridYmin=0        # 
    self.gridXmin=0        # 
    self.gridYmax=5        # 
    self.gridXmax=5        # 
    self.gridXdiv=20      # 
    self.gridYdiv=20      # 
    self.diffusionX=0.1      # 
    self.diffusionY=0.1      # 
    self.rectareas=list()    # list of tuples (<MOLNAME>,<VALUE>,<XORIGIN>,<YORIGIN>,<XEND>,<YEND>)
    self.system_name="chemSystem"  
  def add_rectarea(self,molname,value,xorigin,yorigin,xend,yend):
    """ """
    tupl=(molname,value,xorigin,yorigin,xend,yend)
    if xorigin<self.gridXmin or yorigin<self.gridYmin or \
      xend>self.gridXmax or yend>self.gridYmax:
        abort("area specification outside lattice boundary")
        return
    if not tupl in self.rectareas:
      self.rectareas.append(tupl)
  def set_tbegin(self,value):
    """ """
    self.tbegin=value
  def set_tend(self,value):
    """ """
    self.tend=value
  def set_step(self,value):
    """ """
    self.step=value
  def inc_mol_counter(self):
    """ """
    self.mol_counter+=1
  def inc_eq_counter(self):
    """ """
    self.eq_counter+=1
  def set_verbosity(self,verb=True):
    """ """
    self.verbose=verb
  def abort(self,errorstring=""):
    """ """
    self.aborted=True
    self.errormsg=errorstring
    self.clear()
  def clear(self):
    """ """
    self.chem_init_dict=dict()
    self.chem_dict=dict()
    self.reac_dict=dict()
    self.reac_list=list()
    self.mol_counter=-1
    self.eq_counter=-1
