'''
Global variables
'''
class Environment:
	def __init__(self):
		self.tok_lines=list()
		self.chem_dict=dict()		#molecules dictionary { molname : molecule_index }
		self.chem_init_dict=dict()	#initial concentration { molname : initial_concentration }
		self.reac_dict=dict()
		self.reac_list=list()
		self.mol_counter=-1
		self.eq_counter=-1
		self.tbegin=0
		self.tend=10
		self.step=0.01
		self.verbose=False
		self.csv=False
		self.diffusion=False
		self.graphics=False
		self.aborted=False
		self.errormsg="None"
		self.system_name=None
	def set_tbegin(self,value):
		self.tbegin=value
	def set_tend(self,value):
		self.tend=value
	def set_step(self,value):
		self.step=value
	def inc_mol_counter(self):
		self.mol_counter+=1
	def inc_eq_counter(self):
		self.eq_counter+=1
	def set_verbosity(self,verb=True):
		self.verbose=verb
	def abort(self,errorstring=""):
		self.aborted=True
		self.errormsg=errorstring
		self.clear()
	def clear(self):
		self.chem_init_dict=dict()
		self.chem_dict=dict()
		self.reac_dict=dict()
		self.reac_list=list()
		self.mol_counter=-1
		self.eq_counter=-1
