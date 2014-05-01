##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
#    Name:
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%
#
import types

class AllInfo:
	
	def __init__(self):
		self.data = {}

	def set_info(self,key,value):
		self.data[key] = value

	def get_keys(self):
		return self.data.keys()

	def get_values(self):
		return self.data.values()

	def get_value(self,key):
		if self.data.has_key(key):
			#return str(self.data[key])
			return self.data[key]
		else:
			return ''

	def get_items(self):
		return self.data.items()

	def get_data(self):
		return self.data

	def get_type(self,key):
		return type(self.data[key])

class StationInfo(AllInfo):

	def __init__(self):
		self.data = {}

class MandatoryData(AllInfo):
	
	def __init__(self):
		self.data = {}

	def set_info(self,key,value,missing=""):
		if value == missing:
			self.data[key] = self.__missing(key)
		else:
		    try:
			self.data[key] = value
		    except:
			self.data[key] = self.__missing(key)

	def __missing(self,key):
		if key in ['wind_dir','wind_spd','cig','vis','temp','dewt','pres']:
			return float(9.99999986991e+14)
		else:
			return '\0'

class AdditionalData(AllInfo):
	
	def __init__(self):
		self.data = {}

	def set_info(self,key,value):
		if not self.data.has_key(key):
			self.data[key] = []
		if (type(value) == types.StringType) and (not value.isalpha()):
			if (value.isdigit() and len(value) > 1): 
				#then it is an integer
				value = int(value)
				#oops we might try putting an integer 'missing value' in a string location
				if key in ['type','char_code'] == 99999: value = str(value)
			else:
				value = float(value)
		self.data[key].append(value)

	def get_value_array(self,key):
		if self.data.has_key(key):
			return self.data[key]
		else:
			return []

	def get_value(self,key,idx=0):
		return self.data[key][idx]

	def get_size(self,key):
		if self.data.has_key(key):
			return len(self.data[key])
		else:
			return 0

	def get_type(self,key):
		try:
			return type(self.data[key][0])
		except:
			if not self.data.has_key(key):
				return type('')	

	def fill_missing(self,key,fill_val,fill_size=1):
		#if fill_size == 1:
			#assign a single missing value
		#	if not self.data.has_key(key):
		#		self.data[key] = [fill_val]
		#	return
		if self.data.has_key(key):
			#we've already assigned some value(s) to this variable
			if len(self.data[key]) == fill_size:
				return
			for i in range(len(self.data[key]),fill_size):
				self.data[key].append(fill_val)	
		else:
			#this is a new variable that needs filled
			self.data[key] = []
			for i in range(fill_size):
				self.data[key].append(fill_val)
