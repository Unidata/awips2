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

##
# This is a base file that is not intended to be overridden.
##

class AllInfo:

    def __init__(self):
        self.data = {}

    def set_info(self, key, value):
        self.data[key] = value

    def get_keys(self):
        return list(self.data.keys())

    def get_values(self):
        return list(self.data.values())

    def get_value(self,key):
        if key in self.data:
            return self.data[key]
        return ''

    def get_items(self):
        return list(self.data.items())

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

    def set_info(self, key, value, missing=""):
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
        return '\0'


class AdditionalData(AllInfo):

    def __init__(self):
        self.data = {}

    def set_info(self,key,value):
        if key not in self.data:
            self.data[key] = []
        if type(value) is str and not value.isalpha():
            if value.isdigit() and len(value) > 1:
                # then it is an integer
                value = int(value)
                # oops we might try putting an integer 'missing value' in a
                # string location
                if key in ['type', 'char_code'] and value == 99999:
                    value = str(value)
            else:
                value = float(value)
        self.data[key].append(value)

    def get_value_array(self,key):
        if key in self.data:
            return self.data[key]
        return []

    def get_value(self, key, idx=0):
        return self.data[key][idx]

    def get_size(self,key):
        if key in self.data:
            return len(self.data[key])
        return 0

    def get_type(self,key):
        try:
            return type(self.data[key][0])
        except:
            if key not in self.data:
                return str

    def fill_missing(self,key,fill_val,fill_size=1):
        if key in self.data:
            # we've already assigned some value(s) to this variable
            if len(self.data[key]) == fill_size:
                return
            for i in range(len(self.data[key]),fill_size):
                self.data[key].append(fill_val)
        else:
            # this is a new variable that needs filled
            self.data[key] = []
            for i in range(fill_size):
                self.data[key].append(fill_val)
