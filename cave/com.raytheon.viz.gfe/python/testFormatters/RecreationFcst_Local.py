##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# 
# RecreationFcst_Local
#  Local customizations for RecreationFcst as Base class
#
# Author: 
# ----------------------------------------------------------------------------

import RecreationFcst
import string, copy

class TextProduct(RecreationFcst.TextProduct):
    Definition = copy.deepcopy(RecreationFcst.TextProduct.Definition)
    Definition['displayName'] = "None"
    Definition['displayName'] = "TEST_RecreationFcst"    
    def __init__(self):
        RecreationFcst.TextProduct.__init__(self)
