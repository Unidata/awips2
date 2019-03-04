##
##

##
# This is a base file that is not intended to be overridden.
##

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Hazard_HWO_Local
# Produces HWO product.
#
# Author: Matt Davis/ARX
# ----------------------------------------------------------------------------

import Hazard_HWO_<MultiPil>
import TextRules
import string, time, re, os, types, copy

class TextProduct(Hazard_HWO_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_HWO_<MultiPil>.TextProduct.Definition)

    #Definition['displayName'] = None
    Definition['displayName'] = "Hazard_HWO_<MultiPil> (Hazardous Weather Outlook)"

    def __init__(self):
        Hazard_HWO_<MultiPil>.TextProduct.__init__(self)

