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

#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/29/14        DR  2914      G. Armendariz Remove call to PropertiesFactory
# 

import os
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import HashMap

fxa_data = os.environ.get("FXA_DATA","/data/fxa")

result = HashMap()

try:
    f = open(os.path.join(fxa_data, "tstorm", "fsi-info"), "r")
    try:
        s = f.read()
    finally:
        f.close()
except:
    d = dict()
else:
    d = dict( [ [x[0].strip(), x[1].strip()] for x in
                [ l.split(None, 1) for l in s.split('\n') ]
                if len(x) == 2 ] )

# No exception handle for these because the values are required
if not d.has_key('rssdHost'):
    # FSIprocessorEDEX and rssd most likely run on the same host as EDEX
    import socket
    d['rssdHost'] = socket.gethostname()

if not d.has_key('lbOutputDir'):
    from java.lang import System
    arch_dir = System.getProperty("data.archive.root")
    d['lbOutputDir'] = os.path.join(arch_dir, "radar", "fsi")
    
for k, v in d.items():
    result.put(k, v)

return ResponseMessageGeneric(result)
