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
# Interface for python decoders
#   
#
#    
# SOFTWARE HISTORY
#    
# Date          Ticket#  Engineer    Description
# ------------- -------- ----------- --------------------------
# Sep 22, 2008           njensen     Initial Creation.
# Oct 03, 2013  2402     bsteffen    Make PythonDecoder more extendable.
# Aug 04, 2014  3427     bclement    loadModule() now takes full path to jar
# Nov 21, 2016  5959     njensen     Made pythonic
# Jul  3, 2019  7879     tgurney     Python 3 fixes
#
#

##
# This is a base file that is not intended to be overridden.
##



import zipimport


def loadModule(jarpath, moduleName):

    if moduleName not in sys.modules:
        jar = zipimport.zipimporter(jarpath)
        jar.load_module(moduleName)

def decode(moduleName, **kwargs):
    mod = sys.modules[moduleName]
    # Decoder module should contain a class with the same name as the module.
    decoderCls = getattr(mod, moduleName)
    dec = decoderCls(**kwargs)
    result = dec.decode()
    resultList = []
    if result is not None:
        resultList = [resultDict for resultDict in result]
    return resultList
