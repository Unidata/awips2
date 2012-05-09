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
from serverConfig import * 
import serverConfig 

# redefine the visibilities
serverConfig.visibilities = ['<NoVis>', '0', '1/4', '1/2', '3/4', '1', '11/2',
                '2', '21/2', '3', '4', '5', '6', 'P6']

# add Coverage/Probabilities for SPARSE, modify ISOD
SPARSE = ('Sparse', 'Sparse')
ISOD = ('Isod', 'Isolated')

# define a new set of values for convenience
COV = [ISOD, SCT, NUM, WIDE, SPARSE]


# add new intensity SUPERHEAVY, modify INTEN_MOD
SUPERHEAVY = ('+++', "Super Heavy")
INTEN_MOD = ('mod', 'Moderate')

# define a new set of values for convenience
PCPINTEN = [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY, SUPERHEAVY]

# add a new attribute LOUSY, modify existing FQTLTG
LOUSY = ('Lsy', 'Lousy')
FQTLTG = ('FqLt', 'Frequent Lightning')

# now define all changed weather types, which include any that might
# use the definitions you have changed above in this file
RAIN = ('R', 'Rain', COV, PCPINTEN, [FQTLTG])
SNOW = ('S', 'Snow', [WIDE, SPARSE], [INTEN_LIGHT, INTEN_HEAVY], 
  [LOUSY, OVRPASS])

# put all of the types together. The value must be "types =".
types = [NOWX, RAIN, SNOW]

